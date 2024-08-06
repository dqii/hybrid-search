
-- Create access method
CREATE OPERATOR <-> (
  LEFTARG = vector, RIGHTARG = real[], PROCEDURE = l2sq_dist,
  COMMUTATOR = '<->'
);

CREATE OPERATOR <-> (
  LEFTARG = real[], RIGHTARG = vector, PROCEDURE = l2sq_dist,
  COMMUTATOR = '<->'
);

CREATE OPERATOR <=> (
  LEFTARG = vector, RIGHTARG = real[], PROCEDURE = cos_dist,
  COMMUTATOR = '<=>'
);

CREATE OPERATOR <=> (
  LEFTARG = real[], RIGHTARG = vector, PROCEDURE = cos_dist,
  COMMUTATOR = '<=>'
);

CREATE OPERATOR <+> (
  LEFTARG = vector, RIGHTARG = integer[], PROCEDURE = hamming_dist,
  COMMUTATOR = '<+>'
);

CREATE OPERATOR <+> (
  LEFTARG = vector, RIGHTARG = integer[], PROCEDURE = hamming_dist,
  COMMUTATOR = '<+>'
);

CREATE OR REPLACE FUNCTION _lantern_internal.maybe_setup_weighted_vector_search() RETURNS VOID AS
$weighted_vector_search$
DECLARE
  pgvector_exists boolean;
BEGIN
  -- Check if the vector type from pgvector exists
  SELECT EXISTS (
    SELECT 1
    FROM pg_type
    WHERE typname = 'vector'
  ) INTO pgvector_exists;

  IF NOT pgvector_exists THEN
    RAISE NOTICE 'pgvector extension not found. Skipping lantern weighted vector search setup';
    RETURN;
  END IF;

  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_helper(
    table_name regclass,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TEXT AS $$
  DECLARE
    joint_condition text;
    query_base text;
    query_final_where text = '';
    query1 text;
    query2 text;
    query3 text;
    -- variables for weighted columns
    wc1 text = NULL;
    wc2 text = NULL;
    wc3 text = NULL;
    cte_query text;
    maybe_unions_query text;
    final_query text;
    explain_query text;
    explain_output jsonb;
    old_hnsw_ef_search numeric;
    debug_count integer;
    maybe_analyze text = '';
  BEGIN

    -- TODO:: better validate inputs to throw nicer errors in case of wrong input:
    --   1. only allow valid distance_operator strings (<->, <=>, but not abracadabra)
    --   2. only allow valid column names
    --   3. throw an error on negative weights
    --   4. check that id_col column exists before proceeding
    
    IF analyze_output THEN
        maybe_analyze := 'ANALYZE, BUFFERS,';
    END IF;

    -- Joint similarity metric condition
    -- the cast ::vector is necessary for cases when the column is not of type vector
    -- and for some reason in those cases cast does not happen automatically
    wc1 := format('(%s * (%I %s %L))', w1, col1, distance_operator, vec1);
    IF w2 > 0 AND col2 IS NOT NULL AND vec2 IS NOT NULL THEN
        wc2 := format(' (%s * (%I %s %L))', w2, col2, distance_operator, vec2);
    END IF;
    IF w3 > 0 AND col3 IS NOT NULL AND vec3 IS NOT NULL THEN
        wc3 := format(' (%s * (%I %s %L))', w3, col3, distance_operator, vec3);
    END IF;

    joint_condition := wc1 || COALESCE('+' || wc2, '') || COALESCE('+' || wc3, '');

    -- Base query with joint similarity metric
    query_base := format('SELECT * FROM %s ', table_name);
    IF max_dist IS NOT NULL THEN
        query_final_where := format(' WHERE %s < %L', joint_condition, max_dist);
    END IF;

    IF exact THEN
      final_query := query_base || query_final_where || format(' ORDER BY %s', joint_condition);
      IF debug_output THEN
        explain_query := format('EXPLAIN (%s COSTS FALSE, FORMAT JSON) %s', maybe_analyze, final_query);
        EXECUTE explain_query INTO explain_output;

        RAISE WARNING 'Query: %', _lantern_internal.mask_arrays(final_query);

        explain_output := _lantern_internal.mask_order_by_in_plan(explain_output);
        RAISE WARNING 'weighted vector search explain(exact=true): %', jsonb_pretty(explain_output);
      END IF;

      RETURN final_query;
    END IF;

    EXECUTE format('SET LOCAL hnsw.ef_search TO %L', ef);
    -- UNION ALL.. part of the final query that aggregates results from individual vector search queries
    maybe_unions_query := '';

    -- Query 1: Order by first condition's weighted similarity
    query1 := format('%s ORDER BY %I %s %L LIMIT %L', query_base || query_final_where, col1, distance_operator, vec1, ef);

    IF debug_output THEN
      EXECUTE format('SELECT count(*) FROM (%s) t', query1) INTO debug_count;
      RAISE WARNING 'col1 yielded % rows', debug_count;
    END IF;

    cte_query = format('WITH query1 AS (%s) ', query1);

    -- Query 2: Order by other conditions' weighted similarity, if applicable
    IF w2 > 0 AND col2 IS NOT NULL AND vec2 IS NOT NULL THEN
      query2 := format('%s ORDER BY %I %s %L::vector LIMIT %L', query_base || query_final_where, col2, distance_operator, vec2, ef);
      cte_query := cte_query || format(', query2 AS (%s)', query2);
      maybe_unions_query := maybe_unions_query || format(' UNION ALL (SELECT * FROM query2) ');
      IF debug_output THEN
        EXECUTE format('SELECT count(*) FROM (%s) t', query2) INTO debug_count;
        RAISE WARNING 'col2 yielded % rows', debug_count;
      END IF;
    END IF;

    -- Query 3: Order by third condition's weighted similarity, if applicable
    IF w3 > 0 AND col3 IS NOT NULL AND vec3 IS NOT NULL THEN
      query3 := format('%s ORDER BY %I %s %L::vector LIMIT %L', query_base || query_final_where, col3, distance_operator, vec3, ef);
      cte_query := cte_query || format(', query3 AS (%s)', query3);
      maybe_unions_query := maybe_unions_query || format(' UNION ALL (SELECT * FROM query3) ');
      IF debug_output THEN
        EXECUTE format('SELECT count(*) FROM (%s) t', query3) INTO debug_count;
        RAISE WARNING 'col3 yielded % rows', debug_count;
      END IF;
    END IF;

    final_query := cte_query || format($final_cte_query$SELECT * FROM (
      SELECT DISTINCT ON (%I) * FROM (
          (SELECT * FROM query1)
          %s
      ) t
    )
    tt %s ORDER BY %s$final_cte_query$,
    id_col, maybe_unions_query, query_final_where, joint_condition);

    IF debug_output THEN
      explain_query := format('EXPLAIN (%s COSTS FALSE, FORMAT JSON) %s', maybe_analyze, final_query);
      EXECUTE explain_query INTO explain_output;

      RAISE WARNING 'Query: %', _lantern_internal.mask_arrays(final_query);

      explain_output := _lantern_internal.mask_order_by_in_plan(explain_output);
      RAISE WARNING ' weighted vector search explain: %', jsonb_pretty(explain_output);
    END IF;
    
    RETURN final_query;
    
    END
  $$

  -- v v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    -- set l2 (pgvector) and l2sq (lantern) as default, as we do for lantern index.
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    -- N.B. Something seems strange about PL/pgSQL functions that return table with anyelement
    -- when there is single "anylement column" being returned (e.g. returns table ("row" anylement))
    -- then that single "column" is properly spread with source table's column names
    -- but, when returning ("row" anyelement, "anothercol" integer), things fall all over the place
    -- now, the returned table always has 2 columns one row that is a record of sorts, and one "anothercol"
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- s v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- v s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- v v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- s s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- s v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- v s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- s s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    distance_operator text = '<->',
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
    )
    RETURNS TABLE ("row" anyelement) AS
  $$
  DECLARE
    query text;
  BEGIN
    query := lantern.weighted_vector_search_helper(
      pg_typeof(relation_type),
      w1, col1, vec1,
      w2, col2, vec2,
      w3, col3, vec3,
      ef, max_dist,
      distance_operator,
      id_col,
      exact,
      debug_output,
      analyze_output
    );
    RETURN QUERY EXECUTE query;
  END
  $$ LANGUAGE plpgsql;

  -- setup Cosine API shortcuts

  -- v v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- setup L2SQ API shortcuts

  -- v v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_l2sq(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<->', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s v v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s s v
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 vector = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s v s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 vector = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- v s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 vector,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

  -- s s s
  CREATE OR REPLACE FUNCTION lantern.weighted_vector_search_cos(
    relation_type anyelement,
    w1 numeric,
    col1 text,
    vec1 sparsevec,
    w2 numeric= 0,
    col2 text = NULL,
    vec2 sparsevec = NULL,
    w3 numeric = 0,
    col3 text = NULL,
    vec3 sparsevec = NULL,
    ef integer = 100,
    max_dist numeric = NULL,
    id_col text = 'id',
    exact boolean = false,
    debug_output boolean = false,
    analyze_output boolean = false
  ) RETURNS TABLE ("row" anyelement) AS $$
  BEGIN
    RETURN QUERY SELECT * FROM lantern.weighted_vector_search(relation_type, w1, col1, vec1, w2, col2, vec2, w3, col3, vec3, ef, max_dist, '<=>', id_col, exact, debug_output, analyze_output);
  END $$ LANGUAGE plpgsql;

END
$weighted_vector_search$ LANGUAGE plpgsql;

SELECT _lantern_internal.maybe_setup_weighted_vector_search();
DROP FUNCTION _lantern_internal.maybe_setup_weighted_vector_search;