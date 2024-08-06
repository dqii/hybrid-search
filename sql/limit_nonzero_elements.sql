-- Utility function that accepts an array of numbers and an integer n
-- Keeps the n largest elements in the array and sets the rest to 0
-- This is useful for shrinking the size of a sparse vector
CREATE OR REPLACE FUNCTION limit_nonzero_elements_helper(
    arr ANYARRAY,
    n INTEGER,
    threshold IN ANYELEMENT,
    result IN OUT ANYARRAY
) RETURNS ANYARRAY AS $$
DECLARE
    arr_length INTEGER;
    element_type ALIAS FOR $1;
    count INTEGER;
    idx INTEGER;
BEGIN
    -- If the array is null or empty, return as is
    IF arr IS NULL THEN
        RETURN arr;
    END IF;
    arr_length := array_length(arr, 1);
    IF arr_length IS NULL THEN
        RETURN arr;
    END IF;

    -- If limit is less than or equal to 0, return all zeros
    IF n <= 0 THEN
        result := ARRAY(SELECT 0::element_type FROM generate_series(1, arr_length));
        RETURN result;
    END IF;

    -- Sort the array in descending order and store
    EXECUTE format('SELECT COALESCE((SELECT val FROM unnest($1::%s) val ORDER BY val DESC OFFSET $2 LIMIT 1), 0)', pg_typeof(arr)::text)
    INTO threshold
    USING arr, n-1;

    -- Loop through the array
    -- Count the number of elements that are greater than the threshold
    -- Set the elements that are less than or equal to the threshold to 0
    count := 0;
    FOR idx IN 1..arr_length LOOP
        IF arr[idx] < threshold THEN
            result[idx] := 0;
        ELSE
            count := count + 1;
            IF count > n THEN
                result[idx] := 0;
            END IF;
        END IF;
    END LOOP;

    RETURN result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION limit_nonzero_elements(arr REAL[], n INTEGER) RETURNS REAL[] AS $$
DECLARE
    result REAL[] := arr;
    threshold REAL;
BEGIN
    RETURN limit_nonzero_elements_helper(arr, n, threshold, result)::REAL[];
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION limit_nonzero_elements(arr DOUBLE PRECISION[], n INTEGER) RETURNS DOUBLE PRECISION[] AS $$
DECLARE
    result DOUBLE PRECISION[] := arr;
    threshold DOUBLE PRECISION;
BEGIN
    RETURN limit_nonzero_elements_helper(arr, n, threshold, result)::DOUBLE PRECISION[];
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION limit_nonzero_elements(arr INTEGER[], n INTEGER) RETURNS INTEGER[] AS $$
DECLARE
    result INTEGER[] := arr;
    threshold INTEGER;
BEGIN
    RETURN limit_nonzero_elements_helper(arr, n, threshold, result)::INTEGER[];
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- TODO: Ideally, this should use a different function that is optimized for the sparsevec storage type
CREATE OR REPLACE FUNCTION limit_nonzero_elements(vec SPARSEVEC, n INTEGER) RETURNS SPARSEVEC AS $$
DECLARE
    result REAL[] := vec::REAL[];
    threshold REAL;
BEGIN
    RETURN limit_nonzero_elements_helper(vec::REAL[], n, threshold, result)::SPARSEVEC;
END;
$$ LANGUAGE plpgsql IMMUTABLE;
