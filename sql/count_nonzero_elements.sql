CREATE OR REPLACE FUNCTION count_nonzero_elements_helper(arr ANYARRAY) RETURNS INTEGER AS $$
DECLARE
    arr_length INTEGER;
    count INTEGER := 0;
BEGIN
    IF arr IS NULL THEN
        RETURN arr;
    END IF;
    arr_length := array_length(arr, 1);
    IF arr_length IS NULL THEN
        RETURN arr;
    END IF;

    FOR idx IN 1..array_length(arr, 1) LOOP
        IF arr[idx] <> 0 THEN
            count := count + 1;
        END IF;
    END LOOP;
    RETURN count;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION count_nonzero_elements(arr REAL[]) RETURNS INTEGER AS $$
BEGIN
    RETURN count_nonzero_elements_helper(arr);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION count_nonzero_elements(arr DOUBLE PRECISION[]) RETURNS INTEGER AS $$
BEGIN
    RETURN count_nonzero_elements_helper(arr);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION count_nonzero_elements(arr INTEGER[]) RETURNS INTEGER AS $$
BEGIN
    RETURN count_nonzero_elements_helper(arr);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- TODO: Ideally, this should use a different function that is optimized for the sparsevec storage type
CREATE OR REPLACE FUNCTION count_nonzero_elements(vec SPARSEVEC) RETURNS INTEGER AS $$
BEGIN
    RETURN count_nonzero_elements_helper(vec::REAL[]);
END;
$$ LANGUAGE plpgsql IMMUTABLE;
