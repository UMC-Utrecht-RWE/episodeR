-- Lookup table: replace string variable_id+value with a compact int surrogate key.
-- Only built from variables belonging to people in the current batch.
CREATE OR REPLACE TABLE dim_var AS (
    SELECT
        episodes_filtered.variable_id,
        episodes_filtered.value,
        ROW_NUMBER() OVER (ORDER BY episodes_filtered.variable_id, episodes_filtered.value ASC) AS int_var_id
    FROM (
        SELECT *
        FROM read_parquet({d3_univariate_episodes_path}) episodes
        INNER JOIN i_batch_persons ibp
            ON episodes.person_id = ibp.person_id
    ) episodes_filtered
    GROUP BY
        episodes_filtered.variable_id,
        episodes_filtered.value
);

-- Episode table with int_var_id substituted in, still at one-row-per-episode grain
CREATE OR REPLACE TABLE new_variables_ids AS (
    SELECT
        episodes_filtered.person_id,
        L.int_var_id,
        episodes_filtered.start_episode,
        episodes_filtered.end_episode
    FROM (
        SELECT *
        FROM read_parquet({d3_univariate_episodes_path}) episodes
        INNER JOIN i_batch_persons ibp
            ON episodes.person_id = ibp.person_id
    ) episodes_filtered
    INNER JOIN dim_var L
        ON L.variable_id = episodes_filtered.variable_id
        AND (L.value = episodes_filtered.value OR (L.value IS NULL AND episodes_filtered.value IS NULL))
);

-- Explode each episode into one row per day directly from its own start/end —
-- no dim_date table and no BETWEEN join needed
CREATE OR REPLACE TABLE EXPLODED AS (
    SELECT DISTINCT
        V.person_id,
        V.int_var_id,
        UNNEST(GENERATE_SERIES(
            V.start_episode::DATE,
            V.end_episode::DATE,
            INTERVAL '1 day'
        )) AS dates

    FROM new_variables_ids V

    ORDER BY
        V.person_id ASC,
        dates ASC,
        V.int_var_id ASC
);