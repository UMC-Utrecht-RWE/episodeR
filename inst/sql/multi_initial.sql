-- NOTE: EXPLODED is no longer created here. multi_epi_2_combine.sql now
-- builds the combination timeline directly from new_variables_ids using a
-- sweep-line approach, instead of exploding to one row per person/var/day
-- and re-collapsing it. dim_var and new_variables_ids are unchanged below.

DROP TABLE IF EXISTS dim_var;

DROP TABLE IF EXISTS new_variables_ids;

CREATE OR REPLACE TABLE initial AS (
    SELECT *
    FROM read_parquet({d3_univariate_episodes_path}) episodes
    -- INNER JOIN i_batch_persons ibp
    --     ON episodes.person_id = ibp.person_id
    -- INNER JOIN list_sv lsv ON episodes.variable_id = lsv.variable_id
);

CREATE TABLE dim_var AS (
    SELECT
        -- Here we create a lookup table so we can replace the string variable_id with an int
        episodes_filtered.variable_id,
        episodes_filtered.value,
        ROW_NUMBER() OVER (
            ORDER BY
                episodes_filtered.variable_id,
                episodes_filtered.value ASC
        ) AS int_var_id
    FROM
        (
            SELECT
                *
            FROM
                initial 
        ) episodes_filtered
    GROUP BY
        episodes_filtered.variable_id,
        episodes_filtered.value
);

CREATE TABLE new_variables_ids AS (
    SELECT
        episodes_filtered.person_id,
        L.int_var_id,
        episodes_filtered.start_episode,
        episodes_filtered.end_episode
    FROM
        (
            SELECT
                *
            FROM
                initial 
        ) episodes_filtered
        INNER JOIN dim_var L ON L.variable_id = episodes_filtered.variable_id
        AND (
            L.value = episodes_filtered.value
            OR L.value IS NULL
            AND episodes_filtered.value IS NULL
        )
);