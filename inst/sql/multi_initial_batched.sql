-- Batching-only variant of multi_initial.sql, used solely by
-- create_multivariate_episodes()'s batched fallback path (a safety net for
-- cohorts too large to fit in memory in one pass -- not the default code
-- path). Filters the univariate episodes read down to i_batch_persons,
-- written per-batch by the caller, so each batch's working set (dim_var,
-- new_variables_ids) only covers that batch's persons. Kept as a separate
-- file rather than modifying multi_initial.sql so the default single-pass
-- path is completely unaffected.

DROP TABLE IF EXISTS dim_var;

DROP TABLE IF EXISTS new_variables_ids;

CREATE OR REPLACE TABLE initial AS (
    SELECT episodes.*
    FROM read_parquet({d3_univariate_episodes_path}) episodes
    INNER JOIN i_batch_persons ibp
        ON episodes.person_id = ibp.person_id
);

CREATE TABLE dim_var AS (
    SELECT
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
