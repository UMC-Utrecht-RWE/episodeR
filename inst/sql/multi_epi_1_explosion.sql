DROP TABLE IF EXISTS EXPLODED;

DROP TABLE IF EXISTS dim_var;

DROP TABLE IF EXISTS new_variables_ids;

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
                read_parquet({d3_univariate_episodes_path}) episodes
                INNER JOIN i_batch_persons ibp
                -- Filtering out the variables for a specific batch of people
                ON episodes.person_id = ibp.person_id
                INNER JOIN list_sv lsv ON episodes.variable_id = lsv.variable_id
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
                read_parquet({d3_univariate_episodes_path}) episodes
                INNER JOIN i_batch_persons ibp
                -- Filtering out the variables for a specific batch of people
                ON episodes.person_id = ibp.person_id
                INNER JOIN list_sv lsv ON episodes.variable_id = lsv.variable_id
        ) episodes_filtered
        INNER JOIN dim_var L ON L.variable_id = episodes_filtered.variable_id
        AND (
            L.value = episodes_filtered.value
            OR L.value IS NULL
            AND episodes_filtered.value IS NULL
        )
);

CREATE TABLE EXPLODED AS (
    SELECT DISTINCT
        -- Explode the table to one row per person per variable per day
        V.person_id,
        V.int_var_id,
        DD.dates
    FROM
        new_variables_ids V
        INNER JOIN dim_date DD
        -- Add the date dimension
        -- This is going to be a huge explosion, but that's what I want
        ON DD.dates BETWEEN V.start_episode AND V.end_episode
    ORDER BY
        V.person_id ASC,
        DD.dates ASC,
        V.int_var_id ASC
);
