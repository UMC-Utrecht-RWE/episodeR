DROP TABLE IF EXISTS EXPLODED;
DROP TABLE IF EXISTS dim_var;
DROP TABLE IF EXISTS new_variables_ids;

CREATE TABLE dim_var AS (
    SELECT 
        -- Here we create a lookup table so we can replace the string variable_id with an int
        MS_FILTERED.variable_id,
        MS_FILTERED.value,
        ROW_NUMBER() OVER (ORDER BY MS_FILTERED.variable_id, MS_FILTERED.value ASC) AS int_var_id
    FROM (SELECT *
						FROM 	read_parquet({d3_univariate_episodes_path}) MS
						INNER JOIN i_batch_persons ibp
						 -- Filtering out the variables for a specific batch of people
							ON MS.person_id = ibp.person_id) MS_FILTERED
      								
    GROUP BY
        MS_FILTERED.variable_id,
        MS_FILTERED.value
);

CREATE TABLE new_variables_ids AS (
    SELECT
        MS_FILTERED.person_id,
        L.int_var_id,
        MS_FILTERED.variable_start_spell,
        MS_FILTERED.variable_end_spell
    FROM (SELECT *
						FROM 	read_parquet({d3_univariate_episodes_path}) MS
						INNER JOIN i_batch_persons ibp
						 -- Filtering out the variables for a specific batch of people
							ON MS.person_id = ibp.person_id) MS_FILTERED
    INNER JOIN dim_var L
        ON L.variable_id = MS_FILTERED.variable_id 
        AND (L.value = MS_FILTERED.value OR L.value IS NULL AND MS_FILTERED.value IS NULL)
);

CREATE TABLE EXPLODED AS 	(
								SELECT DISTINCT
									-- Explode the table to one row per person per variable per day
									V.person_id
									, V.int_var_id									
									, DD.dates
								FROM new_variables_ids V
								
								INNER JOIN dim_date DD
									-- Add the date dimension
									-- This is going to be a huge explosion, but that's what I want
									ON DD.dates BETWEEN V.variable_start_spell AND V.variable_end_spell 
									
								ORDER BY
									V.person_id ASC
									, DD.dates ASC
									, V.int_var_id ASC
							);
