-- Step 3: Add full-period episodes for persons absent from all concept data (v2 thirdstep)
-- Any (person, variable) pair not present in episodes_with_gaps gets a single row
-- spanning [start_study_date, end_study_date] with missing_set_to as value.
-- Input: episodes_with_gaps, all_persons, list_sv, study_variables
-- Output: episodes_complete

CREATE OR REPLACE TABLE episodes_complete AS
WITH existing AS (
  SELECT DISTINCT person_id, variable_id FROM episodes_with_gaps
),
missing_pairs AS (
  SELECT a.person_id, l.variable_id
  FROM all_persons a
  CROSS JOIN list_sv l
  LEFT JOIN existing e
    ON a.person_id = e.person_id AND l.variable_id = e.variable_id
  WHERE e.person_id IS NULL
)
SELECT person_id, variable_id, value, start_episode, end_episode FROM episodes_with_gaps
UNION ALL
SELECT
  mp.person_id,
  mp.variable_id,
  sv.missing_set_to                AS value,
  DATE({start_study_date})         AS start_episode,
  DATE({end_study_date})           AS end_episode
FROM missing_pairs mp
JOIN study_variables sv ON sv.variable_id = mp.variable_id;
