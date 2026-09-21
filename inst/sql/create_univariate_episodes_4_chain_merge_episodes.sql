-- Step 4: Chain-merge same-value overlapping/adjacent intervals (v2 fifthstep)
-- For each (person, variable), collapses contiguous runs of identical value
-- into a single episode, producing the minimal set of maximal same-value intervals.
-- Input: episodes_complete
-- Output: D3_UNIVARIATE_EPISODES
--
-- Runs directly on step 3's output: the former step 4
-- (create_univariate_episodes_4_trim_to_study_period.sql, a clip-to-study-
-- period pass) was removed as a no-op - step 1's `trimmed` CTE already
-- clamps every real episode, and steps 2/3 only ever construct rows from
-- those already-bounded values, so episodes_complete is already within
-- [start_study_date, end_study_date] by the time this step runs. See
-- test-create-univariate-episodes-blackbox.R's bounds-invariant test for
-- the ongoing regression check.
--
-- Uses the same gaps-and-islands window-function merge as uni_epi_1's
-- episodes_raw/univariate_episodes step (see comment there) instead of a self-join + NOT
-- EXISTS, which is O(n^2) per (person_id, variable_id, value) partition.
CREATE OR REPLACE TABLE D3_UNIVARIATE_EPISODES AS
WITH ordered AS (
  SELECT *,
    MAX(end_episode) OVER (
      PARTITION BY person_id, variable_id, value
      ORDER BY start_episode, end_episode
      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
    ) AS running_max_end
  FROM episodes_complete
),
flagged AS (
  SELECT *,
    CASE
      WHEN running_max_end IS NULL OR start_episode > running_max_end + 1
      THEN 1 ELSE 0
    END AS new_island
  FROM ordered
),
grouped AS (
  SELECT *,
    SUM(new_island) OVER (
      PARTITION BY person_id, variable_id, value
      ORDER BY start_episode, end_episode
      ROWS UNBOUNDED PRECEDING
    ) AS island
  FROM flagged
)
SELECT
  person_id,
  variable_id,
  value,
  MIN(start_episode) AS start_episode,
  MAX(end_episode)   AS end_episode
FROM grouped
GROUP BY person_id, variable_id, value, island
ORDER BY person_id, variable_id, start_episode;
