-- Step 5: Chain-merge same-value overlapping/adjacent intervals (v2 fifthstep)
-- For each (person, variable), collapses contiguous runs of identical value
-- into a single episode, producing the minimal set of maximal same-value intervals.
-- Input: episodes_complete
-- Output: D3_UNIVARIATE_EPISODES

-- Uses the same gaps-and-islands window-function merge as uni_epi_1's
-- episodes_raw step (see comment there) instead of a self-join + NOT
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
