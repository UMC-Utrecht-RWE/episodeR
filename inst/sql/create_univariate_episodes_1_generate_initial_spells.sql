-- Step 1: Generate most-recent-record-resolved, trimmed, chain-merged episodes 
--   (replicates v2 firststep_v2.sql)
-- Input: D3_CONCEPTS (view on concepts_db), study_variables, all_persons
-- Output: episodes_raw (person_id, variable_id, value, start_episode, end_episode)
--
-- Pipeline inside this step (mirrors v2 firststep CTEs):
--   concept_dedup      -> deduplicate per (person, concept, date)
--   initial_episodes   -> initial episodes: date + end_look_back / date + start_look_back
--   ranked_dates       -> sort by start_episode per (person, variable)
--   episode_boundaries -> LEFT JOIN next row to get new_end
--   adjusted           -> most-recent-record resolution: crop end_episode to next_start - 1
--   trimmed            -> clamp to [start_study_date, end_study_date] + filter degenerate
--   episodes_raw       -> chain-merge same-value adjacent/overlapping trimmed intervals

-- Filtering to concept_id_list here (rather than only later in initial_episodes)
-- shrinks the dedup GROUP BY to just the concept_ids this call actually
-- needs. Safe because the GROUP BY is per concept_id already, so rows for
-- other concept_ids can never affect a kept concept_id's dedup result.
-- This call reruns per pipeline invocation (once for non-batched
-- variables, once for batched, and once per person-batch when batching is
-- active), so trimming its input matters when D3_CONCEPTS holds many
-- concept types but only a few are relevant to a given call.
CREATE OR REPLACE TABLE concept_dedup AS
WITH concepts_dated AS (
  SELECT
    c.person_id,
    c.concept_id,
    CAST(c.date AS DATE) AS date,
    c.value
  FROM D3_CONCEPTS c
  WHERE c.concept_id IN ({concept_id_list})
)
SELECT
  c.person_id,
  c.concept_id,
  c.date,
  CASE
    WHEN COUNT(DISTINCT c.value) > 1 THEN 'episodeR_conflict_values_on_date'
    ELSE MAX(c.value)
  END AS value
FROM concepts_dated c
INNER JOIN all_persons p ON c.person_id = p.person_id
GROUP BY c.person_id, c.concept_id, c.date;

CREATE OR REPLACE TABLE trimmed_episodes AS
WITH
initial_episodes AS (
  SELECT DISTINCT
    c.person_id,
    sv.variable_id,
    c.value,
    c.date + CAST(sv.end_look_back   AS INTEGER) AS start_episode,
    c.date + CAST(sv.start_look_back AS INTEGER) AS end_episode
  FROM concept_dedup c
  JOIN study_variables sv ON c.concept_id = sv.concept_id
),
ranked_dates AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY start_episode) AS rn
  FROM initial_episodes
),
episode_boundaries AS (
  SELECT
    rd1.person_id,
    rd1.variable_id,
    rd1.value,
    rd1.start_episode,
    rd1.end_episode,
    rd2.start_episode AS new_end
  FROM ranked_dates rd1
  LEFT JOIN ranked_dates rd2
    ON rd1.person_id = rd2.person_id
    AND rd1.variable_id = rd2.variable_id
    AND rd1.rn + 1 = rd2.rn
),
-- Most-recent-record resolution: crop end to next record's start - 1
adjusted AS (
  SELECT
    person_id,
    variable_id,
    value,
    start_episode,
    CASE
      WHEN new_end IS NOT NULL
        AND new_end <= end_episode
        AND NOT start_episode = end_episode
      THEN new_end - 1
      ELSE end_episode
    END AS end_episode
  FROM episode_boundaries
),
-- Clamp to [start_study_date, end_study_date]; filter out episodes that don't overlap study period
trimmed AS (
  SELECT
    a.person_id,
    a.variable_id,
    a.value,
    CASE WHEN a.start_episode < DATE({start_study_date}) THEN DATE({start_study_date}) ELSE a.start_episode END AS start_episode,
    CASE WHEN a.end_episode   > DATE({end_study_date})   THEN DATE({end_study_date})   ELSE a.end_episode   END AS end_episode
  FROM adjusted a
  WHERE
    (a.start_episode BETWEEN {start_study_date} AND {end_study_date})
    OR (a.end_episode   BETWEEN {start_study_date} AND {end_study_date})
    OR (a.start_episode < {start_study_date} AND a.end_episode > {end_study_date})
)
SELECT person_id, variable_id, value, start_episode, end_episode FROM trimmed;

-- Chain-merge same-value overlapping/adjacent intervals within
-- (person_id, variable_id, value) using a gaps-and-islands window-function
-- merge instead of a self-join + NOT EXISTS (which is O(n^2) per
-- partition, the same class of query multi_epi_3_mergestatus.sql's
-- sweep-line rewrite replaced on the multivariate side).
-- running_max_end tracks the widest end_episode seen so far in start order,
-- so a nested interval (e.g. [1,10] followed by [2,3]) still merges
-- correctly instead of only comparing against the immediately preceding
-- row. A new island starts whenever the current start is more than 1 day
-- past that running max; PARTITION BY value groups NULL values together
-- natively, so no extra NULL-equality handling is needed.
CREATE OR REPLACE TABLE episodes_raw AS
WITH ordered AS (
  SELECT *,
    MAX(end_episode) OVER (
      PARTITION BY person_id, variable_id, value
      ORDER BY start_episode, end_episode
      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
    ) AS running_max_end
  FROM trimmed_episodes
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
GROUP BY person_id, variable_id, value, island;
