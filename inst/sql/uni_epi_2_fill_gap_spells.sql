-- Step 2: Fill gaps between/before/after known episodes with missing_set_to value (v2 secondstep)
-- For each (person, variable):
--   (a) between consecutive non-adjacent episodes: [prev_end+1, next_start-1]
--   (b) before the first episode: [start_study_date, first_start-1]
--   (c) after the last episode: [last_end+1, end_study_date]
-- Gap fills use the variable-specific missing_set_to from study_variables.
-- Input: episodes_raw, study_variables
-- Output: episodes_with_gaps

CREATE OR REPLACE TABLE episodes_with_gaps AS
WITH ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY start_episode, end_episode)      AS rank_asc,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY start_episode DESC, end_episode DESC) AS rank_desc,
    LAG(end_episode) OVER (PARTITION BY person_id, variable_id ORDER BY start_episode, end_episode)    AS prev_end
  FROM episodes_raw
),
-- (a) Gaps between consecutive non-overlapping non-adjacent episodes
--     gap = [prev_episode_end + 1, current_episode_start - 1]
empty_spaces AS (
  SELECT person_id, variable_id,
    prev_end + 1        AS start_episode,
    start_episode - 1         AS end_episode
  FROM ranked
  WHERE prev_end IS NOT NULL
    AND start_episode > prev_end + 1
),
-- (b) Before first episode: [start_study_date, first_episode_start - 1]
--     Only when first episode doesn't already start at study start
before_first AS (
  SELECT person_id, variable_id,
    DATE({start_study_date}) AS start_episode,
    start_episode - 1              AS end_episode
  FROM ranked
  WHERE rank_asc = 1
    AND start_episode > DATE({start_study_date})
),
-- (c) After last episode: [last_episode_end + 1, end_study_date]
after_last AS (
  SELECT person_id, variable_id,
    end_episode + 1                   AS start_episode,
    DATE({end_study_date})     AS end_episode
  FROM ranked
  WHERE rank_desc = 1
)
SELECT person_id, variable_id, value, start_episode, end_episode FROM episodes_raw
UNION ALL
SELECT es.person_id, es.variable_id, sv.missing_set_to AS value, es.start_episode, es.end_episode
FROM empty_spaces es JOIN study_variables sv ON sv.variable_id = es.variable_id
WHERE es.start_episode <= es.end_episode
UNION ALL
SELECT bf.person_id, bf.variable_id, sv.missing_set_to AS value, bf.start_episode, bf.end_episode
FROM before_first bf JOIN study_variables sv ON sv.variable_id = bf.variable_id
WHERE bf.start_episode <= bf.end_episode
UNION ALL
SELECT al.person_id, al.variable_id, sv.missing_set_to AS value, al.start_episode, al.end_episode
FROM after_last al JOIN study_variables sv ON sv.variable_id = al.variable_id
WHERE al.start_episode <= al.end_episode;
