-- Step 4: Clip episodes_complete to [start_study_date, end_study_date] in place (v2 cleanOutsidePeriod)
-- Removes rows that don't overlap the study period at all,
-- then clamps start/end of remaining rows to the study boundaries.
-- Input/output: episodes_complete (modified in place)

DELETE FROM episodes_complete
WHERE NOT (
  start_episode BETWEEN {start_study_date} AND {end_study_date}
  OR end_episode   BETWEEN {start_study_date} AND {end_study_date}
  OR (start_episode <= {start_study_date} AND end_episode >= {end_study_date})
);

UPDATE episodes_complete
SET
  start_episode = CASE WHEN start_episode < {start_study_date} THEN {start_study_date} ELSE start_episode END,
  end_episode   = CASE WHEN end_episode   > {end_study_date}   THEN {end_study_date}   ELSE end_episode   END;
