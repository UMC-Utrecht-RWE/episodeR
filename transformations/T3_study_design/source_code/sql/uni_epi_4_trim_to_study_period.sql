-- Step 4: Clip spells_complete to [start_study_date, end_study_date] in place (v2 cleanOutsidePeriod)
-- Removes rows that don't overlap the study period at all,
-- then clamps spell_start/spell_end of remaining rows to the study boundaries.
-- Input/output: spells_complete (modified in place)

DELETE FROM spells_complete
WHERE NOT (
  spell_start BETWEEN {start_study_date} AND {end_study_date}
  OR spell_end   BETWEEN {start_study_date} AND {end_study_date}
  OR (spell_start <= {start_study_date} AND spell_end >= {end_study_date})
);

UPDATE spells_complete
SET
  spell_start = CASE WHEN spell_start < {start_study_date} THEN {start_study_date} ELSE spell_start END,
  spell_end   = CASE WHEN spell_end   > {end_study_date}   THEN {end_study_date}   ELSE spell_end   END;
