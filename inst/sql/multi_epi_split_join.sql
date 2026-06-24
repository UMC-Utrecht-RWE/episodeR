CREATE OR REPLACE TABLE i_status_long AS
SELECT
  me.person_id,
  me.start_episode,
  me.end_episode,
  dv.variable_id,
  dv.value
FROM multivariate_episode me,
     UNNEST(string_split(me.combination, ';')) AS u(combination_str)
JOIN dim_var dv
  ON CAST(u.combination_str AS INTEGER) = dv.int_var_id;