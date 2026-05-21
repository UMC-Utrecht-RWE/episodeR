
DROP TABLE IF EXISTS multivariate_episode;

CREATE TABLE multivariate_episode AS (

WITH COMBINATIONS AS	(
							SELECT 
								-- Here we're going to create a single 'combination' variable for each date for each valid variable
								E.person_id
								, E.dates
								, string_agg(E.int_var_id, ';') AS combination	 
							FROM EXPLODED E
							
							GROUP BY
								E.person_id
								, E.dates 

						)
						
, CHANGES AS			(
							-- Here we're detecting changes in the combination variable
							SELECT
								C.person_id
								, C.dates
								, C.combination
								, LAG(C.combination) OVER (PARTITION BY C.person_id ORDER BY C.dates) AS previous_value
								, CASE	WHEN LAG(C.combination) OVER (PARTITION BY C.person_id ORDER BY C.dates) = C.combination THEN 0
										ELSE 1
										END AS value_changed
								FROM COMBINATIONS C
    
--							WHERE
--								-- Just a nice example
--								C.PERSON_ID = '10000003000'
    
						)

, PERSONSEQ AS 			(
							SELECT
								-- Here we creare a unique id for each timeline we can group by (a cumulative rownumber)
								C.person_id
								, C.dates
								, C.combination
								, SUM(C.value_changed) OVER (PARTITION BY C.person_id ORDER BY C.dates ROWS UNBOUNDED PRECEDING) AS person_group
							FROM CHANGES C
						)

							SELECT
								-- And make a nice begin and end date for each episode
								P.person_id
								, P.combination
								, MIN(P.dates) AS start_episode
								, MAX(P.dates) AS end_episode
							FROM PERSONSEQ p
							
							GROUP BY
								P.person_id
								, P.combination
								, P.person_group
						);

-- Some cleaning up					
DROP TABLE IF EXISTS EXPLODED;
DROP TABLE IF EXISTS int_var_id;
