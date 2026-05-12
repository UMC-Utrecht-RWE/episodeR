# based on LATEST anchoring function


WITH
    intial_episodes AS (
        SELECT
            MSC.person_id,
            MSC.variable_id,
            concepts.date,
            CAST(
                CASE
                    WHEN COUNT(DISTINCT concepts.value) > 1 THEN 'unknown_edited_1'
                    ELSE MAX(concepts.value)
                END AS TEXT
            ) AS value
        FROM
            concepts_db.concept_table AS concepts
            INNER JOIN lookback_defined MSC ON concepts.concept_id = '/*STARTCHANGEME*/P_GESTDIAB_COV/*ENDCHANGEME*/'
            AND concepts.person_id = MSC.person_id
            AND concepts.date BETWEEN MSC.lookback_start AND MSC.lookback_end
        GROUP BY
            MSC.person_id,
            MSC.variable_id,
            concepts.date
        QUALIFY
            concepts.date = MAX(concepts.date) OVER (
                PARTITION BY
                    MSC.person_id,
                    MSC.pregnancy_id,
                    MSC.anchor_date,
                    MSC.anchor_type,
                    MSC.variable_id
            )
    )
SELECT DISTINCT
    *
FROM
    intial_episodes