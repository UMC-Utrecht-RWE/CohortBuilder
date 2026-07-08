WITH
    exposed_ranked AS (
        SELECT
            P.spell_id AS exp_spell_id,
            P.person_id AS exp_person_id,
            P.groupkey,
            P.year_of_birth AS exp_year_of_birth,
            P.startdateINT AS exp_startdateINT,
            P.enddateINT AS exp_enddateINT,
            ROW_NUMBER() OVER (
                ORDER BY
                    P.startdateINT,
                    P.groupkey,
                    hash(P.person_id, P.startdateINT, __START_SEED__),
                    P.spell_id
            ) AS exposed_priority
        FROM
            pool_nr P
            INNER JOIN person_state_nr S ON S.person_id = P.person_id
        WHERE
            S.available = TRUE
            AND P.group = 'exposed'
    ),
    exposed_available AS (
        SELECT
            *
        FROM
            exposed_ranked
        WHERE
            exposed_priority BETWEEN __EXPOSED_PRIORITY_MIN__ AND __EXPOSED_PRIORITY_MAX__
    ),
    control_available AS (
        SELECT
            P.spell_id AS ctrl_spell_id,
            P.person_id AS ctrl_person_id,
            P.groupkey,
            P.year_of_birth AS ctrl_year_of_birth,
            P.startdateINT AS ctrl_startdateINT,
            P.enddateINT AS ctrl_enddateINT
        FROM
            pool_nr P
            INNER JOIN person_state_nr S ON S.person_id = P.person_id
        WHERE
            S.available = TRUE
            AND P.group = 'control'
            AND mod(
                hash(P.spell_id, __START_SEED__),
                __CONTROL_BUCKET_COUNT__
            ) = __CONTROL_BUCKET_ID__
    ),
    candidate_pairs AS (
        SELECT
            E.exp_spell_id,
            E.exp_person_id,
            E.groupkey,
            E.exp_year_of_birth,
            E.exp_startdateINT,
            E.exp_enddateINT,
            E.exposed_priority,
            C.ctrl_spell_id,
            C.ctrl_person_id,
            C.ctrl_startdateINT,
            C.ctrl_enddateINT,
            hash(E.exp_spell_id, C.ctrl_spell_id, __START_SEED__) AS candidate_order_key,
            ROW_NUMBER() OVER (
                PARTITION BY
                    E.exp_spell_id
                ORDER BY
                    candidate_order_key,
                    C.ctrl_spell_id
            ) AS candidate_rank
        FROM
            exposed_available E
            INNER JOIN control_available C ON C.groupkey = E.groupkey
            AND C.ctrl_year_of_birth BETWEEN E.exp_year_of_birth - 1 AND E.exp_year_of_birth  + 1
            AND E.exp_startdateINT BETWEEN C.ctrl_startdateINT AND C.ctrl_enddateINT
            AND E.exp_person_id <> C.ctrl_person_id
    ),
    proposals AS (
        SELECT
            *
        FROM
            candidate_pairs
        WHERE
            candidate_rank = 1
    )
SELECT
    exp_spell_id,
    exp_person_id,
    groupkey,
    exp_year_of_birth,
    exp_startdateINT,
    exp_enddateINT,
    exposed_priority,
    ctrl_spell_id,
    ctrl_person_id,
    ctrl_startdateINT,
    ctrl_enddateINT,
    candidate_order_key
FROM
    proposals
ORDER BY
    exposed_priority;
