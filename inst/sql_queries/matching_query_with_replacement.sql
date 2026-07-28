WITH
    dfexpORD AS (
        -- Here we're going to select all exposed persons for a single year. We'll change this year in the R-script
        SELECT
            *
        FROM
            dfexp
        WHERE
            -- So just a single year
            year_of_birth = 1920
        ORDER BY
            groupkey,
            startdateINT
    ),
    dfunORD AS (
        -- Here we're going to select all unexposed persons for two years to match between. We'll change this year in the R-script
        SELECT
            *
        FROM
            dfun
        WHERE
            -- So two years
            year_of_birth BETWEEN 1919 AND 1921
        ORDER BY
            groupkey,
            startdateINT
    ),
    joined AS (
        -- Join the exposed and the unexposed
        SELECT
            E.person_id AS idexp,
            E.match_id,
            U.person_id AS idun
            -- Here we calculate the lowest absolute difference of the two random numbers
            -- We will select the row with the lowest RandomDiff
,
            ABS(E.random - U.random) AS RandomDiff,
            U.startdateINT AS startdateINT_unexposed,
            U.enddateINT AS enddateINT_unexposed
        FROM
            dfexpORD E
            INNER JOIN dfunORD U
            -- Join (match) the exposed to the unexposed based on the groupkey (profile) and the spell periods (start exposed between start and end unexposed)
            ON E.groupkey = U.groupkey
            -- {{SPELL_OFFSET_CONDITIONS}}
            -- {{DATE_MATCH_CONDITIONS}}
    ),
    least AS (
        -- Here we're going to select the row with the lowest RandomDiff per match_id
        SELECT
            J.match_id,
            MIN(J.RandomDiff) AS MinRandomDiff
        FROM
            joined J
        GROUP BY
            J.match_id
        ORDER BY
            J.match_id
    ),
    matched_persons AS (
        -- So which persons are this?
        SELECT
            J.idexp,
            J.idun,
            J.match_id,
            J.startdateINT_unexposed,
            J.enddateINT_unexposed
        FROM
            joined J
            INNER JOIN least L
            -- Select only the combination with the smallest MinRandomDiff
            ON L.match_id = J.match_id
            AND L.MinRandomDiff = J.RandomDiff
    )
    -- I want to have a resultset with the matched exposed and the unmatched
INSERT INTO
    match_result (
        SELECT
            O.person_id AS idexp,
            O.groupkey,
            O.startdateINT AS startdateINT_exposed,
            O.enddateINT AS enddateINT_exposed,
            M.idun,
            M.startdateINT_unexposed,
            M.enddateINT_unexposed,
            CASE
                WHEN M.idun IS NOT NULL THEN 1
                ELSE 0
            END AS matched_exposed,
            B.boot_id
        FROM
            dfexpORD O
            LEFT JOIN matched_persons M
            -- The line above will change to an inner join on bootstrapping
            -- Add the information on who was matched
            ON M.match_id = O.match_id
            LEFT JOIN dfbootstrap B
            -- Add the bootstrap number
            ON 1 = 1
    )
