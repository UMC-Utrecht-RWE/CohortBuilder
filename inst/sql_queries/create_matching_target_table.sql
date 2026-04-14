-- Here we're going to create (or replace) the table that will hold the birthyear looped matching results
CREATE OR REPLACE TABLE match_result (
    idexp VARCHAR,
    groupkey INTEGER,
    startdateINT_exposed INTEGER,
    enddateINT_exposed INTEGER,
    idun VARCHAR,
    startdateINT_unexposed INTEGER,
    enddateINT_unexposed INTEGER,
    matched_exposed INTEGER,
    boot_id INTEGER
);
