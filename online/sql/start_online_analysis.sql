BEGIN; 

-- quality info?
CREATE TABLE wikitrust_quality_info (
        rev_id                  int PRIMARY KEY,
        n_edit_judges           int NOT NULL,
        total_edit_quality      float NOT NULL,
        min_edit_quality        float NOT NULL,
        nix_bit                 bool NOT NULL
);
GRANT ALL ON wikitrust_quality_info TO wikiuser;

CREATE TABLE wikitrust_edit_lists (
        version         text NOT NULL,
        from_revision   int ,
        to_revision     int,
        edits           text,
        PRIMARY KEY (from_revision, to_revision)
);
GRANT ALL ON wikitrust_edit_lists TO wikiuser;

-- Reputation of a user
-- first, a table with all known user names and ids
CREATE TABLE wikitrust_trust_user_rep (
    user_id     int PRIMARY KEY   ,
    user_text   varchar(255)     ,
    user_rep    float DEFAULT 0.0,
    addedon     timestamp NOT NULL DEFAULT now()
);
GRANT ALL ON wikitrust_trust_user_rep TO wikiuser;

-- also have historical records of user're reputation
CREATE TABLE wikitrust_user_rep_history (
        user_id     int NOT NULL,
        rep_before  float NOT NULL,
        rep_after   float NOT NULL,
        change_time float NOT NULL,
	event_id    int NOT NULL AUTO_INCREMENT PRIMARY KEY
);
GRANT ALL ON wikitrust_user_rep_history TO wikiuser;

-- place a store colored markup
CREATE TABLE wikitrust_colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   text NOT NULL,
        coloredon       timestamp NOT NULL DEFAULT now()
);
GRANT ALL ON wikitrust_colored_markup TO wikiuser;

CREATE TABLE wikitrust_dead_page_chunks (
  page_id     int PRIMARY KEY,
  chunks      text
);
GRANT ALL ON wikitrust_dead_page_chunks TO wikiuser;

COMMIT;
