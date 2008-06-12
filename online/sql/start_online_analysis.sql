BEGIN;
-- quality info?
CREATE TABLE quality_info (
        rev_id                  int PRIMARY KEY,
        n_edit_judges           int NOT NULL,
        total_edit_quality      float NOT NULL,
        min_edit_quality        float NOT NULL,
        nix_bit                 bool NOT NULL
);
GRANT ALL ON quality_info TO wikiuser;

CREATE TABLE edit_lists (
        version         text NOT NULL,
        from_revision   int ,
        to_revision     int,
        edits           text,
        PRIMARY KEY (from_revision, to_revision)
);
GRANT ALL ON edit_lists_flat TO wikiuser;

CREATE TABLE trust_user_rep(
    user_id     int PRIMARY KEY   ,
    user_text   varchar(255)     ,
    user_rep    float DEFAULT 0.0,
    addedon     timestamp NOT NULL DEFAULT now()
);
GRANT ALL ON trust_user_rep TO wikiuser;

-- also have historical records of user're reputation
CREATE TABLE user_rep_history(
        user_id     int NOT NULL,
        rep_before  float NOT NULL,
        rep_after   float NOT NULL,
        change_time float NOT NULL,
        primary key (user_id, change_time)
);
GRANT ALL ON user_rep_history TO wikiuser;

-- place a store colored markup
CREATE TABLE colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   text NOT NULL
);
GRANT ALL ON colored_markup TO wikiuser;

CREATE TABLE dead_page_chunks (
  page_id     int PRIMARY KEY,
  chunks      text
);
GRANT ALL ON dead_page_chunks TO wikiuser;
COMMIT;
