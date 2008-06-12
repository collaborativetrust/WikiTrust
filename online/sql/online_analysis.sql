
BEGIN;
DROP TABLE text_split_version;
DROP TABLE edit_lists;
DROP TABLE edit_lists_flat;
DROP TABLE trust_user_rep;
DROP TABLE user_rep_history;
DROP TABLE colored_markup;
DROP TABLE dead_page_chunks;
DROP TABLE chunk_text;
DROP TABLE chunk_trust;
DROP TABLE chunk_origin;
DROP TABLE quality_info;
COMMIT;

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

-- maps page_id to text of the most recently added revision
CREATE TABLE text_split_version (
        page_id                 int PRIMARY KEY,
        current_rev_text        text NOT NULL,
        updatedon               timestamp DEFAULT now()
);

GRANT ALL ON text_split_version TO wikiuser;

-- now, a place to store edit lists
CREATE TABLE edit_lists (
        from_revision   int , 
        to_revision     int,
        edit_type       enum ('Ins','Del','Mov'),
        version         text NOT NULL,
        val1            int NOT NULL,
        val2            int NOT NULL,
        val3            int,
        updatedon       timestamp DEFAULT now(),
        PRIMARY KEY (from_revision, to_revision, edit_type)
);

CREATE INDEX edit_lists_idx ON edit_lists (from_revision, to_revision);
GRANT ALL ON edit_lists TO wikiuser;

-- flattened edit lists
CREATE TABLE edit_lists_flat (
        version         text NOT NULL,
        from_revision   int ,
        to_revision     int,
        edits           text,
        PRIMARY KEY (from_revision, to_revision)
);
GRANT ALL ON edit_lists_flat TO wikiuser;

-- Reputation of a user
-- first, a table with all known user names and ids
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
	event_id    int NOT NULL AUTO_INCREMENT PRIMARY KEY
);
GRANT ALL ON user_rep_history TO wikiuser;

-- place a store colored markup
CREATE TABLE colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   text NOT NULL,
        coloredon       timestamp NOT NULL DEFAULT now()
);
GRANT ALL ON colored_markup TO wikiuser;

CREATE TABLE chunk_text (
        text_id serial PRIMARY KEY,
        chunk_text text NOT NULL
);
GRANT ALL ON chunk_text TO wikiuser;
/* CREATE INDEX chunk_text_idx ON chunk_text(chunk_text, 1000); */

CREATE TABLE chunk_trust (
        trust_id serial PRIMARY KEY,
        chunk_trust float  NOT NULL
);
GRANT ALL ON chunk_trust TO wikiuser;

CREATE TABLE chunk_origin (
        origin_id serial PRIMARY KEY,
        chunk_origin int  NOT NULL
);
GRANT ALL ON chunk_origin TO wikiuser;

CREATE TABLE dead_page_chunks (
  page_id     int PRIMARY KEY,
  chunks      text
);
GRANT ALL ON dead_page_chunks TO wikiuser;





