
BEGIN; 

-- histogram containing the number of users in each rep bucket
CREATE TABLE wikitrust_histogram (
       median			  float NOT NULL,
       rep_0			  float,
       rep_1			  float,
       rep_2			  float,
       rep_3			  float,
       rep_4			  float,
       rep_5			  float,
       rep_6			  float,
       rep_7			  float,
       rep_8			  float,
       rep_9			  float
);
GRANT ALL ON wikitrust_histogram TO wikiuser;

INSERT INTO wikitrust_histogram VALUES (0,0,0,0,0,0,0,0,0,0,0);

-- quality info
CREATE TABLE wikitrust_quality_info (
        revision_id             int PRIMARY KEY,
        qual_info		text NOT NULL
);
GRANT ALL ON wikitrust_quality_info TO wikiuser;

CREATE TABLE wikitrust_edit_lists (
        version         text NOT NULL,
        from_revision   int ,
        to_revision     int,
        edits           longtext,
        PRIMARY KEY (from_revision, to_revision)
);
GRANT ALL ON wikitrust_edit_lists TO wikiuser;

-- Reputation of a user
-- first, a table with all known user names and ids
CREATE TABLE wikitrust_user_rep (
    user_id     int PRIMARY KEY   ,
    user_rep    float DEFAULT 0.0
);
GRANT ALL ON wikitrust_user_rep TO wikiuser;

-- place a store colored markup
CREATE TABLE wikitrust_colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   longtext NOT NULL,
        coloredon       timestamp NOT NULL DEFAULT now()
);
GRANT ALL ON wikitrust_colored_markup TO wikiuser;

CREATE TABLE wikitrust_sigs (
       revision_id      int PRIMARY KEY,
       sigs     	longtext NOT NULL
);
GRANT ALL ON wikitrust_sigs TO wikiuser;

CREATE TABLE wikitrust_dead_page_chunks (
  page_id     int PRIMARY KEY,
  chunks      longtext
);
GRANT ALL ON wikitrust_dead_page_chunks TO wikiuser;

COMMIT;
