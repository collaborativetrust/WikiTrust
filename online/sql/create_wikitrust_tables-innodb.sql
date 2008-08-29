
BEGIN; 

-- histogram containing the number of users in each rep bucket
CREATE TABLE wikitrust_global (
       median		          float,
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
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_global TO wikiuser;

INSERT INTO wikitrust_global VALUES (0,0,0,0,0,0,0,0,0,0,0);

CREATE TABLE wikitrust_page (
       page_id             int PRIMARY KEY,
       deleted_chunks      longtext,
       page_info	   text NOT NULL
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_page TO wikiuser;

CREATE TABLE wikitrust_revision (
        revision_id             int PRIMARY KEY,
        quality_info		text NOT NULL, 
	reputation_delta        float DEFAULT 0.0,
	overall_trust           float DEFAULT 0.0
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_revision TO wikiuser;

CREATE TABLE wikitrust_colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   longtext NOT NULL,
	revision_createdon varchar(32) NOT NULL
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_colored_markup TO wikiuser;

CREATE TABLE wikitrust_sigs (
       revision_id      int PRIMARY KEY,
       words		longtext NOT NULL,
       trust            longtext NOT NULL,
       origin           longtext NOT NULL,
       sigs     	longtext NOT NULL
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_sigs TO wikiuser;

CREATE TABLE wikitrust_user (
       user_id     int PRIMARY KEY   ,
       user_rep    float DEFAULT 0.0
) ENGINE=InnoDB;
GRANT ALL ON wikitrust_user TO wikiuser;

COMMIT;
