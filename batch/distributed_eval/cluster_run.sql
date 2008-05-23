
DROP TABLE cluster_experiments;
DROP TABLE cluster_files;
DROP TABLE cluster_files_in_exper;
DROP TABLE cluster_simple;

CREATE TABLE cluster_simple (
  file_id serial PRIMARY KEY,
  file_return_dir varchar(1000),
  file_name varchar(1000) NOT NULL UNIQUE ,
  file_status ENUM('unprocessed', 'processing', 'processed') DEFAULT 'unprocessed',
  processedon timestamp
);

CREATE INDEX cluster_simple_idx ON cluster_simple(file_name);

CREATE TABLE cluster_experiments (
  exper_id serial PRIMARY KEY,
  exper_name varchar(256) NOT NULL,
  exper_prefix varchar(256) NOT NULL,
  exper_suffix varchar(256) NOT NULL,
  exper_ending_dir varchar(256) NOT NULL,
  createdon timestamp DEFAULT now()
);

CREATE INDEX cluster_exper_name_idx ON cluster_experiments (exper_name);

CREATE TABLE cluster_files (
  file_id serial PRIMARY KEY,
  file_name varchar(256) NOT NULL,
  createdon timestamp DEFAULT now()
);

CREATE TABLE cluster_files_in_exper (
  exper_id int REFERENCES cluster_experiments,
  file_id int REFERENCES cluster_files,
  file_status ENUM('unprocessed', 'processing', 'processed'),
  processedon timestamp
);

GRANT ALL ON cluster_simple TO wikiuser;
GRANT ALL ON cluster_experiments TO wikiuser;
GRANT ALL ON cluster_files TO wikiuser;
GRANT ALL ON cluster_files_in_exper TO wikiuser;

