

-- maps page_id to text of the most recently added revision

-- includes a trigger so that whenever an insert happens, the existing text is
-- deleted first

CREATE TABLE text_split_version (
        page_id                 int PRIMARY KEY,
        current_rev_text        text NOT NULL,
        updatedon               timestamp DEFAULT now()
);

-- delete so we don't have to do insert or update stuff
CREATE FUNCTION remove_existing_split_version() RETURNS trigger AS
$remove_existing_split_version$
  BEGIN
    IF NEW.page_id IS NULL THEN -- make sure we have page_id
      RAISE EXCEPTION 'page_id cannot be null';
    END IF;
    -- delete the old info before inserting the new  
    DELETE FROM text_split_version WHERE page_id = NEW.page_id;

    -- finally, to end the trigger behaviour
    RETURN NEW;
  END;
$remove_existing_split_version$ LANGUAGE plpgsql;

-- this is the trigger that calls the above function
CREATE TRIGGER remove_existing_split_version BEFORE INSERT OR UPDATE 
  ON text_split_version
  FOR EACH ROW EXECUTE PROCEDURE remove_existing_split_version();


-- now, a place to store edit lists
CREATE TABLE edit_lists (
        from_revision   int , 
        to_revision     int,
        edit_type       enum ('Ins','Del','Mov'),
        val1            int NOT NULL,
        val2            int NOT NULL,
        val3            int,
        updatedon       timestamp DEFAULT now(),
        PRIMARY KEY (from_revision, to_revision)
);

-- Reputation of a user
-- first, a table with all known user names and ids
CREATE TABLE trust_user_rep(
    user_id     int PRIMARY KEY   ,
    user_text   varchar(255)     ,
    user_rep    float DEFAULT 0.0,
    addedon     timestamp NOT NULL DEFAULT now()
);

-- also have historical records of user're reputation
CREATE TABLE user_rep_history(
        user_id     int NOT NULL,
        rep_before  float NOT NULL,
        rep_after   float NOT NULL,
        change_time float NOT NULL,
        primary key (user_id, change_time)
);

-- place a store colored markup
CREATE TABLE colored_markup (
        revision_id     int PRIMARY KEY,
        revision_text   text NOT NULL
);

-- place to store dead page chunks
CREATE TABLE dead_page_chunks (
  chunk_id       bigserial      PRIMARY KEY,
  chunk_time     timestamp      NOT NULL,
  n_del_revisions       int     NOT NULL
);

-- every word of text added is added only once, here
CREATE TABLE chunk_text (
  text_id       bigserial       PRIMARY KEY,
  text_val      text            NOT NULL
);

-- we will search a lot on text_val
CREATE INDEX chunk_text_text_idx ON chunk_text(text_val);

-- and mapped to all appropriate chunks, here
CREATE TABLE chunk_2_text_map (
  chunk_id      bigint REFERENCES dead_page_chunks ON DELETE CASCADE ON UPDATE
        CASCADE,
  text_id       bigint REFERENCES chunk_text ON DELETE CASCADE ON UPDATE
        CASCADE,
  PRIMARY KEY (chunk_id, text_id)
);

-- this keeps the list of dead chunks together
CREATE TABLE dead_page_chunk_lists (
  page_id       int,
  chunk_id      bigint REFERENCES dead_page_chunks ON DELETE CASCADE ON UPDATE
        CASCADE,
  addedon       timestamp DEFAULT now(),
  PRIMARY KEY (page_id, chunk_id)
);

-- assuming a lot of searches by page id
CREATE INDEX dead_page_chunk_idx ON dead_page_chunk_lists(page_id);

/*
        To insert a chunk list, we first insert all of the text which we need to
        into the chunk_text table, compiling a list of text_id's to use. With
        each one, we insert the entry into the chunks table. -- and also insert
        the corresponding entry into the lists table.
*/
CREATE FUNCTION insert_chunk(page_id_p int, chunk_time_p timestamp, n_del_revisions_p int) RETURNS
    int AS $$
    DECLARE
        next_id RECORD;
    BEGIN
        -- first, get the next id in the chunks sequnce
        SELECT INTO next_id nextval('dead_page_chunks_chunk_id_seq') AS val;

        -- now, do some insertn'
        INSERT INTO dead_page_chunks (chunk_id, chunk_time, n_del_revisions)
        VALUES (next_id.val, chunk_time_p, n_del_revisions_p )
        
        INSERT INTO dead_page_chunk_lists (page_id, chunk_id) VALUES (page_id_p,
        next_id.val)
    END;
$$ LANGUAGE plpgsql;





