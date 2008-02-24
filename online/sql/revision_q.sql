-- Copyright (c) 2007 The Regents of the University of California 
-- All rights reserved.
--
-- Author: Ian Pye
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
--
-- 3. The names of the contributors may not be used to endorse or promote
-- products derived from this software without specific prior written
-- permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

-- tables that contain the revision numbers which need to be processed

CREATE TABLE trust_revision_q(
  revision int PRIMARY KEY,
  status ENUM('added', 'downloading', 'downloaded', 'processing', 'processed') NOT NULL DEFAULT added,
  addedon timestamp NOT NULL DEFAULT now(),
  processedon timestamp
);

CREATE TABLE trust_page(
  page_id                int(8) unsigned     PRIMARY KEY,
  page_namespace         int(11)             ,
  page_title             varchar(255)       ,
  page_restrictions      tinyblob           ,
  page_counter           bigint(20) unsigned ,
  page_is_redirect       tinyint(1) unsigned,
  page_is_new            tinyint(1) unsigned ,
  page_random            double unsigned     ,
  page_touched           timestamp            ,
  page_latest            int(8) unsigned   , 
  page_len               int(8) unsigned
);

CREATE TABLE trust_revision (
  rev_id          int(8) unsigned  PRIMARY KEY,
  rev_page        int(8) unsigned   ,
  rev_text_id     int(8) unsigned    ,
  rev_comment     tinyblob           ,
  rev_user        int(5) unsigned    ,
  rev_user_text   varchar(255)      ,
  rev_timestamp   timestamp           ,
  rev_minor_edit  tinyint(1) unsigned ,
  rev_deleted     tinyint(1) unsigned ,
  rev_len         int(8) unsigned  ,
  rev_parent_id   int(8) unsigned   

);

CREATE TABLE trust_text(
     old_id     int(8) unsigned PRIMARY KEY,
     old_text   mediumblob    ,
     old_flags  tinyblob                   
);

CREATE TABLE trust_users(
  trust_user        int(5) unsigned PRIMARY KEY   ,
  trust_user_text   varchar(255) NOT NULL     ,
  addedon           timestamp NOT NULL DEFAULT now() 
);
