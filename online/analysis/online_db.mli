(*

Copyright (c) 2008 The Regents of the University of California
All rights reserved.

Authors: Luca de Alfaro, Ian Pye 

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. The names of the contributors may not be used to endorse or promote
products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

 *)

(** This class provides a handle for accessing the database in the on-line 
    implementation.
    Ian, can you add documentation on what these values are? *)
class db : 
  string -> 
  string ->
  string -> 

  object

    (** [read_edit_diff revid1 revid2] reads from the database the edit list 
	from the (live) text of revision [revid1] to revision [revid2]. 
        The edit list consists in a string, identifying the way revision of 
        Text.ml used for splitting the text, and in the edit list proper.
        The return type is an option: the db should return None if no such row for 
        revid1 and revid2 can be found in the database. *)
    method read_edit_diff : int -> int -> (string * (Editlist.edit list)) option 

    (** [write_edit_diff revid1 revid2 vers elist] writes to the database the edit list 
	[elist] from the (live) text of revision [revid1] to revision [revid2], 
	computed by splitting the text of revision with version [vers] of 
	Text.ml.  Note that for each revid1 and revid2, I want a UNIQUE [vers] and [elist]; 
	previous values should be over-written. *)
    method write_edit_diff : int -> int -> string -> (Editlist.edit list) -> unit

    (** [get_rep uid] gets the reputation of user [uid], from a table 
	relating user ids to their reputation *)
    method get_rep : int -> float

    (** [set_rep uid r] sets, in the table relating user ids to reputations, 
	the reputation of user [uid] to be equal to [r]. *)
    method set_rep : int -> float -> unit

    (** [set_rep_hist uid t r0 r1] writes, in a table with keys user_id, time, 
	and reputation, that at time [t] the reputation of user [uid] went from
	[r0] to [r1]. *)
    method set_rep_hist : int -> float -> float -> float -> unit

    (** [write_colored_markup rev_id markup] writes, in a table with columns by 
	(revision id, string), that the string [markup] is associated with the 
	revision with id [rev_id]. 
	The [markup] represents the main text of the revision, annotated with trust 
	and origin information; it is what the "colored revisions" of our 
	batch demo are. 
	When visitors want the "colored" version of a wiki page, it is this chunk 
	they want to see.  Therefore, it is very important that this chunk is 
	easy and efficient to read.  A filesystem implementation, for small wikis, 
	may be highly advisable. *)
    method write_colored_markup : int -> string -> unit 

    (** [read_colored_markup rev_id] reads the text markup of a revision with id
	[rev_id].  The markup is the text of the revision, annontated with trust
	and origin information. *)
    method read_colored_markup : int -> string

    (** [write_dead_page_chunks page_id chunk_list] writes, in a table indexed by 
	(page id, string list) that the page with id [page_id] is associated 
	with the "dead" strings of text [chunk1], [chunk2], ..., where
	[chunk_list = [chunk1, chunk2, ...] ]. 
	The chunk_list contains text that used to be present in the article, but has 
	been deleted; the database records its existence. *)
    method write_dead_page_chunks : int -> Online_types.chunk_t list -> unit

    (** [read_dead_page_chunks page_id] returns the list of dead chunks associated
	with the page [page_id]. *)
    method read_dead_page_chunks : int -> Online_types.chunk_t list

    (** The database should contain a table about user to user feedback, as follows: 
	(revid1, userid1, revid2, userid2, timestamp, q), where: 
	- revid1, userid1 is the judge revision
	- revid2, userid2 is the judged revision
	- timestamp is the time of the judgement
	- q is the reputation increase quantity (can be negative). 
	- voided is a flag that, if true, indicates that this reputation operation 
	  has been later reverted. 

	Make sure this table is widely indexed, as we may like to use it for data
	analysis.  Note that I can get userid1 and userid2 via joins, but I believe it 
	is more efficient to have such fields already in the table.  Otherwise, we need 
	too many accesses every time a new revision is made. 

	[write_feedback revid1 userid1, revid2, userid2, timestamp, q, voided] adds one such tuple 
	to the db. 

        NOTE: [revid1, revid2] is a key pair to the db, so that if a row with the same
        values for [revid1] and [revid2] exists in the db already, the values are 
        overwritten.  Ian, delete this sentence once you read and implement this. *)
    method write_feedback : int -> int -> int -> int -> float -> float -> bool -> unit

    (** [read_feedback_by revid1] reads from the db all the (revid2, userid2,  timestamp, q, reverted) that 
	have been caused by the revision with id [revid1]. *)
    method read_feedback_by : int -> (int * int * float * float * bool) list

    (** [write_quality_info rev_id n_edit_judges total_edit_quality min_edit_quality
	  n_text_judges new_text persistent_text] writes in a table on disk indexed by [rev_id]
	the tuple (rev_id  n_edit_judges total_edit_quality min_edit_quality
	  n_text_judges new_text persistent_text). *)
    method write_quality_info : int -> int -> float -> float -> int -> int -> int -> unit

    (** [read_quality_info rev_id] returns the tuple 
	(rev_id  n_edit_judges total_edit_quality min_edit_quality
	  n_text_judges new_text persistent_text)
	associated with the revision with id [rev_id]. *)
    method read_quality_info : int -> (int * float * float * int * int * int)

    (** Totally clear out the db structure -- THIS IS INTENDED ONLY FOR UNIT
    TESTING *)
    method delete_all : bool -> unit

  end

