
open Online_db
open Editlist
open Online_types
open Printf
open Std

let test_db dbuser dbpass dbname = (
  let epsilon = 0.001 in (* MYSQL is not all about precics floating point values *)
  let test_page_id = 10 in
  let test_page_text = "this is the page text" in
  let revid1 = 1 in
  let revid2 = 2 in
  let valid = true in
  let userid1 = 11 in
  let userid2 = 22 in
  let time1 = 100.4533 in
  let quality = 10.0 in
  let n_del_revs = 22 in
  let vers = "0.1" in
  let sizeof_arr = 100 in
  let array_init_str i = string_of_int i in
  let array_init_int i = i in
  let array_init_float i = float_of_int i in
  let texta = Array.init sizeof_arr array_init_str in
  let trusta = Array.init sizeof_arr array_init_float in
  let origina = Array.init sizeof_arr array_init_int in
  let rep0 = 0.0 in
  let rep1 = 1.1 in
  let myeditlist = [Editlist.Ins (0, 1) ; Editlist.Del (0, 1); Editlist.Mov (0,
      1, 2)] in
  let testchunk = { timestamp = time1; 
                    n_del_revisions = n_del_revs; 
                    text = texta; 
                    trust = trusta;
                    origin = origina } in
  let mydb = new db dbuser dbpass dbname in
  begin

    (* first, clear everything out *)
    mydb#delete_all true;

    (* test the split text function *)(*
    mydb#write_text_split_version test_page_id test_page_text ;
    let db_text = mydb#read_text_split_version test_page_id in
    assert( db_text = test_page_text ); 
*)
    (* edit dif *)
    mydb#write_edit_diff revid1 revid2 vers myeditlist ;
    let db_editlist = mydb#read_edit_diff revid1 revid2 in
    let num_eds = 0 in
    let p_num_eds = ref num_eds in
    let fel ed = (
      match ed with
      | Ins (i, l) -> p_num_eds := 1 + !p_num_eds;
      | Del (i, l) -> p_num_eds := 1 + !p_num_eds;
      | Mov (i, j, l) -> p_num_eds := 1 + !p_num_eds
    ) in
    match db_editlist with
      | None -> assert(false)
      | Some ((version : string), (edits : Editlist.edit list )) -> List.iter fel edits
      ;
    assert (!p_num_eds >= (List.length myeditlist));
    
    (* user reputation *)
    mydb#set_rep userid1 rep0;
    assert( rep0 = mydb#get_rep userid1 );
      
    (* user rep history *)
    (* Luca -- should there be a get_rep_hist function? *)  
    mydb#set_rep_hist userid1 time1 rep0 rep1;

    (* colored markup *)
    mydb#write_colored_markup revid1 test_page_text;
    assert( test_page_text = mydb#read_colored_markup revid1 );

    (* dead_page_chunks *) 
    mydb#write_dead_page_chunks test_page_id [ testchunk ];
    let f_chk (chk : Online_types.chunk_t ) = (
      assert ((abs_float (chk.timestamp -. testchunk.timestamp)) < epsilon);
      assert (chk.n_del_revisions = testchunk.n_del_revisions);
      assert (chk.text = testchunk.text);
      assert (chk.trust = testchunk.trust);
      assert (chk.origin = testchunk.origin);
    ) in
    let db_chk_list = mydb#read_dead_page_chunks test_page_id in
    List.iter f_chk db_chk_list;

  end;  
) ;;
test_db "wikiuser" "wikiword" "wikitest"
