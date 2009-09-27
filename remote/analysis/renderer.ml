(**
   Renders html to trust-colored html.
   Duplicates the code in WikiTrustBase.php
*)

let link_regex = Pcre.regexp ~flags:[] "/\\{\\{#t:(\\d+),(\\d+),([^}]+)\\}\\}\\s*\\[\\[([^\\]]+)\\]\\]\\s*(?=\\{\\{#t:|$)/D"
let dt_regex = Pcre.regexp ~flags:[] "/<dt>\\{\\{#t<\\/dt>\n<dd>(.*?)<\\/dd>/"
let edit_section_regex = Pcre.regexp ~flags:[] "/title=\"Edit section: (.*?)\">/"
let trust_tags_regex = Pcre.regexp ~flags:[] "/\\{\\{#t:(\\d+),(\\d+),([^}]+)\\}\\}([^\\{<]++[^<]*?)(?=\\{\\{#t:|<|$)/D"
let dup_remove_regex = Pcre.regexp ~flags:[] "/\\{\\{#t:\\d+,\\d+,[^}]+\\}\\}/"

(** 
    At some point make this work, but for the time being return the empty 
    string. This will make the php layer do the rendering.
*)
let render (raw_html : string) : string = 
  ""
  
  


