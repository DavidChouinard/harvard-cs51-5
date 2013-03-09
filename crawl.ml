open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*  
   = QuantumRanker (PageGraph) (PageScore) (struct 
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  let rec add_words_to_index (d: WordDict.dict) (url: link) (words: string list)
      : WordDict.dict =
    match words with
    | [] -> d
    (*TODO: Union instead*)
    | hd::tl -> add_words_to_index (WordDict.insert d hd (LinkSet.insert url
        LinkSet.empty)) url tl
  in
  if n <= 1 then WordDict.empty else
  match LinkSet.choose frontier with
  | None -> WordDict.empty (* Set is empty, our work here is done *)
  | Some (url, frontier) ->
      match CrawlerServices.get_page url with
      | None -> WordDict.empty (* Set is empty, our work here is done *)
      | Some {url = _; links = outgoing; words = words} ->
          (*let new_frontier = LinkSet.union outgoing frontier in*)
          let new_visited = LinkSet.insert url visited in
          add_words_to_index d url words

      (*let old_set =*)
      (*match WordDict.lookup d "test" with*)
      (*| None -> LinkSet.empty*)
      (*| Some s -> s*)
      (*in*)
      (*WordDict.insert d "test" (LinkSet.insert url old_set)*)
      (*num_pages_to_search (n-1)*)
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
