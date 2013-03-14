(* Definitions for sets. *)

exception TODO


(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  (* Generates a list containing an ordered sequence of values *)
  let generate_sequence_list (size: int) : elt list =
    let rec gen_seq c size =
      if size <= 0 then []
      else c :: (gen_seq (C.gen_gt(c)()) (size - 1)) in
    gen_seq (C.gen()) size

  (* Insert random values into an initially empty list  *)
  (* and verify that the inserted values are members of *)
  (* resulting set.                                     *)
  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  (* Insert random values into an initially empty list  *)
  (* and then remove them.  Then verify that the        *)
  (* inserted values are no longer members of the       *)
  (* resulting set.                                     *)
  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  (* Generate two random sets.  Take the union and test *)
  (* that the members of the union are members of each  *)
  (* set.  Check that unions with empty are ok.         *)
  let test_union () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let us1s2 = union s1 s2 in
    assert((union empty empty) = empty);
    assert((union s1 empty) = s1);
    List.iter (fun k -> assert(member us1s2 k)) elts1 ;
    List.iter (fun k -> assert(member us1s2 k)) elts2 ;
    ()

  (* Generate two random sets.  Intersect them and test *)
  (* that the members of the result are members of both *)
  (* individual sets.  Check that unions with empty are *)
  (* ok.                                                *)
  let test_intersect () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let is1s1 = intersect s1 s1 in
    List.iter (fun k -> assert(member is1s1 k)) elts1 ;
    let is1empty = intersect s1 empty in
    List.iter (fun k -> assert(not (member is1empty k))) elts1 ;
    let is1s2 = intersect s1 s2 in
    assert(
      fold (fun k a -> member s1 k && member s2 k && a) true is1s2)

  (* Generate two random lists.  Check that they are not    *)
  (* members of the empty set.  Then insert them into a     *)
  (* and confirm they are members.  Then remove and confirm *)
  (* they are not longer members.                           *)
  let test_member () =
    let elts = generate_random_list 100 in
    List.iter (fun k -> assert(not (member empty k))) elts ;
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert((member s1 k))) elts ;
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  (* choose members until the set is empty then report *)
  (* report how many were chosen.                      *)
  let rec choose_till_empty (size: int) (s: set): int =
    match choose s with
    | None -> size
    | Some (k, s) -> choose_till_empty (size + 1) s

  (* choose members until the set is empty then report *)
  (* report how many were chosen.                      *)
  let test_choose () =
    let elts = generate_sequence_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert((choose s1) != None)) elts ;
    assert((choose_till_empty 0 s1) = 100);
    assert((choose empty) = None);
    ()

  let test_fold () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let us1s2 = intersect s1 s2 in
    assert(
      fold (fun k a -> member s1 k && a) true empty);
    assert(
      not(fold (fun k a -> member s1 k && a) false empty));
    assert(
      fold (fun k a -> member s1 k || member s2 k && a) true us1s2)

  let test_is_empty () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    assert(not(is_empty s1));
    assert(is_empty empty);
    ()

  let test_singleton () =
    let elts = generate_random_list 1 in
    let s1 = insert_list empty elts in
    let s2 = singleton (List.hd elts) in
    assert(s1=s2);
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end

(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
  open Order
  type key = C.t
  type value = C.t
  let compare = C.compare
  let string_of_key = C.string_of_t
  let string_of_value = C.string_of_t
  let gen_key = C.gen 
  let gen_key_gt x = C.gen 
  let gen_key_lt x = C.gen 
  let gen_key_between x y () = None 
  let gen_key_random () = C.gen_random ()
  let gen_value () = C.gen_random ()
  let gen_pair () = (gen_key(),gen_value())
  end)
    
  type elt = D.key
  type set = D.dict
  let empty = D.empty
  let insert k d = (D.insert d k k)
  let member = D.member
  let remove k d = (D.remove d k)
  let choose d = 
    match (D.choose d) with
    | Some (k, v, d') -> Some (k, d')
    | None -> None

  let fold f = D.fold (fun k v a -> f k a)

  let singleton x = (insert x empty)

  (* implement the rest of the functions in the signature! *)

  let is_empty d = 
    match choose d with
    | Some (k, d) -> false
    | None -> true

  (* union: we can simply insert the members of one set into the  *
   * other                                                        *)
  let union = fold insert

  (* intersect: lookup all members of one set in the other.       * 
   * Remove any members that don't return a match in the other    *  
   * set                                                          *)
  let intersect s1 s2 =
    (fold 
      (fun e s -> if member s2 e then s else remove e s)
      s1 s1)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* adds a list of keys to the set *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  (* Generates a list containing an ordered sequence of values *)
  let generate_sequence_list (size: int) : elt list =
    let rec gen_seq c size =
      if size <= 0 then []
      else c :: (gen_seq (C.gen_gt(c)()) (size - 1)) in
    gen_seq (C.gen()) size

  (* Insert random values into an initially empty list  *)
  (* and verify that the inserted values are members of *)
  (* resulting set.                                     *)
  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  (* Insert random values into an initially empty list  *)
  (* and then remove them.  Then verify that the        *)
  (* inserted values are no longer members of the       *)
  (* resulting set.                                     *)
  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  (* Generate two random sets.  Take the union and test *)
  (* that the members of the union are members of at    *)
  (* least set.  Check that unions with empty are ok.   *)
  let test_union () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let us1s2 = union s1 s2 in
    assert((union empty empty) = empty);
    assert((union s1 empty) = s1);
    List.iter (fun k -> assert(member us1s2 k)) elts1 ;
    List.iter (fun k -> assert(member us1s2 k)) elts2 ;
    ()

  (* Generate two random sets.  Intersect them and test *)
  (* that the members of the result are members of both *)
  (* individual sets.  Check that unions with empty are *)
  (* ok.                                                *)
  let test_intersect () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let is1s1 = intersect s1 s1 in
    List.iter (fun k -> assert(member is1s1 k)) elts1 ;
    let is1empty = intersect s1 empty in
    List.iter (fun k -> assert(not (member is1empty k))) elts1 ;
    let is1s2 = intersect s1 s2 in
    assert(
      fold (fun k a -> member s1 k && member s2 k && a) true is1s2)

  (* Generate two random lists.  Check that they are not    *)
  (* members of the empty set.  Then insert them into a     *)
  (* and confirm they are members.  Then remove and confirm *)
  (* they are not longer members.                           *)
  let test_member () =
    let elts = generate_random_list 100 in
    List.iter (fun k -> assert(not (member empty k))) elts ;
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert((member s1 k))) elts ;
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  (* choose members until the set is empty then report *)
  (* report how many were chosen.                      *)
  let rec choose_till_empty (size: int) (s: set): int =
    match choose s with
    | None -> size
    | Some (k, s) -> choose_till_empty (size + 1) s

  let test_choose () =
    let elts = generate_sequence_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert((choose s1) != None)) elts ;
    assert((choose_till_empty 0 s1) = 100);
    assert((choose empty) = None);
    ()

  let test_fold () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    let elts2 = generate_random_list 100 in
    let s2    = insert_list empty elts2 in
    let us1s2 = intersect s1 s2 in
    assert(
      fold (fun k a -> member s1 k && a) true empty);
    assert(
      not(fold (fun k a -> member s1 k && a) false empty));
    assert(
      fold (fun k a -> member s1 k || member s2 k && a) true us1s2)


  let test_is_empty () =
    let elts1 = generate_random_list 100 in
    let s1    = insert_list empty elts1 in
    assert(not(is_empty s1));
    assert(is_empty empty);
    ()

  let test_singleton () =
    let elts = generate_random_list 1 in
    let s1 = insert_list empty elts in
    let s2 = singleton (List.hd elts) in
    assert(s1=s2);
    ()

  let print s = 
    let _ = Printf.printf "%s\n" s in
    flush_all();;

  let run_tests () = 
    print "DictSet tests in\n";
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    print "DictSet tests out\n";
    ()

end

(* Create a set of ints using our DictSet functor. *)
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;


(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)
(*
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;
*)


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
  ListSet (C)
  (* DictSet (C) *)

