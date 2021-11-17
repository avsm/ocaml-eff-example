open Printf

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t * 'a
end

module State (S : sig type t val to_string : t -> string end) : STATE with type t = S.t = struct

  type t = S.t

  effect Put : t -> unit

  let put v =
    Printf.printf ">> put %s\n%!" (S.to_string v);
    perform (Put v)

  effect Get : t

  let get () =
    Printf.printf ">> get ()\n%!";
    let r = perform Get in
    Printf.printf ">> get = %s\n%!" (S.to_string r);
    r

  let run f ~init =
    let comp =
      match (
        printf ">> running f ()\n%!";
        let r = f () in
        printf ">> returning from f ()\n%!";
        r
      )
      with
      | x ->
          printf ">> got return from match\n%!";
          (fun s -> (s, x))
      | effect (Put s') k ->
          printf ">> handling a Put %s effect\n%!" (S.to_string s');
          let r = (fun _s ->
            printf ">> waking up Put with init %s and %s\n%!" (S.to_string _s) (S.to_string s');
            (continue k ()) s') in
          printf ">> returning from Put closure\n%!";
          r
      | effect Get k ->
          printf ">> handling a Get effect\n%!";
          let r = (fun s -> 
            printf ">> waking up Get with %s\n%!" (S.to_string s);
            (continue k s) s) in
          r
    in 
    printf ">> applying comp %s\n%!" (S.to_string init);
    let r = comp init in
    printf ">> comp init returned %s\n%!" (S.to_string (fst r));
    r
end

module IS = State (struct type t = int let to_string = string_of_int end)

let foo () : unit =
  (* printf "%d\n" (IS.get ()); *)
  IS.put 42;
  printf "%d\n" (IS.get ());
  IS.put 21;
  IS.put 22;
  printf "%d\n" (IS.get ())

let _ = IS.run foo ~init:0
