open Printf
open EffectHandlers
open EffectHandlers.Deep

module type STATE = sig
  type t
  val put : t -> unit
  val get : unit -> t
  val run : (unit -> 'a) -> init:t -> t * 'a
end

module State (S : sig type t end) : STATE with type t = S.t = struct

  type t = S.t

  type _ eff += Put : t -> unit eff
  let put v = perform (Put v)

  type _ eff += Get : t eff
  let get () = perform Get

  let run f ~init =
    let comp =
      match_with f ()
      { retc = (fun x -> (fun s -> (s, x)));
        exnc = (fun e -> raise e);
        effc = fun (type a) (e : 'a eff) ->
                 match e with
                 | Put s' -> Some (fun k -> (fun _s -> continue k () s'))
                 | Get -> Some (fun k -> (fun s -> continue k s s))
                 | e -> None
       }
    in comp init
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  printf "%d\n" (IS.get ());
  IS.put 42;
  printf "%d\n" (IS.get ());
  IS.put 21;
  printf "%d\n" (IS.get ());
  SS.put "hello";
  printf "%s\n" (SS.get ());
  SS.put "world";
  printf "%s\n" (SS.get ())

let _ = IS.run (fun () -> SS.run foo ~init:"") ~init:0
