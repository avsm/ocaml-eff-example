open Effect
open Effect.Deep

type _ eff += Fork  : (unit -> unit) -> unit eff
type _ eff += Yield : unit eff

let fork f = perform (Fork f)
let yield () = perform Yield

(* A concurrent round-robin scheduler *)
let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    (* Effect handler => instantiates fiber *)
    match f () with
    | () -> dequeue ()
    | exception e ->
        ( print_string (Printexc.to_string e);
          dequeue () )
    | effect Yield, k ->
        ( enqueue k; dequeue () )
    | effect (Fork f), k ->
        ( enqueue k; spawn f )
  in
  spawn main
