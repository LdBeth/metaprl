(*
 * Interface to ensemble.
 *)

open Nl_debug

open Hsys
open Ensemble
open Ensemble.Util
open Ensemble.View
open Ensemble.Appl_handle

open Printf

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This is the info we keep abou the local cd.
 *   nl_state: view state of this application
 *   nl_local: view local of this application
 *   nl_hbeat: function to call ot get immediate heartbeat
 *)
type cdrom_info =
   { (* Ensemble info *)
     nl_heartbeat_rate : Time.t;
     mutable nl_next_heartbeat : Time.t;
     mutable nl_state : View.state;
     mutable nl_local : View.local;
     mutable nl_hbeat : unit -> unit;
   }

(************************************************************************
 * APPLICATION                                                          *
 ************************************************************************)

(*
 * Debug variables.
 *)
let debug_nlapp =
   create_debug (**)
      { debug_name = "nlapp";
        debug_description = "Display NL Ensemble Application actions";
        debug_value = false
      }

(*
 * Create the initial state for the application.
 *)
let open_nl (ls, vs) =
   { nl_state = vs;
     nl_local = ls;
     nl_hbeat = Appl.async ls.async;
     nl_heartbeat_rate = Time.of_int 10;
     nl_next_heartbeat = Time.zero
   }

(************************************************************************
 * ENSEMBLE INTERFACE                                                   *
 ************************************************************************)

(*
 * Handle a received message.
 *)
let recv_cast info srchand (msg, vecs) =
   [||]

let recv_send info srchand (msg, vecs) =
   [||]

let receive info srchand blockp castp msg =
   match castp with
      New.C ->
         recv_cast info srchand msg
    | New.S ->
         recv_send info srchand msg

(*
 * The heartbeat sends all the messages in the queue.
 *)
let heartbeat info now =
   if !debug_nlapp then
      eprintf "Nlappdapp.heartbeat%t" eflush;
   [||]

(*
 * View changes.
 * We don't do much except recompute the ranks of the players.
 *)
let block info () =
   if !debug_nlapp then
      eprintf "Nlappdapp.block%t" eflush;
   [||]

let disable info () =
   if !debug_nlapp then
      eprintf "Nlappdapp.disable%t" eflush

let make_handlers info =
   { New.block               = block info;
     New.heartbeat           = heartbeat info;
     New.receive             = receive info;
     New.disable             = disable info
   }

let install info (ls, vs) handles =
   if !debug_nlapp then
      eprintf "Nlapp.install%t" eflush;
   info.nl_state <- vs;
   info.nl_local <- ls;
   [||], make_handlers info

let exit info () =
   if !debug_nlapp then
      eprintf "Nlapp.exit%t" eflush;
   ()

(*
 * This function returns the interface record that defines
 * the callbacks for the application.  This is derived from
 * the mtalk demo code.
 *)
let make_interface vs =
   let info = open_nl vs in
   let handlers = make_handlers info in
   let intf =
      { New.heartbeat_rate      = info.nl_heartbeat_rate;
        New.install             = install info;
        New.exit                = exit info
      }
   in
      info, intf

(*
 * Setup the application.
 * The main call is elsewhere.
 *)
let setup_nl () =
   (*
    * Get default transport for this group,
    * and override the endpoint.
    *)
   let ls, vs = Appl.default_info "nlapp" in
   let endpt = Endpt.named "NLAPP" in
   let vs = View.set vs [Vs_view (Arrayf.create 1 endpt)] in
   let ls = View.local "NLAPP" endpt vs in
   let state = ls, vs in

   (*
    * Initialize the application interface.
    *)
   let info, interface = make_interface state in
   let handle_gen, interface = Appl_handle.New.f interface in
      (*
       * Initialize the protocol stack, using the interface and
       * view state chosen above.  then enter the main loop.
       *)
      state, interface

let main () =
   let state, interface = setup_nl () in
   let interface = Appl_intf.New.power interface in
   let interface = Appl_intf.New.debug_view "NLSERVER" interface in
   let _ = Appl.config_new interface state in
      Appl.main_loop ()

let _ =
   Printexc.catch (Unix.handle_unix_error main) ();


(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
