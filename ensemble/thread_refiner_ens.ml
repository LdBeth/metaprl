(*
 * This is the null thread implementation.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf
open Mp_debug

open Refiner.Refiner.RefineError

open Remote_sig
open Thread_util
open Thread_refiner_sig

let debug_strategy =
   create_debug (**)
      { debug_name = "strategy";
        debug_description = "Show tactic strategy";
        debug_value = false
      }

let debug_sync =
   create_debug (**)
      { debug_name = "sync";
        debug_description = "Show event synchronization";
        debug_value = false
      }

let debug_schedule =
   create_debug (**)
      { debug_name = "schedule";
        debug_description = "Show thread scheduling";
        debug_value = false
      }

let debug_remote =
   create_debug (**)
      { debug_name = "remote";
        debug_description = "Show remote process management";
        debug_value = false
      }

external yield : unit -> unit = "caml_thread_yield"

(*
 * This functions are lifted out for speed, and so that
 * the marshaler does not get extra suprious values in
 * the closures for these functions.
 *)
module ThreadRefinerTacticals =
struct
   (*
    * These are the values that a tactic returns.
    *)
   type ('term, 'extract) t =
      Value of 'term list * 'extract
    | All1 of ('term, 'extract) tactic * ('term, 'extract) tactic * 'term
    | All2 of ('term, 'extract) tactic * ('term, 'extract) tactic list * 'term
    | AllF of ('term, 'extract) tactic * ('term list -> ('term, 'extract) t list) * 'term
    | First of ('term, 'extract) tactic list * 'term

   and ('term, 'extract) tactic = 'term -> ('term, 'extract) t

   (*
    * Constructors.
    *)
   let create_value args ext =
      Value (args, ext)

   let first tacs arg =
      First (tacs, arg)

   let compose1 tac1 tac2 arg =
      All1 (tac1, tac2, arg)

   let compose2 tac1 tacs2 arg =
      All2 (tac1, tacs2, arg)

   let composef tac1 tacf arg =
      AllF (tac1, tacf, arg)
end

module MakeThreadRefiner (Arg : ThreadRefinerArgSig) =
struct
   module Remote = (* Remote_monitor.MakeMonitor *) (Remote_ensemble.Remote)

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type extract = Arg.extract
   type 'term t = ('term, extract) ThreadRefinerTacticals.t
   type 'term tactic = ('term, extract) ThreadRefinerTacticals.tactic

   (*
    * Shared memory keys.
    *)
   type 'share key = 'share Remote.key

   (*
    * We keep a stack of goals.
    *)
   type 'term entry =
      AndEntryThen1 of 'term tactic * 'term tactic * 'term
    | AndEntryThen2 of 'term tactic * 'term tactic list * 'term
    | AndEntryThenF of 'term tactic * ('term list -> 'term t list) * 'term
    | AndEntry1 of 'term tactic * 'term list * extract * ('term list * extract) list
    | AndEntry2 of 'term tactic list * 'term list * extract * ('term list * extract) list
    | AndEntryF of 'term t list * extract * ('term list * extract) list
    | OrEntry of 'term tactic list * 'term
    | ValueEntry of 'term list * extract

   (*
    * Entries in or-queues are one of three values.
    *)
   type exn_error = string * refine_error

   type 'term or_result =
      OrSuccess of 'term list * extract
    | OrFailure of exn_error
    | OrPending

   (*
    * Messages from the scheduler to a thread.
    *)
   type sched_interrupt =
      SchedNone
    | SchedStack
    | SchedCancel

   type 'term sched_message =
      SchedTacticArg of 'term tactic * 'term
    | SchedThread of 'term t

   (*
    * Messages from the thread to the scheduler.
    *)
   type 'term proc_message =
      ProcSuccess of 'term list * extract
    | ProcFailure of exn_error
    | ProcStack of 'term entry
    | ProcCanceled

   (*
    * This is the message returned to a client.
    *)
   type 'term job_message =
      JobSuccess of 'term list * extract
    | JobFailure of exn_error

   (*
    * The client canceled an operation.
    *)
   type 'term client_message =
      ClientCancel

   (*
    * This is the message sent as a submission.
    * The channel is used to send the response.
    *)
   type 'term submit_message =
      { submit_goal : 'term t;
        submit_request : 'term client_message Thread_event.channel;
        submit_response : 'term job_message Thread_event.channel
      }

   (*
    * A union type for getting messages from lots of places.
    *)
   type 'term any_message =
      ProcMessage of 'term proc_entry * 'term proc_message
    | SubmitMessage of 'term submit_message
    | ClientMessage of 'term root_entry * 'term client_message
    | RemoteMessage of 'term remote_entry * 'term job_message
    | CancelMessage of 'term local_entry
    | RequestMessage of ('term sched_message, 'term job_message) Remote.local

   (*
    * The scheduler saves a doubly-linked tree of entries.
    *)
   and 'term tree =
      SProcess of 'term proc_entry
    | SRemote of 'term remote_entry
    | SPending of 'term pending_entry
    | SAndEntryThen1 of 'term and_then_entry1
    | SAndEntryThen2 of 'term and_then_entry2
    | SAndEntryThenF of 'term and_then_entryf
    | SAndEntry of 'term and_entry
    | SAndEntryF of 'term and_entryf
    | SOrEntry of 'term or_entry
    | SRoot of 'term root_entry
    | SLocal of 'term local_entry

   and 'term proc_entry =
      { proc_arg : 'term sched_message;
        proc_process : 'term process;
        mutable proc_parent : 'term tree
      }

   and 'term remote_entry =
      { remote_arg : 'term sched_message;
        remote_id : int;
        remote_hand : ('term sched_message, 'term job_message) Remote.handle;
        mutable remote_parent : 'term tree
      }

   and 'term pending_entry =
      { pending_arg : 'term sched_message;
        pending_parent : 'term tree
      }

   and 'term root_entry =
      { root_request : 'term client_message Thread_event.channel;
        root_response : 'term job_message Thread_event.channel;
        mutable root_child : 'term tree
      }

   and 'term local_entry =
      { local_local : ('term sched_message, 'term job_message) Remote.local;
        mutable local_child : 'term tree
      }

   and 'term and_then_entry1 =
      { and_then1_tac1 : 'term tactic;
        and_then1_tac2 : 'term tactic;
        and_then1_arg : 'term;
        mutable and_then1_parent : 'term tree;
        mutable and_then1_child : 'term tree
      }

   and 'term and_then_entry2 =
      { and_then2_tac1 : 'term tactic;
        and_then2_tacs2 : 'term tactic list;
        and_then2_arg : 'term;
        mutable and_then2_parent : 'term tree;
        mutable and_then2_child : 'term tree
      }

   and 'term and_then_entryf =
      { and_thenf_tac1 : 'term tactic;
        and_thenf_tacf : 'term list -> 'term t list;
        and_thenf_arg : 'term;
        mutable and_thenf_parent : 'term tree;
        mutable and_thenf_child : 'term tree
      }

   and 'term and_entry =
      { and_extract : extract;
        mutable and_parent : 'term tree;
        mutable and_children : (int * 'term tree) list;
        and_results : ('term list * extract) option array
      }

   and 'term and_entryf =
      { andf_extract : extract;
        mutable andf_children : (int * 'term tree) list;
        mutable andf_parent : 'term tree;
        andf_results : ('term list * extract) option array
      }

   and 'term or_entry =
      { mutable or_parent : 'term tree;
        mutable or_children : (int * 'term tree) list;
        or_results : 'term or_result array
      }

   (*
    * A process has a channel for returning its results,
    * and it also has a flag for interrupts from the scheduler.
    * The status is managed by the scheduler, not the process.
    *)
   and 'term process =
      { mutable proc_wakeup : bool;
        mutable proc_interrupt : sched_interrupt;
        mutable proc_status : proc_status;
        proc_pid : int;
        proc_printer : out_channel -> 'term -> unit;
        proc_result : 'term proc_message Thread_event.channel;
        proc_request : 'term sched_message Thread_event.channel
      }

   and proc_status =
      StatusIdle
    | StatusRunning
    | StatusWaiting
    | StatusCanceled

   (*
    * The scheduler has a channel for submitting new jobs.
    *    idle: the list of processes that are idle
    *    waiting: the list of leaves of
    *          running processes that have not yet responded to
    *          a stack or cancelation request.
    *    running: the list of leaves that are being processed
    *    pending: the list of leaves waiting to be processed
    *
    *    clients: a list of all the roots for the trees
    *       being processed.
    *
    *    events: a list of all possible events that the
    *       scheduler is interested it.
    *
    *    submit: the channel used to submit new jobs to
    *       the scheduler.
    *)
   type ('term, 'share) scheduler =
      { sched_nomarshal : unit -> unit;
        sched_printer : out_channel -> 'term -> unit;
        sched_remote : ('term sched_message, 'term job_message, 'share) Remote.t;
        mutable sched_idle : 'term process list;
        mutable sched_waiting : 'term proc_entry list;
        mutable sched_running : 'term proc_entry list;
        mutable sched_remotes : 'term remote_entry list;
        mutable sched_pending : 'term pending_entry list;
        mutable sched_roots   : 'term root_entry list;
        mutable sched_locals  : 'term local_entry list;
        sched_submit : 'term submit_message Thread_event.channel
      }

   (*
    * A server is just a scheduler.
    *)
   type ('term, 'share) server = ('term, 'share) scheduler

   (*
    * This is the number of threads we fork.
    * Once OCaml gets real multithreading, we
    * will want to increase this count to the
    * number of processors.
    *)
   let thread_count = 2

   (*
    * This is the number of remote jobs we issue.
    * This should be just large enough that the
    * Ensemble queues rarely get empty.
    *)
   let set_int _ iref i =
      iref := i

   let remote_count = Env_arg.int "remote" 5 "max number of remote jobs" set_int

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Printing.
    *)
   let print_term_list debug_print out terms =
      let print_term t =
         fprintf out "\t\targ = << %a >>%t" debug_print t eflush
      in
         List.iter print_term terms

   let print_stack_entry debug_print = function
      AndEntryThen1 (_, _, arg) ->
         eprintf "\tAndEntryThen1: %a%t" debug_print arg eflush
    | AndEntryThen2 (_, _, arg) ->
         eprintf "\tAndEntryThen2: %a%t" debug_print arg eflush
    | AndEntryThenF (_, _, arg) ->
         eprintf "\tAndEntryThenF: %a%t" debug_print arg eflush
    | AndEntry1 (_, args, _, _) ->
         eprintf "\tAndEntry1:\n%a" (print_term_list debug_print) args
    | AndEntry2 (_, args, _, _) ->
         eprintf "\tAndEntry2:\n%a" (print_term_list debug_print) args
    | AndEntryF _ ->
         eprintf "\tAndEntryF%t" eflush
    | OrEntry (_, arg) ->
         eprintf "\tOrEntry: %a%t" debug_print arg eflush
    | ValueEntry (args, _) ->
         eprintf "\tValueEntry:\n%a" (print_term_list debug_print) args

   let print_stack printer stack =
      eprintf "Stack:%t" eflush;
      List.iter (print_stack_entry (fun out _ -> fprintf out "*")) stack

   (*
    * Print a process status.
    *)
   let print_status out status =
      let s =
         match status with
            StatusIdle ->
               "Idle"
          | StatusCanceled ->
               "Canceled"
          | StatusWaiting ->
               "Wait"
          | StatusRunning ->
               "Running"
      in
         output_string out s

   (*
    * Scheduler stack.
    *)
   let tab level =
      output_char stderr '\n';
      for i = 0 to level do
         output_string stderr "  "
      done

   let rec print_sched_stack_aux debug_print level entry =
      tab level;
      let children =
         match entry with
            SRoot { root_child = child } ->
               output_string stderr "(Root";
               [child]
          | SLocal { local_child = child } ->
               output_string stderr "(Local";
               [child]
          | SOrEntry { or_children = children } ->
               output_string stderr "(Or";
               List.map snd children
          | SAndEntry { and_children = children } ->
               output_string stderr "(And";
               List.map snd children
          | SAndEntryF { andf_children = children } ->
               output_string stderr "(AndF";
               List.map snd children
          | SAndEntryThen1 { and_then1_child = child } ->
               output_string stderr "(AndThen1";
               [child]
          | SAndEntryThen2 { and_then2_child = child } ->
               output_string stderr "(AndThen2";
               [child]
          | SAndEntryThenF { and_thenf_child = child } ->
               output_string stderr "(AndThenF";
               [child]
          | SProcess { proc_process = { proc_pid = pid } } ->
               eprintf "(Process %d" pid;
               []
          | SRemote { remote_id = id } ->
               eprintf "(Remote %d" id;
               []
          | SPending _ ->
               output_string stderr "(Pending";
               []
      in
         List.iter (print_sched_stack_aux debug_print (succ level)) children;
         output_string stderr ")"

   let print_sched_stack printer child =
      print_sched_stack_aux printer 0 child;
      eflush stderr

   let print_sched_stacks sched =
      List.iter (fun root -> print_sched_stack sched.sched_printer root.root_child) sched.sched_roots

   (*
    * Check that the stack is linked correctly.
    *)
   let rec check_parent parent entry =
      match entry with
         SRoot { root_child = child } ->
            raise (Failure "check_parent: SRoot")
       | SLocal { local_child = child } ->
            raise (Failure "check_parent: SLocal")
       | SOrEntry { or_parent = parent'; or_children = children } ->
            assert(parent' == parent);
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntry { and_parent = parent'; and_children = children } ->
            assert(parent' == parent);
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntryF { andf_parent = parent'; andf_children = children } ->
            assert(parent' == parent);
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntryThen1 { and_then1_parent = parent'; and_then1_child = child } ->
            assert(parent' == parent);
            check_parent entry child
       | SAndEntryThen2 { and_then2_parent = parent'; and_then2_child = child } ->
            assert(parent' == parent);
            check_parent entry child
       | SAndEntryThenF { and_thenf_parent = parent'; and_thenf_child = child } ->
            assert(parent' == parent);
            check_parent entry child
       | SProcess { proc_parent = parent' } ->
            assert(parent' == parent)
       | SRemote { remote_parent = parent' } ->
            assert(parent' == parent)
       | SPending { pending_parent = parent' } ->
            assert(parent' == parent)

   let rec check_sched_stack entry =
      match entry with
         SRoot { root_child = child } ->
            check_parent entry child
       | SLocal { local_child = child } ->
            check_parent entry child
       | SOrEntry { or_children = children } ->
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntry { and_children = children } ->
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntryF { andf_children = children } ->
            List.iter (fun (_, child) -> check_parent entry child) children
       | SAndEntryThen1 { and_then1_child = child } ->
            check_parent entry child
       | SAndEntryThen2 { and_then2_child = child } ->
            check_parent entry child
       | SAndEntryThenF { and_thenf_child = child } ->
            check_parent entry child
       | SProcess _
       | SRemote _
       | SPending _ ->
            ()

   let check_sched_stacks sched =
      List.iter (fun client -> check_sched_stack client.root_child) sched.sched_roots

   (************************************************************************
    * PROCESSING                                                           *
    ************************************************************************)

   (*
    * Pop the stack until the next choice goal,
    * or until the stack is empty.
    *)
   let rec pop_failure exn = function
      entry :: stack ->
         begin
            match entry with
               AndEntry1 _
             | AndEntry2 _
             | AndEntryF _
             | AndEntryThen1 _
             | AndEntryThen2 _
             | AndEntryThenF _
             | OrEntry ([_], _) ->
                  pop_failure exn stack
             | OrEntry (_ :: tacs, goal) ->
                  OrEntry (tacs, goal) :: stack
             | OrEntry ([], _)
             | ValueEntry _ ->
                  raise (Invalid_argument "pop_failure")
         end
    | [] ->
         let name, exn = exn in
            raise (RefineError (name, exn))

   (*
    * Pop a successful entry.
    *)
   let rec flatten argsl extl = function
      (args, ext) :: subgoals ->
         flatten (args @ argsl) (ext :: extl) subgoals
    | [] ->
         argsl, extl

   let rec pop_flatten ext subgoals stack =
      let argsl, extl = flatten [] [] subgoals in
         pop_success argsl (Arg.compose ext extl) stack

   and pop_success args ext = function
      entry :: stack ->
         begin
            match entry with
               AndEntryThen1 (_, tac, _) ->
                  if args = [] then
                     pop_success args ext stack
                  else
                     AndEntry1 (tac, args, ext, []) :: stack
             | AndEntryThen2 (_, tacs, _) ->
                  if args = [] then
                     pop_success args ext stack
                  else
                     AndEntry2 (tacs, args, ext, []) :: stack
             | AndEntryThenF (_, tacf, _) ->
                  begin
                     try
                        let goals = tacf args in
                           AndEntryF (goals, ext, []) :: stack
                     with
                        RefineError (name, exn) ->
                           pop_failure (name, exn) stack
                  end
             | AndEntry1 (tac, _ :: goals, ext', subgoals) ->
                  if goals = [] then
                     pop_flatten ext ((args, ext') :: subgoals) stack
                  else
                     AndEntry1 (tac, goals, ext, (args, ext') :: subgoals) :: stack
             | OrEntry _ ->
                  pop_success args ext stack
             | AndEntry2 ([_], [_], ext', subgoals) ->
                  pop_flatten ext ((args, ext') :: subgoals) stack
             | AndEntry2 (_ :: tacs, _ :: goals, ext', subgoals) ->
                  AndEntry2 (tacs, goals, ext, (args, ext') :: subgoals) :: stack
             | AndEntryF (_ :: goals, ext', subgoals) ->
                  if goals = [] then
                     pop_flatten ext ((args, ext') :: subgoals) stack
                  else
                     AndEntryF (goals, ext, (args, ext') :: subgoals) :: stack
             | _ ->
                  raise (Invalid_argument "pop_success")
         end
    | [] ->
         [ValueEntry (args, ext)]

   (*
    * Push a new goal onto the stack.
    *)
   let push_goal goal stack =
      match goal with
         ThreadRefinerTacticals.Value (args, ext) ->
            pop_success args ext stack
       | ThreadRefinerTacticals.First (tacs, arg) ->
            OrEntry (tacs, arg) :: stack
       | ThreadRefinerTacticals.All1 (tac1, tac2, arg) ->
            AndEntryThen1 (tac1, tac2, arg) :: stack
       | ThreadRefinerTacticals.All2 (tac1, tacs2, arg) ->
            AndEntryThen2 (tac1, tacs2, arg) :: stack
       | ThreadRefinerTacticals.AllF (tac1, tacf, arg) ->
            AndEntryThenF (tac1, tacf, arg) :: stack

   (*
    * Evaluate the stack.
    *)
   let eval_entry entry stack =
      match entry with
         AndEntry1 (tac, goal :: _, _, _) ->
            begin
               try push_goal (tac goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | AndEntry2 (tac :: _, goal :: _, _, _) ->
            begin
               try push_goal (tac goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | AndEntryF (arg :: _, _, _) ->
            push_goal arg (entry :: stack)
       | OrEntry ([tac], goal) ->
            begin
               try push_goal (tac goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | OrEntry (tac :: tacs, goal) ->
            begin
               try push_goal (tac goal) (entry :: stack) with
                  RefineError _ ->
                     OrEntry (tacs, goal) :: stack
            end
       | AndEntryThen1 (tac1, _, goal) ->
            begin
               try push_goal (tac1 goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | AndEntryThen2 (tac1, _, goal) ->
            begin
               try push_goal (tac1 goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | AndEntryThenF (tac1, _, goal) ->
            begin
               try push_goal (tac1 goal) (entry :: stack) with
                  RefineError (name, exn) ->
                     pop_failure (name, exn) stack
            end
       | AndEntry1 _
       | AndEntry2 _
       | AndEntryF _
       | OrEntry _
       | ValueEntry _ ->
            raise (Invalid_argument "expand: invalid stack entry")

   (*
    * Handle a message from the scheduler.
    *)
   let rec big_stack i = function
      _ :: t ->
         if i = 2 then
            true
         else
            big_stack (succ i) t
    | [] ->
         false

   let rec handle_wakeup proc stack =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_wakeup: %d%t" proc.proc_pid eflush;
            unlock_printer ()
         end;
      proc.proc_wakeup <- false;
      match proc.proc_interrupt with
         SchedStack ->
            send_stack proc stack
       | SchedCancel ->
            ProcCanceled
       | SchedNone ->
            raise (Invalid_argument "handle_wakeup")

   (*
    * Return some stack entries to the scheduler.
    * Keep processing until the stack has more than one
    * entry.  Return the remaining stack after the split.
    *)
   and send_stack proc stack =
      match stack with
         [ValueEntry (args, ext)] ->
            ProcSuccess (args, ext)
       | [] ->
            raise (Invalid_argument "send_stack")
       | stack ->
            if big_stack 0 stack then
               let stack, split = List_util.split_last stack in
                  if !debug_sync then
                     begin
                        lock_printer ();
                        eprintf "Thread_refiner.send_stack: %d sending stack: %d%t" proc.proc_pid (List.length stack) eflush;
                        print_stack_entry proc.proc_printer split;
                        unlock_printer ()
                     end;
                  Thread_event.sync proc.proc_pid (Thread_event.send proc.proc_result (ProcStack split));
                  if !debug_sync then
                     begin
                        lock_printer ();
                        eprintf "Thread_refiner.send_stack: %d done%t" proc.proc_pid eflush;
                        unlock_printer ()
                     end;
                  eval_stack proc stack
            else
               match stack with
                  entry :: stack ->
                     send_stack proc (eval_entry entry stack)
                | [] ->
                     raise (Invalid_argument "Thread_refiner_ens.send_stack")

   (*
    * Loop until exception is raised.
    *)
   and eval_stack proc stack =
      if !debug_strategy then
         begin
            lock_printer ();
            eprintf "Thread_refiner.eval_stack: Process %d, Stack:%t" proc.proc_pid eflush;
            print_stack proc.proc_printer stack;
            unlock_printer ()
         end;
      if proc.proc_wakeup then
         handle_wakeup proc stack
      else
         match stack with
            [ValueEntry (args, ext)] ->
               ProcSuccess (args, ext)
          | entry :: stack ->
               yield ();
               eval_stack proc (eval_entry entry stack)
          | [] ->
               raise (Invalid_argument "eval_stack")

   (*
    * This is the thread main loop.
    *)
   let rec process_main proc =
      let result =
         try
            if !debug_sync then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner.process_main: waiting %d%t" proc.proc_pid eflush;
                  unlock_printer ()
               end;
            let arg = Thread_event.sync proc.proc_pid (Thread_event.receive proc.proc_request) in
               if !debug_sync then
                  begin
                     lock_printer ();
                     eprintf "Thread_refiner.process_main: got argument %d%t" proc.proc_pid eflush;
                     unlock_printer ()
                  end;
               match arg with
                  SchedTacticArg (tac, arg) ->
                     eval_stack proc (push_goal (tac arg) [])
                | SchedThread goal ->
                     eval_stack proc (push_goal goal [])
         with
            RefineError (name, exn) ->
               ProcFailure (name, exn)
      in
         proc.proc_wakeup <- false;
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.process_main: sending result: %d%t" proc.proc_pid eflush;
               unlock_printer ()
            end;
         Thread_event.sync proc.proc_pid (Thread_event.send proc.proc_result result);
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.process_main: sent result: %d%t" proc.proc_pid eflush;
               unlock_printer ()
            end;
         process_main proc

   (*
    * Create a process.
    *)
   let rec create_procs printer count =
      if count = 0 then
         []
      else
         let proc =
            { proc_wakeup = false;
              proc_interrupt = SchedNone;
              proc_status = StatusIdle;
              proc_pid = count;
              proc_printer = printer;
              proc_request = Thread_event.new_channel ();
              proc_result = Thread_event.new_channel ()
            }
         in
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner.create_procs: %d%t" count eflush;
                  unlock_printer ()
               end;
            Thread.create process_main proc;
            proc :: create_procs printer (pred count)

   (************************************************************************
    * SCHEDULING UTILITIES                                                 *
    ************************************************************************)

   (*
    * Iterate over the second component.
    *)
   let rec iter_snd f = function
      (_, x) :: t ->
         f x;
         iter_snd f t
    | [] ->
         ()

   (*
    * Remove an element from a list.
    * Do not fail if it is not found.
    *)
   let rec removeq x = function
      h :: t ->
         if h == x then
            t
         else
            h :: removeq x t
    | [] ->
         []

   let rec removeq_error x = function
      h :: t ->
         if h == x then
            t
         else
            h :: removeq_error x t
    | [] ->
         raise (Invalid_argument "removeq_error")

   (*
    * Find the process in the list of children.
    *)
   let find_process proc =
      let rec search = function
         (_, ((SProcess proc') as entry)) :: tl ->
            if proc' == proc then
               entry
            else
               search tl
       | _ :: tl ->
            search tl
       | [] ->
            eprintf "Thread_refiner.find_process: %d%t" proc.proc_process.proc_pid eflush;
            print_sched_stack proc.proc_process.proc_printer proc.proc_parent;
            raise (Invalid_argument "find_process")
      in
         match proc.proc_parent with
            SOrEntry parent ->
               search parent.or_children
          | SAndEntry parent ->
               search parent.and_children
          | SAndEntryF parent ->
               search parent.andf_children
          | SAndEntryThen1 parent ->
               parent.and_then1_child
          | SAndEntryThen2 parent ->
               parent.and_then2_child
          | SAndEntryThenF parent ->
               parent.and_thenf_child
          | SRoot parent ->
               parent.root_child
          | SLocal parent ->
               parent.local_child
          | SProcess _
          | SRemote _
          | SPending _ ->
               raise (Invalid_argument "set_child_process")

   (*
    * Find the remote in the list of children.
    *)
   let find_remote remote =
      let rec search = function
         (_, ((SRemote remote') as entry)) :: tl ->
            if remote' == remote then
               entry
            else
               search tl
       | _ :: tl ->
            search tl
       | [] ->
            eprintf "Thread_refiner.find_remote: %d%t" remote.remote_id eflush;
            raise (Invalid_argument "find_remote")
      in
         match remote.remote_parent with
            SOrEntry parent ->
               search parent.or_children
          | SAndEntry parent ->
               search parent.and_children
          | SAndEntryF parent ->
               search parent.andf_children
          | SAndEntryThen1 parent ->
               parent.and_then1_child
          | SAndEntryThen2 parent ->
               parent.and_then2_child
          | SAndEntryThenF parent ->
               parent.and_thenf_child
          | SRoot parent ->
               parent.root_child
          | SLocal parent ->
               parent.local_child
          | SProcess _
          | SRemote _
          | SPending _ ->
               raise (Invalid_argument "find_remote")

   (*
    * Replace a particular child with a new one.
    *)
   let rec replace_child old_child new_child = function
      ((i, child) as h) :: t ->
         if child == old_child then
            (i, new_child) :: t
         else
            h :: replace_child old_child new_child t
    | [] ->
         raise (Invalid_argument "replace_child")

   let set_child parent old_child new_child =
      match parent with
         SOrEntry parent ->
            parent.or_children <- replace_child old_child new_child parent.or_children
       | SAndEntry parent ->
            parent.and_children <- replace_child old_child new_child parent.and_children
       | SAndEntryF parent ->
            parent.andf_children <- replace_child old_child new_child parent.andf_children
       | SAndEntryThen1 parent ->
            parent.and_then1_child <- new_child
       | SAndEntryThen2 parent ->
            parent.and_then2_child <- new_child
       | SAndEntryThenF parent ->
            parent.and_thenf_child <- new_child
       | SRoot parent ->
            parent.root_child <- new_child
       | SLocal parent ->
            parent.local_child <- new_child
       | SProcess _
       | SRemote _
       | SPending _ ->
            raise (Invalid_argument "set_child")

   (*
    * Replace a pending entry with a new entry.
    *)
   let rec replace_pending pending proc = function
      (i, SPending pending') as hd :: tl ->
         if pending' == pending then
            (i, proc) :: tl
         else
            hd :: replace_pending pending proc tl
    | hd :: tl ->
         hd :: replace_pending pending proc tl
    | _ ->
         raise (Invalid_argument "replace_pending")

   let set_child_process parent pending proc =
      match parent with
         SOrEntry parent ->
            parent.or_children <- replace_pending pending proc parent.or_children
       | SAndEntry parent ->
            parent.and_children <- replace_pending pending proc parent.and_children
       | SAndEntryF parent ->
            parent.andf_children <- replace_pending pending proc parent.andf_children
       | SAndEntryThen1 parent ->
            parent.and_then1_child <- proc
       | SAndEntryThen2 parent ->
            parent.and_then2_child <- proc
       | SAndEntryThenF parent ->
            parent.and_thenf_child <- proc
       | SRoot parent ->
            parent.root_child <- proc
       | SLocal parent ->
            parent.local_child <- proc
       | SProcess _
       | SRemote _
       | SPending _ ->
            raise (Invalid_argument "set_child_process")

   (*
    * Determine the state of the or-queue.
    *)
   let or_queue_state results =
      let len = Array.length results in
      let rec search exn i =
         if i = len then
            OrFailure exn
         else
            let result = results.(i) in
               match result with
                  OrSuccess _ ->
                     result
                | OrFailure exn ->
                     search exn (succ i)
                | OrPending ->
                     result
      in
      let result = results.(0) in
         match result with
            OrSuccess _ ->
               result
          | OrFailure exn ->
               search exn 1
          | OrPending ->
               result

   (*
    * Append the new and1 subgoals.
    *)
   let rec spread_and1 sched i parent tac = function
      arg :: args ->
         let pending =
            { pending_arg = SchedTacticArg (tac, arg);
              pending_parent = parent
            }
         in
            sched.sched_pending <- pending :: sched.sched_pending;
            (i, SPending pending) :: spread_and1 sched (succ i) parent tac args
    | [] ->
         []

   let append_and1 sched i parent child tac = function
      arg :: args ->
         (i, child) :: spread_and1 sched (succ i) parent tac args
    | [] ->
         raise (Invalid_argument "append_and1")

   (*
    * Append the new and2 subgoals.
    *)
   let rec spread_and2 sched i parent tacs args =
      match tacs, args with
         tac :: tacs, arg :: args ->
            let pending =
               { pending_arg = SchedTacticArg (tac, arg);
                 pending_parent = parent
               }
            in
               sched.sched_pending <- pending :: sched.sched_pending;
               (i, SPending pending) :: spread_and2 sched (succ i) parent tacs args
       | [], [] ->
            []
       | _ ->
            raise (Invalid_argument "spread_and2")

   let append_and2 sched i parent child tacs args =
      match tacs, args with
         _ :: tacs, _ :: args ->
            (i, child) :: spread_and2 sched (succ i) parent tacs args
       | _ ->
            raise (Invalid_argument "append_and2")

   (*
    * Append the new andf subgoals.
    *)
   let rec spread_andf sched i parent = function
      arg :: args ->
         let pending =
            { pending_arg = SchedThread arg;
              pending_parent = parent
            }
         in
            sched.sched_pending <- pending :: sched.sched_pending;
            (i, SPending pending) :: spread_andf sched (succ i) parent args
    | [] ->
         []

   let append_andf sched i parent child = function
      _ :: args ->
         (i, child) :: spread_andf sched (succ i) parent args
    | [] ->
         raise (Invalid_argument "append_andf")

   (*
    * Append the new or subgoals.
    *)
   let rec spread_or sched i parent tacs arg =
      match tacs with
         tac :: tacs ->
            let pending =
               { pending_arg = SchedTacticArg (tac, arg);
                 pending_parent = parent
               }
            in
               sched.sched_pending <- pending :: sched.sched_pending;
               (i, SPending pending) :: spread_or sched (succ i) parent tacs arg
       | [] ->
            []

   let append_or sched i parent child tacs arg =
         match tacs with
            tac :: tacs ->
               (i, child) :: spread_or sched (succ i) parent tacs arg
          | [] ->
               raise (Invalid_argument "append_or")

   (*
    * Create the initial results array from the list.
    * The list is reversed.
    *)
   let spread_results args_length res_length results =
      let res_array = Array.create (args_length + res_length) None in
      let rec spread i = function
         result :: results ->
            res_array.(i) <- Some result;
            spread (pred i) results
       | [] ->
            ()
      in
         spread (pred res_length) results;
         res_array

   (************************************************************************
    * SCHEDULER OPERATIONS                                                 *
    ************************************************************************)

   (*
    * Make a process idle.  Remove it from the wait/running queue,
    * and place it on the idle queue.
    *)
   let make_idle sched entry =
      let proc = entry.proc_process in
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.make_idle: %d%t" proc.proc_pid eflush;
               unlock_printer ()
            end;
         proc.proc_status <- StatusIdle;
         sched.sched_waiting <- removeq entry sched.sched_waiting;
         sched.sched_running <- removeq entry sched.sched_running;
         sched.sched_idle <- proc :: sched.sched_idle

   (*
    * Interrupt the process and move it to the
    * wait queue.
    *)
   let make_wait sched entry message status =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.make_wait: %d%t" entry.proc_process.proc_pid eflush;
            unlock_printer ()
         end;
      let proc = entry.proc_process in
         proc.proc_interrupt <- message;
         proc.proc_status <- status;
         proc.proc_wakeup <- true;
         sched.sched_running <- removeq_error entry sched.sched_running;
         sched.sched_waiting <- entry :: sched.sched_waiting

   (*
    * Move a process off the wait queue onto the running queue.
    *)
   let make_running sched entry =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.make_running: %d%t" entry.proc_process.proc_pid eflush;
            unlock_printer ()
         end;
      if entry.proc_process.proc_status <> StatusRunning then
         begin
            entry.proc_process.proc_status <- StatusRunning;
            sched.sched_waiting <- removeq_error entry sched.sched_waiting;
            sched.sched_running <- entry :: sched.sched_running
         end

   (*
    * Request the stack from all running processes.
    * All processes move onto the wait queue.
    *)
   let request_stack sched =
      let request entry =
         let proc = entry.proc_process in
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner.request_stack: %d%t" entry.proc_process.proc_pid eflush;
                  unlock_printer ()
               end;
            proc.proc_interrupt <- SchedStack;
            proc.proc_status <- StatusWaiting;
            proc.proc_wakeup <- true
      in
      let running = sched.sched_running in
         sched.sched_running <- [];
         sched.sched_waiting <- running @ sched.sched_waiting;
         List.iter request running

   (*
    * Cancel all the children in a tree.
    * We don't need to modify the tree in this function,
    * it is done by the calling function.
    *)
   let rec cancel_children sched = function
      SOrEntry { or_children = children } ->
         iter_snd (cancel_children sched) children
    | SAndEntry { and_children = children } ->
         iter_snd (cancel_children sched) children
    | SAndEntryF { andf_children = children } ->
         iter_snd (cancel_children sched) children
    | SAndEntryThen1 { and_then1_child = child } ->
         cancel_children sched child
    | SAndEntryThen2 { and_then2_child = child } ->
         cancel_children sched child
    | SAndEntryThenF { and_thenf_child = child } ->
         cancel_children sched child
    | SRoot { root_child = child } ->
         cancel_children sched child
    | SLocal { local_child = child } ->
         cancel_children sched child
    | SProcess entry ->
         begin
            let proc = entry.proc_process in
               if !debug_sync then
                  begin
                     lock_printer ();
                     eprintf "Thread_refiner.cancel_children: %d%t" proc.proc_pid eflush;
                     unlock_printer ()
                  end;
               match proc.proc_status with
                  StatusRunning ->
                     make_wait sched entry SchedCancel StatusCanceled
                | StatusWaiting ->
                     proc.proc_status <- StatusCanceled
                | StatusIdle ->
                     (* This is the process that is canceling *)
                     ()
                | StatusCanceled ->
                     raise (Invalid_argument "cancel_children")
         end
    | SRemote entry ->
         if !debug_remote then
            begin
               lock_printer ();
               eprintf "Thread_refiner.cancel_children: cancel remote %d%t" entry.remote_id eflush;
               unlock_printer ()
            end;
         Remote.cancel_handle sched.sched_remote entry.remote_hand;
         sched.sched_remotes <- removeq entry sched.sched_remotes
    | SPending entry ->
         sched.sched_pending <- removeq_error entry sched.sched_pending

   (*
    * Return a result to a client, and remove the client
    * from the client list.
    *)
   let return_result sched root result =
      if !debug_sync then
         begin
            lock_printer ();
            eprintf "Thread_refiner.return_result%t" eflush;
            unlock_printer ();
         end;
      Thread_event.sync 0 (Thread_event.send root.root_response result);
      if !debug_sync then
         begin
            lock_printer ();
            eprintf "Thread_refiner.return_result: done%t" eflush;
            unlock_printer ()
         end;
      sched.sched_roots <- removeq_error root sched.sched_roots

   let return_success sched root args ext =
      return_result sched root (JobSuccess (args, ext))

   let return_failure sched root exn =
      return_result sched root (JobFailure exn)

   (*
    * Return a value to the remote handler.
    *)
   let remote_success sched local args ext =
      if !debug_remote then
         begin
            lock_printer ();
            eprintf "Thread_refiner.remote_success: start%t" eflush;
            unlock_printer ()
         end;
      Remote.return_local sched.sched_remote local.local_local (JobSuccess (args, ext));
      sched.sched_locals <- removeq_error local sched.sched_locals

   let remote_failure sched local exn =
      if !debug_remote then
         begin
            lock_printer ();
            eprintf "Thread_refiner.remote_failure%t" eflush;
            unlock_printer ()
         end;
      Remote.return_local sched.sched_remote local.local_local (JobFailure exn);
      sched.sched_locals <- removeq_error local sched.sched_locals

   (*
    * Get the index for an entry in the list of children.
    *)
   let rec pop_index entry = function
      ((index, entry') as h) :: t ->
         if entry == entry' then
            index, t
         else
            let index, t = pop_index entry t in
               index, h :: t
    | [] ->
         raise (Failure "pop_index")

   (*
    * Handle a success in a process.
    *)
   let rec sched_flatten argsl extl i results =
      if i < 0 then
         argsl, extl
      else
         match results.(i) with
            Some (args, ext) ->
               sched_flatten (args @ argsl) (ext :: extl) (pred i) results
          | None ->
               raise (Invalid_argument "sched_flatten")

   let rec sched_pop_flatten sched child parent ext results =
      let argsl, extl = sched_flatten [] [] (pred (Array.length results)) results in
         sched_pop_success sched child parent argsl (Arg.compose ext extl)

   (*
    * Pop and and-success.
    *)
   and sched_pop_and_success sched child this entry args ext =
      let index, children = pop_index child entry.and_children in
         entry.and_children <- children;
         entry.and_results.(index) <- Some (args, ext);
         match entry with
            { and_children = [];
              and_extract = ext;
              and_parent = parent;
              and_results = results
            } ->
               sched_pop_flatten sched this parent ext results
          | _ ->
               ()

   and sched_pop_andf_success sched child this entry args ext =
      let index, children = pop_index child entry.andf_children in
         entry.andf_children <- children;
         entry.andf_results.(index) <- Some (args, ext);
         match entry with
            { andf_children = [];
              andf_extract = ext;
              andf_parent = parent;
              andf_results = results
            } ->
               sched_pop_flatten sched this parent ext results
          | _ ->
               ()

   and sched_pop_and_then1_success sched this entry args ext =
      if args = [] then
         sched_pop_success sched this entry.and_then1_parent args ext
      else
         let parent = entry.and_then1_parent in
         let and_entry =
            { and_extract = ext;
              and_parent = parent;
              and_children = [];
              and_results = Array.create (List.length args) None
            }
         in
         let new_entry = SAndEntry and_entry in
            set_child parent this new_entry;
            and_entry.and_children <- spread_and1 sched 0 new_entry entry.and_then1_tac2 args

   and sched_pop_and_then2_success sched this entry args ext =
      if args = [] then
         begin
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner.sched_pop_and_then2_success: continuing%t" eflush;
                  unlock_printer ()
               end;
            sched_pop_success sched this entry.and_then2_parent args ext
         end
      else
         let parent = entry.and_then2_parent in
         let and_entry =
            { and_extract = ext;
              and_parent = parent;
              and_children = [];
              and_results = Array.create (List.length args) None
            }
         in
         let new_entry = SAndEntry and_entry in
            set_child parent this new_entry;
            and_entry.and_children <- spread_and2 sched 0 new_entry entry.and_then2_tacs2 args

   and sched_pop_and_thenf_success sched this entry args ext =
      try
         match entry.and_thenf_tacf args with
            [] ->
               sched_pop_success sched this entry.and_thenf_parent [] ext
          | goals ->
               let parent = entry.and_thenf_parent in
               let andf_entry =
                  { andf_extract = ext;
                    andf_parent = parent;
                    andf_children = [];
                    andf_results = Array.create (List.length goals) None
                  }
               in
               let new_entry = SAndEntryF andf_entry in
                  set_child parent this new_entry;
                  andf_entry.andf_children <- spread_andf sched 0 new_entry goals
      with
         RefineError (name, exn) ->
            sched_pop_failure sched this entry.and_thenf_parent (name, exn)

   (*
    * Pop the or-success.
    *)
   and sched_pop_or_result sched child this entry result =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.sched_pop_or_result%t" eflush;
            unlock_printer ()
         end;
      let index, children = pop_index child entry.or_children in
         entry.or_children <- children;
         entry.or_results.(index) <- result;
         match or_queue_state entry.or_results with
            OrSuccess (args, ext) ->
               iter_snd (cancel_children sched) children;
               entry.or_children <- [];
               sched_pop_success sched this entry.or_parent args ext
          | OrFailure exn ->
               sched_pop_failure sched this entry.or_parent exn
          | OrPending ->
               ()

   and sched_pop_or_success sched child this entry args ext =
      sched_pop_or_result sched child this entry (OrSuccess (args, ext))

   and sched_pop_or_failure sched child this entry exn =
      sched_pop_or_result sched child this entry (OrFailure exn)

   (*
    * Handle a success from a process.
    *)
   and sched_pop_success sched child this args ext =
      match this with
         SAndEntry entry ->
            sched_pop_and_success sched child this entry args ext
       | SAndEntryF entry ->
            sched_pop_andf_success sched child this entry args ext
       | SOrEntry entry ->
            sched_pop_or_success sched child this entry args ext
       | SAndEntryThen1 entry ->
            sched_pop_and_then1_success sched this entry args ext
       | SAndEntryThen2 entry ->
            sched_pop_and_then2_success sched this entry args ext
       | SAndEntryThenF entry ->
            sched_pop_and_thenf_success sched this entry args ext
       | SRoot entry ->
            return_success sched entry args ext
       | SLocal entry ->
            remote_success sched entry args ext
       | SProcess _
       | SRemote _
       | SPending _ ->
            raise (Invalid_argument "sched_pop_success")

   (*
    * Handle a failure from a process.
    *)
   and sched_pop_failure sched child this exn =
      match this with
         SAndEntry ({ and_children = children; and_parent = parent } as entry) ->
            sched_pop_failure sched this parent exn
       | SAndEntryF ({ andf_children = children; andf_parent = parent } as entry) ->
            sched_pop_failure sched this parent exn
       | SAndEntryThen1 { and_then1_parent = parent } ->
            sched_pop_failure sched this parent exn
       | SAndEntryThen2 { and_then2_parent = parent } ->
            sched_pop_failure sched this parent exn
       | SAndEntryThenF { and_thenf_parent = parent } ->
            sched_pop_failure sched this parent exn
       | SOrEntry entry ->
            sched_pop_or_failure sched child this entry exn
       | SRoot entry ->
            cancel_children sched this;
            return_failure sched entry exn
       | SLocal entry ->
            cancel_children sched this;
            remote_failure sched entry exn
       | SProcess _
       | SRemote _
       | SPending _ ->
            raise (Invalid_argument "sched_pop_failure")

   (*
    * Handle client success/failure.  The process is placed on the idle queue.
    *)
   let handle_proc_success sched entry args ext =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_success: %d%t" entry.proc_process.proc_pid eflush;
            unlock_printer ()
         end;
      if entry.proc_process.proc_status = StatusCanceled then
         make_idle sched entry
      else
         let this = find_process entry in
            sched_pop_success sched this entry.proc_parent args ext;
            make_idle sched entry

   let handle_proc_failure sched entry exn =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_failure: %d%t" entry.proc_process.proc_pid eflush;
            unlock_printer ()
         end;
      if entry.proc_process.proc_status = StatusCanceled then
         make_idle sched entry
      else
         let this = find_process entry in
            make_idle sched entry;
            sched_pop_failure sched this entry.proc_parent exn

   let handle_remote_success sched entry args ext =
      if !debug_remote then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_remote_success: %d%t" entry.remote_id eflush;
            unlock_printer ()
         end;
      sched.sched_remotes <- removeq_error entry sched.sched_remotes;
      sched_pop_success sched (find_remote entry) entry.remote_parent args ext

   let handle_remote_failure sched entry exn =
      if !debug_remote then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_remote_failure: %d%t" entry.remote_id eflush;
            unlock_printer ()
         end;
      sched.sched_remotes <- removeq_error entry sched.sched_remotes;
      sched_pop_failure sched (find_remote entry) entry.remote_parent exn

   (*
    * Handle the reception of a new stack entry.
    *)
   let handle_stack_aux sched proc stack =
      let parent = proc.proc_parent in
      let _ =
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.handle_stack_aux: %d%t" proc.proc_process.proc_pid eflush;
               print_sched_stack sched.sched_printer parent;
               unlock_printer ()
            end
      in
      let entry = find_process proc in
      let _ =
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.handle_stack_aux: %d%t" proc.proc_process.proc_pid eflush;
               print_sched_stack sched.sched_printer entry;
               unlock_printer ()
            end
      in
      let new_entry =
         match stack with
            AndEntryThen1 (tac1, tac2, arg) ->
               SAndEntryThen1 { and_then1_tac1 = tac1;
                                and_then1_tac2 = tac2;
                                and_then1_arg = arg;
                                and_then1_parent = parent;
                                and_then1_child = entry
               }
          | AndEntryThen2 (tac1, tacs2, arg) ->
               SAndEntryThen2 { and_then2_tac1 = tac1;
                                and_then2_tacs2 = tacs2;
                                and_then2_arg = arg;
                                and_then2_parent = parent;
                                and_then2_child = entry
               }
          | AndEntryThenF (tac1, tacf, arg) ->
               SAndEntryThenF { and_thenf_tac1 = tac1;
                                and_thenf_tacf = tacf;
                                and_thenf_arg = arg;
                                and_thenf_parent = parent;
                                and_thenf_child = entry
               }
          | AndEntry1 (tac, args, ext, results) ->
               let args_length = List.length args in
               let res_length = List.length results in
               let this =
                  { and_extract = ext;
                    and_parent = parent;
                    and_children = [];
                    and_results = spread_results args_length res_length results
                  }
               in
               let this_entry = SAndEntry this in
                  this.and_children <- append_and1 sched res_length this_entry entry tac args;
                  this_entry

          | AndEntry2 (tacs, args, ext, results) ->
               let args_length = List.length args in
               let res_length = List.length results in
               let this =
                  { and_extract = ext;
                    and_parent = parent;
                    and_children = [];
                    and_results = spread_results args_length res_length results
                  }
               in
               let this_entry = SAndEntry this in
                  this.and_children <- append_and2 sched res_length this_entry entry tacs args;
                  this_entry

          | AndEntryF (args, ext, results) ->
               let args_length = List.length args in
               let res_length = List.length results in
               let this =
                  { andf_extract = ext;
                    andf_parent = parent;
                    andf_children = [];
                    andf_results = spread_results args_length res_length results
                  }
               in
               let this_entry = SAndEntryF this in
                  this.andf_children <- append_andf sched res_length this_entry entry args;
                  this_entry

          | OrEntry (tacs, arg) ->
               let this =
                  { or_parent = parent;
                    or_children = [];
                    or_results = Array.create (List.length tacs) OrPending
                  }
               in
               let this_entry = SOrEntry this in
                  this.or_children <- append_or sched 0 this_entry entry tacs arg;
                  this_entry

          | ValueEntry _ ->
               raise (Invalid_argument "handle_stack1")
      in
         set_child parent entry new_entry;
         proc.proc_parent <- new_entry;
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.handle_stack: new stacks:%t" eflush;
               print_sched_stacks sched;
               check_sched_stacks sched;
               print_sched_stack sched.sched_printer new_entry;
               unlock_printer ()
            end

   let handle_stack sched entry stack =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_stack: %d%t" entry.proc_process.proc_pid eflush;
            print_sched_stacks sched;
            check_sched_stacks sched;
            print_sched_stack sched.sched_printer entry.proc_parent;
            unlock_printer ()
         end;
      let proc = entry.proc_process in
         if proc.proc_status = StatusCanceled then
            begin
               proc.proc_interrupt = SchedCancel;
               proc.proc_wakeup <- true
            end
         else
            begin
               handle_stack_aux sched entry stack;
               make_running sched entry
            end

   (*
    * A process cancels after we sent it a cancelation request.
    * The process must be on the wait queue.
    *)
   let handle_process_cancelation sched entry =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_process_cancelation: %d%t" entry.proc_process.proc_pid eflush;
            unlock_printer ()
         end;
      make_idle sched entry

   (*
    * Handle a new submission.
    *)
   let handle_submission sched
       { submit_goal = goal;
         submit_request = request;
         submit_response = response
       } =
      let rec pending =
         { pending_arg = SchedThread goal;
           pending_parent = SRoot root
         }
      and root =
         { root_request = request;
           root_response = response;
           root_child = SPending pending
         }
      in
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.handle_submission%t" eflush;
               unlock_printer ()
            end;
         sched.sched_roots <- root :: sched.sched_roots;
         sched.sched_pending <- pending :: sched.sched_pending

   (*
    * Remove a client when it cancels its request.
    *)
   let handle_client_cancelation sched root =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_client_cancelation%t" eflush;
            unlock_printer ()
         end;
      sched.sched_roots <- removeq_error root sched.sched_roots;
      cancel_children sched (SRoot root)

   (*
    * Handle a new local submission.
    *)
   let handle_local_submission sched local =
      let rec pending =
         { pending_arg = Remote.arg_of_local local;
           pending_parent = SLocal root
         }
      and root =
         { local_local = local;
           local_child = SPending pending
         }
      in
         if !debug_remote then
            begin
               lock_printer ();
               eprintf "Thread_refiner.handle_local_submission: 0x%08x%t" (Obj.magic root) eflush;
               unlock_printer ()
            end;
         sched.sched_locals <- root :: sched.sched_locals;
         sched.sched_pending <- pending :: sched.sched_pending

   (*
    * Handle a local cancelation.
    *)
   let handle_local_cancelation sched local =
      if !debug_remote then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_local_cancelation%t" eflush;
            unlock_printer ()
         end;
      sched.sched_locals <- removeq_error local sched.sched_locals;
      cancel_children sched (SLocal local)

   (*
    * Handle all the events that the scheduler receives.
    * This function returns true if the state has changed
    * and the event list needs to be recalculated.
    *)
   let handle_event sched event =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.handle_event: got_event%t" eflush;
            unlock_printer ()
         end;
      match event with
         ProcMessage (entry, msg) ->
            begin
               match msg with
                  ProcSuccess (args, ext) ->
                     if !debug_schedule then
                        begin
                           lock_printer ();
                           eprintf "Thread_refiner_ens.handle_event: ProcSuccess%t" eflush;
                           unlock_printer ()
                        end;
                     handle_proc_success sched entry args ext
                | ProcFailure exn ->
                     if !debug_schedule then
                        begin
                           lock_printer ();
                           eprintf "Thread_refiner_ens.handle_event: ProcFailure%t" eflush;
                           unlock_printer ()
                        end;
                     handle_proc_failure sched entry exn
                | ProcStack stack ->
                     if !debug_schedule then
                        begin
                           lock_printer ();
                           eprintf "Thread_refiner_ens.handle_event: ProcStack%t" eflush;
                           unlock_printer ()
                        end;
                     handle_stack sched entry stack
                | ProcCanceled ->
                     if !debug_schedule then
                        begin
                           lock_printer ();
                           eprintf "Thread_refiner_ens.handle_event: ProcCanceled%t" eflush;
                           unlock_printer ()
                        end;
                     handle_process_cancelation sched entry
            end
       | SubmitMessage msg ->
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner_ens.handle_event: ProcMessage%t" eflush;
                  unlock_printer ()
               end;
            handle_submission sched msg
       | ClientMessage (root, msg) ->
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner_ens.handle_event: ClientMessage%t" eflush;
                  unlock_printer ()
               end;
            begin
               match msg with
                  ClientCancel ->
                     handle_client_cancelation sched root
            end
       | RemoteMessage (remote, msg) ->
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner_ens.handle_event: RemoteMessage%t" eflush;
                  unlock_printer ()
               end;
            begin
               match msg with
                  JobSuccess (args, ext) ->
                     handle_remote_success sched remote args ext
                | JobFailure exn ->
                     handle_remote_failure sched remote exn
            end
       | CancelMessage local ->
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner_ens.handle_event: CancelMessage%t" eflush;
                  unlock_printer ()
               end;
            handle_local_cancelation sched local
       | RequestMessage local ->
            if !debug_schedule then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner_ens.handle_event: RequestMessage%t" eflush;
                  unlock_printer ()
               end;
            handle_local_submission sched local

   (************************************************************************
    * SCHEDULER MAIN LOOP                                                  *
    ************************************************************************)

   (*
    * Get events for any possible event the scheduler may be interested in.
    * Always poll for:
    *    1. Running threads
    *    2. Waiting threads
    *    3. New submissions
    *    4. Remote jobs
    *
    * If all jobs are idle, and there are pending remote jobs,
    * also request a job from the remote server.
    *)
   let process_event ({ proc_process = proc } as event) =
      Thread_event.wrap (Thread_event.receive proc.proc_result) (fun msg -> ProcMessage (event, msg))

   let stack_event ({ root_request = request } as root) =
      Thread_event.wrap (Thread_event.receive request) (fun msg -> ClientMessage (root, msg))

   let submit_event sched =
      Thread_event.wrap (Thread_event.receive sched.sched_submit) (fun msg -> SubmitMessage msg)

   let remote_event sched ({ remote_hand = hand } as remote) =
      Remote.wrap (Remote.event_of_handle sched.sched_remote hand) (fun msg -> (RemoteMessage (remote, msg)))

   let local_event sched ({ local_local = hand } as local) =
      Remote.wrap (Remote.event_of_local sched.sched_remote hand) (fun () -> (CancelMessage local))

   let remote_request_event sched =
      (Remote.wrap (Remote.request sched.sched_remote) (fun msg -> RequestMessage msg))

   let schedule_events sched =
      let block_events =
         submit_event sched ::
            ((List.map process_event sched.sched_running) @
                (List.map process_event sched.sched_waiting) @
                (List.map stack_event sched.sched_roots))
      in
      let block_event =
         Remote.wrap_event (Thread_event.choose block_events)
      in
      let events =
         block_event ::
            ((List.map (remote_event sched) sched.sched_remotes) @
                (List.map (local_event sched) sched.sched_locals))
      in
         if sched.sched_running = [] && sched.sched_waiting = [] then
            remote_request_event sched :: events
         else
            events

   (*
    * Start a process working on a particular job.
    *)
   let start_process sched proc ({ pending_arg = msg; pending_parent = parent } as pending) =
      let entry =
         { proc_arg = msg;
           proc_process = proc;
           proc_parent = parent
         }
      in
         proc.proc_status <- StatusRunning;
         proc.proc_wakeup <- false;
         set_child_process parent pending (SProcess entry);
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.start_process: %d%t" proc.proc_pid eflush;
               unlock_printer ()
            end;
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.start_process %d: stack%t" proc.proc_pid eflush;
               print_sched_stacks sched;
               check_sched_stacks sched;
               print_sched_stack sched.sched_printer parent;
               unlock_printer ()
            end;
         Thread_event.sync 0 (Thread_event.send proc.proc_request msg);
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.start_process: sent %d%t" proc.proc_pid eflush;
               unlock_printer ()
            end;
         entry

   (*
    * Start a remote job.
    *)
   let max_remote_id remotes =
      let rec search i = function
         { remote_id = j } :: remotes ->
            search (max i j) remotes
       | [] ->
            succ i
      in
         search 0 remotes

   let start_remote sched ({ pending_arg = msg; pending_parent = parent } as pending) =
      let id = max_remote_id sched.sched_remotes in
      let _ =
         if !debug_remote then
            begin
               lock_printer ();
               eprintf "Thread_refiner.start_remote: %d:" id;
               begin
                  match msg with
                     SchedTacticArg _ ->
                        eprintf "tactic"
                   | SchedThread _ ->
                        eprintf "goal"
               end;
               eflush stderr;
               unlock_printer ()
            end
      in
      let hand = Remote.submit sched.sched_remote msg in
      let entry =
         { remote_arg = msg;
           remote_hand = hand;
           remote_id = id;
           remote_parent = parent
         }
      in
         set_child_process parent pending (SRemote entry);
         entry

   (*
    * After the scheduler has handled an event, start
    * new processes, and potentially ask running threads for their stack.
    *)
   let rec schedule sched =
      let { sched_idle = idle;
            sched_pending = pending;
            sched_running = running;
            sched_remotes = remotes
          } = sched
      in
         match pending with
            pending :: pendings ->
               begin
                  match idle with
                     proc :: procs ->
                        sched.sched_running <- start_process sched proc pending :: running;
                        sched.sched_idle <- procs;
                        sched.sched_pending <- pendings;
                        schedule sched
                   | [] ->
                        if List.length remotes < !remote_count then
                           begin
                              sched.sched_remotes <- start_remote sched pending :: remotes;
                              sched.sched_pending <- pendings;
                              schedule sched
                           end
               end
          | [] ->
               (* Always try to keep a few pending jobs *)
               request_stack sched

   (*
    * The scheduler waits for events to occur,
    * then potentially reschedules.
    *)
   let rec sched_main_loop sched =
      let events = schedule_events sched in
      let _ =
         if !debug_schedule then
            begin
               lock_printer ();
               eprintf "Thread_refiner.sched_main_loop: Stacks:";
               print_sched_stacks sched;
               check_sched_stacks sched;
               List.iter (fun proc ->
                     eprintf "\nIdle process %d (%a)" (**)
                        proc.proc_pid
                        print_status proc.proc_status) (**)
                  sched.sched_idle;
               List.iter (fun proc ->
                     eprintf "\nRunning process %d (%a):" (**)
                        proc.proc_process.proc_pid
                        print_status proc.proc_process.proc_status;
                     print_sched_stack sched.sched_printer proc.proc_parent) (**)
                  sched.sched_running;
               List.iter (fun proc ->
                     eprintf "\nWaiting process %d (%a):" (**)
                        proc.proc_process.proc_pid
                        print_status proc.proc_process.proc_status;
                     print_sched_stack sched.sched_printer proc.proc_parent) (**)
                  sched.sched_waiting;
               eprintf "Event count: %d%t" (List.length events) eflush;
               unlock_printer ()
            end
      in
      let _ =
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.sched_main_loop: waiting";
               List.iter (fun proc -> eprintf " %d" proc.proc_process.proc_pid) sched.sched_running;
               output_string stderr " /";
               List.iter (fun proc -> eprintf " %d" proc.proc_process.proc_pid) sched.sched_waiting;
               eflush stderr;
               unlock_printer ()
            end
      in
      let _ =
         match Remote.select sched.sched_remote (schedule_events sched) with
            None ->
               if !debug_sync then
                  begin
                     lock_printer ();
                     eprintf "Thread_Refiner.sched_main_loop: timeout%t" eflush;
                     unlock_printer ()
                  end
          | Some event ->
               if !debug_sync then
                  begin
                     lock_printer ();
                     eprintf "Thread_Refiner.sched_main_loop: handling event%t" eflush;
                     unlock_printer ()
                  end;
               handle_event sched event
      in
         schedule sched;
         sched_main_loop sched

   (*
    * Create a refiner.
    *)
   external identity : unit -> unit = "%identity"

   let create printer =
      let printer = (fun out _ -> output_string out "#") in
      let sched =
         { sched_nomarshal = identity;
           sched_printer = printer;
           sched_remote = Remote.create ();
           sched_idle = [];
           sched_waiting = [];
           sched_running = [];
           sched_remotes = [];
           sched_pending = [];
           sched_roots = [];
           sched_locals = [];
           sched_submit = Thread_event.new_channel ()
         }
      in
         sched

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Constructors.
    *)
   let create_value = ThreadRefinerTacticals.create_value
   let first = ThreadRefinerTacticals.first
   let compose1 = ThreadRefinerTacticals.compose1
   let compose2 = ThreadRefinerTacticals.compose2
   let composef = ThreadRefinerTacticals.composef

   (*
    * Submit a job to the scheduler.
    * Only one job at a time.
    *)
   let eval_lock = Mutex.create ()

   let eval sched goal =
      if !debug_schedule then
         begin
            lock_printer ();
            eprintf "Thread_refiner.eval: new job%t" eflush;
            unlock_printer ()
         end;
      if Mutex.try_lock eval_lock = false then
         raise (Invalid_argument "Thread_refiner.eval: recursive call");
      let msg =
         { submit_goal = goal;
           submit_request = Thread_event.new_channel ();
           submit_response = Thread_event.new_channel ()
         }
      in
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.eval: submitting job%t" eflush;
               unlock_printer ()
            end;
         Thread_event.sync (-1) (Thread_event.send sched.sched_submit msg);
         if !debug_sync then
            begin
               lock_printer ();
               eprintf "Thread_refiner.eval: job submitted%t" eflush;
               unlock_printer ()
            end;
         let result = Thread_event.sync (-1) (Thread_event.receive msg.submit_response) in
            if !debug_sync or true then
               begin
                  lock_printer ();
                  eprintf "Thread_refiner.eval: job complete%t" eflush;
                  unlock_printer ()
               end;
            Mutex.unlock eval_lock;
            match result with
               JobSuccess (args, ext) ->
                  args, ext
             | JobFailure (name, exn) ->
                  raise (RefineError (name, exn))

   (*
    * Shared memory.
    *)
   let share sched x =
      Remote.share sched.sched_remote x

   let arg_of_key sched key =
      Remote.arg_of_key sched.sched_remote key

   (*
    * Start the main loop.
    *)
   let args = Remote.args

   let main_loop sched =
      sched.sched_idle <- create_procs sched.sched_printer thread_count;
      Thread.create sched_main_loop sched;
      Remote.main_loop sched.sched_remote
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
