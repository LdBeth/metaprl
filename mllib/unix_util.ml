(*
 * Extra unix utilities.
 *)

(*
 * Copy a file.
 *)
let rec complete_write fd buf off len =
   let count = Unix.write fd buf off len in
      if count < len then
         complete_write fd buf (off + count) (len - count)

let rec copy_file_fd buffer from_fd to_fd =
   let count = Unix.read from_fd buffer 0 (String.length buffer) in
      if count > 0 then
         begin
            complete_write to_fd buffer 0 count;
            copy_file_fd buffer from_fd to_fd
         end

let copy_file from_name to_name =
   let from_fd = Unix.openfile from_name [Unix.O_RDONLY] 438 in
      try
         let to_fd = Unix.openfile to_name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 438 in
            try
               copy_file_fd (String.create 8192) from_fd to_fd;
               Unix.close from_fd;
               Unix.close to_fd
            with
               x ->
                  Unix.close to_fd;
                  raise x
      with x ->
            Unix.close from_fd;
            raise x

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
