(*
 * Bignums are represented as signed magnitude.
 * The magnitude is a list of digits, least-significant
 * first.
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

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type big_int = bool * int list

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Maximum digit size.
 *)
let shift_int = 15
let carry_int = 1 lsl shift_int
let max_int = pred carry_int

(*
 * Representation of zero.
 *)
let zero_big_int = true, []

(*
 * Most-significant digits are zeros.
 *)
let rec zeros = function
   0 :: t ->
      zeros t
 | _ :: t ->
      false
 | [] ->
      true

(*
 * Build from an int.
 *)
let rec make_mag i =
   if i = 0 then
      []
   else
      i land max_int :: make_mag (i lsr shift_int)

let big_int_of_int i =
   if i < 0 then
      false, make_mag (-i)
   else
      true, make_mag i

(*
 * Integer testing is conservative.
 *)
let is_integer_big_int (sign, mag) =
   match mag with
      []
    | [_]
    | [_; _] ->
         true
    | _ :: _ :: mag ->
         zeros mag

let integer_big_int (sign, mag) =
   let rec collect = function
      digit :: mag ->
         digit + ((collect mag) lsl shift_int)
    | [] ->
         0
   in
   let mag = collect mag in
      if sign then
         mag
      else
         -mag

(************************************************************************
 * COMPARISON                                                           *
 ************************************************************************)

(*
 * Compare two magnitudes.
 *)
let rec compare_mag val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         let comp = compare_mag val1 val2 in
            if comp = 0 then
               digit1 - digit2
            else
               comp
    | [], [] ->
         0
    | [], _ ->
         -1
    | _, [] ->
         1

let compare_big_int (sign1, val1) (sign2, val2) =
   if sign1 then
      if sign2 then
         compare_mag val1 val2
      else
         1
   else if sign2 then
      -1
   else
      compare_mag val2 val1

let rec eq_mag val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         digit1 = digit2 && eq_mag val1 val2
    | [], val2 ->
         zeros val2
    | val1, [] ->
         zeros val1

let eq_big_int (sign1, val1) (sign2, val2) =
   sign1 = sign2 && eq_mag val1 val2

(************************************************************************
 * ADDITION/SUBTRACTIOB                                                 *
 ************************************************************************)

(*
 * Addition of the magnitudes.
 *)
let rec add_mag carry val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         add_digit carry digit1 digit2 val1 val2
    | [], val2 ->
         add_carry carry val2
    | val1, [] ->
         add_carry carry val1

and add_digit carry digit1 digit2 val1 val2 =
   let z =
      if carry then
         digit1 + digit2 + 1
      else
         digit1 + digit2
   in
      if z > max_int then
         z land max_int :: add_mag true val1 val2
      else
         z :: add_mag false val1 val2

and add_carry carry digits =
   if carry then
      match digits with
         digit :: vals ->
            if digit = max_int then
               0 :: add_carry true vals
            else
               succ digit :: vals
       | [] ->
            [1]
   else
      digits

(*
 * Subtraction of magnitudes.
 *)
let rec sub_mag borrow val1 val2 =
   match val1, val2 with
      digit1 :: val1, digit2 :: val2 ->
         sub_digit borrow digit1 digit2 val1 val2
    | val1, [] ->
         sub_borrow borrow val1
    | [], val2 ->
         raise (Failure "sub_mag")

and sub_digit borrow digit1 digit2 val1 val2 =
   let z =
      if borrow then
         digit1 - digit2 - 1
      else
         digit1 - digit2
   in
      if z < 0 then
         z land max_int :: sub_mag true val1 val2
      else
         z :: sub_mag false val1 val2

and sub_borrow borrow val1 =
   if borrow then
      match val1 with
         digit1 :: val1 ->
            if digit1 = 0 then
               max_int :: sub_borrow true val1
            else
               pred digit1 :: val1
       | [] ->
            raise (Failure "sub_borrow")
   else
      val1

(*
 * Bigint addition.
 *)
let add_big_int (sign1, val1) (sign2, val2) =
   if sign1 then
      if sign2 then
         true, add_mag false val1 val2
      else
         let comp = compare_mag val1 val2 in
            if comp = 0 then
               zero_big_int
            else if comp < 0 then
               false, sub_mag false val2 val1
            else
               true, sub_mag false val1 val2
   else if sign2 then
      let comp = compare_mag val1 val2 in
         if comp = 0 then
            zero_big_int
         else if comp < 0 then
            true, sub_mag false val2 val1
         else
            false, sub_mag false val1 val2
   else
      false, add_mag false val1 val2

(*
 * Bigint subtraction.
 *)
let sub_big_int val1 (sign2, val2) =
   add_big_int val1 (not sign2, val2)

(************************************************************************
 * MULTIPLICATION                                                       *
 ************************************************************************)

(*
 * Long multiplication.
 *)
let rec mult_mag total val1 = function
   digit2 :: val2 ->
      mult_mag (add_mag false (mult_digit 0 digit2 val1) total) (shift_mag val1) val2
 | [] ->
      total

and mult_digit carry digit2 = function
   digit1 :: val1 ->
      let z = digit2 * digit1 + carry in
         z land max_int :: mult_digit (z lsr shift_int) digit2 val1
 | [] ->
      if carry <> 0 then
         [carry]
      else
         []

and shift_mag val1 =
   0 :: val1

(*
 * Bigint multiplication.
 *)
let mult_big_int (sign1, val1) (sign2, val2) =
   (sign1 = sign2, mult_mag [] val1 val2)

(************************************************************************
 * DIVISION                                                             *
 ************************************************************************)

(*
 * Right shift by one bit.
 *)
let rec div2 = function
   [digit] ->
      [digit lsr 1]
 | digit :: ((digit' :: _) as vals) ->
      (((digit + (digit' lsl shift_int)) lsr 1) land max_int) :: div2 vals
 | [] ->
      []

(*
 * We use a simple, expensive binary search.
 *)
let rec div_mag min max num den =
   let mid = div2 (add_mag false min max) in
      if eq_mag mid min then
         mid
      else
         let prod = mult_mag [] mid den in
         let comp = compare_mag prod num in
            if comp = 0 then
               mid
            else if comp > 0 then
               div_mag min mid num den
            else
               div_mag mid max num den

let div_big_int (sign1, val1) (sign2, val2) =
   sign1 = sign2, div_mag [] val1 val1 val2

let mod_mag num den =
   let quo = div_mag [] num num den in
      sub_mag false num (mult_mag [] den quo)

let mod_big_int (sign1, val1) (sign2, val2) =
   sign1 = sign2, mod_mag val1 val2

let quo_big_int = div_big_int
let rem_big_int = mod_big_int

(*
 * Absolute value.
 *)
let abs_big_int (_, mag) =
   (true, mag)

(************************************************************************
 * STRING CONVERSION                                                    *
 ************************************************************************)

(*
 * Divide the number by 10.
 * Magnitude is in reverse order.
 *)
let div10_rev_mag digits =
   let rec collect rem = function
      digit :: digits ->
         let digit = digit + rem * (succ max_int) in
         let rem, digits = collect (digit mod 10) digits in
            rem, (digit / 10) :: digits
    | [] ->
         rem, []
   in
      collect 0 digits

let div10 (sign, mag) =
   let rem, mag = div10_rev_mag (List.rev mag) in
      rem, (sign, List.rev mag)

(*
 * Multiply a mag by 10.
 *)
let rec mult10_mag digits =
   let rec collect carry = function
      digit :: digits ->
         let z =
            digit * 10 + carry
         in
            z land max_int :: collect (z lsr shift_int) digits
    | [] ->
         if carry = 0 then
            []
         else
            [carry]
   in
      collect 0 digits

let mult10 (sign, mag) =
   sign, mult10_mag mag

let zero_code = Char.code '0'

let add_char mag c =
   let i = Char.code c - zero_code in
   let rec collect carry digits =
      if carry = 0 then
         digits
      else
         match digits with
            digit :: digits ->
               let z = digit + carry in
                  z land max_int :: collect (z lsr shift_int) digits
          | [] ->
               [carry]
   in
      collect i mag

(*
 * Produce a string.
 *)
let string_of_big_int (sign, mag) =
   let flatten = function
      [] ->
         "0"
    | digits ->
         let s, start =
            if sign then
               String.create (List.length digits), 0
            else
               String.make (succ (List.length digits)) '-', 1
         in
         let rec collect i = function
            digit :: digits ->
               s.[i] <- Char.chr (Char.code '0' + digit);
               collect (succ i) digits
          | [] ->
               s
         in
            collect start digits
   in
   let rec collect digits mag =
      if zeros mag then
         flatten digits
      else
         let rem, quo = div10_rev_mag mag in
            collect (rem :: digits) quo
   in
      collect [] (List.rev mag)

(*
 * Produce it from a string.
 *)
let big_int_of_string s =
   let len = String.length s in
   let rec collect i mag =
      if i = len then
         mag
      else
         collect (succ i) (add_char (mult10_mag mag) s.[i])
   in
      if len = 0 then
         true, []
      else if s.[0] = '-' then
         false, collect 1 []
      else
         true, collect 0 []

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
