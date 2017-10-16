open Printf
open Encode
open X86Syntax

(* No prefix *)
let null_prefix = {lock_rep = None; seg_override = None; op_override = false; addr_override = false}

(* Function composition *)
let (<|) f g = (fun x -> f (g x))

(* Convert an integer representing a byte to hexadecimal *)
let byte_dec_to_hex byte =
  sprintf "%02X" byte

(* Encode an instruction with no prefix into hex *)
let encode instr = 
  let bits = x86_encode null_prefix instr in 
  match bits with
  | None -> failwith ("Encoding failed")
  | Some bits -> List.map (byte_dec_to_hex <| Big.to_int) bits


(* 'AAA' *)
let aaa = encode AAA

(* Register operands *)
let eax = Reg_op EAX
let ebx = Reg_op EBX
let ecx = Reg_op ECX
let edx = Reg_op EDX

(* 'add cl, al' should be encoded as '02 C8' *)
let add_cl_al = ADD (false, ecx, eax)
let () = assert (encode add_cl_al = ["02"; "C8"])

(* 'add ecx, eax' should be encoded as '03 C8' *)
let add_ecx_eax = ADD (true, ecx, eax)
let () = assert (encode add_ecx_eax = ["03"; "C8"])

(* 'add edx, 0x0F0E' should be encoded as '03 15 0E 0F 00 00' *)
let add_edx_0f0e = 
  ADD (true, edx, 
       Address_op {addrDisp = Big.of_int 0x0F0E; addrBase = None; addrIndex = None})
let () = assert (encode add_edx_0f0e = ["03"; "15"; "0E"; "0F"; "00"; "00"])


  
