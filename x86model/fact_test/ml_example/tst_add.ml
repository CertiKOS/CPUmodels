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


(** The following test cases come from the course at

    http://www.c-jump.com/CIS77/CPU/x86/index.html

**)

(* 'AAA' *)
let aaa = encode AAA

(* Register operands *)
let eax = Reg_op EAX
let ebx = Reg_op EBX
let ecx = Reg_op ECX
let edx = Reg_op EDX
let edi = Reg_op EDI
let ebp = Reg_op EBP

(* 'add cl, al' should be encoded as '02 C8' *)
let add_cl_al = ADD (false, ecx, eax)
let () = assert (encode add_cl_al = ["02"; "C8"])

(* 'add ecx, eax' should be encoded as '03 C8' *)
let add_ecx_eax = ADD (true, ecx, eax)
let () = assert (encode add_ecx_eax = ["03"; "C8"])

(* 'add edx, [0x0F0E]' should be encoded as '03 15 0E 0F 00 00' *)
let add_edx_disp32 = 
  ADD (true, edx, 
       Address_op {addrDisp = Big.of_int 0x0F0E; addrBase = None; addrIndex = None})
let () = assert (encode add_edx_disp32 = ["03"; "15"; "0E"; "0F"; "00"; "00"])

(* 'add edi, [ebx]' should be encoded as '03 3B'*)
let add_edi_ind_ebx = 
  ADD (true, edi, 
       Address_op {addrDisp = Big.zero; addrBase = Some EBX; addrIndex = None})
let () = assert (encode add_edi_ind_ebx = ["03"; "3B"])

(* 'add eax, [esi + 0x1D]' should be encoded as '03 46 1D*)
let add_eax_esi_disp8 = 
  ADD (true, eax,
       Address_op {addrDisp = Big.of_int 0x1D; addrBase = Some ESI; addrIndex = None})
let () = assert (encode add_eax_esi_disp8 = ["03"; "46"; "1D"])

(* 'add ebx, [ebp + 0x1D34]' should be encoded as '03 9D 34 1D 00 00*)
let add_ebx_ebp_disp32 = 
  ADD (true, ebx,
       Address_op {addrDisp = Big.of_int 0x1D34; addrBase = Some EBP; addrIndex = None})
let () = assert (encode add_ebx_ebp_disp32 = ["03"; "9D"; "34"; "1D"; "00"; "00"])

(* 'add ebp, [0x1D34 + eax*1]' should be encoded as '03 2C 05 34 1D 00 00' *)
let add_ebp_disp32_eax =
  ADD (true, ebp,
       Address_op {addrDisp = Big.of_int 0x1D34; addrBase = None; addrIndex = Some(Scale1,EAX)})
let () = assert (encode add_ebp_disp32_eax = ["03"; "2C"; "05"; "34"; "1D"; "00"; "00"])

(* 'add ecx, [ebx + edi*4]' should be encoded as '03 0C BB' *)
let add_ecx_ebx_edi_4 =
  ADD (true, ecx,
       Address_op {addrDisp = Big.zero; addrBase = Some EBX; addrIndex = Some(Scale4,EDI)})
let () = assert (encode add_ecx_ebx_edi_4 = ["03"; "0C"; "BB"])
