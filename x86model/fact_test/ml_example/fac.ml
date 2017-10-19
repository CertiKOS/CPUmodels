open Printf
open Encode
open X86Syntax

(* No prefix *)
let null_prefix = {lock_rep = None; seg_override = None; op_override = false; addr_override = false}

(* Function composition *)
let (<|) f g = (fun x -> f (g x))

(* Register operands *)
let eax = Reg_op EAX
let ebx = Reg_op EBX
let ecx = Reg_op ECX
let edx = Reg_op EDX
let edi = Reg_op EDI
let ebp = Reg_op EBP
let esp = Reg_op ESP

let imm n = Imm_op (Big.of_int n)
let addr_reg_ofs r ofs = 
  match r with
  | Reg_op r ->
     Address_op {addrDisp = Big.of_int ofs; addrBase = Some r; addrIndex = None}
  | _ -> failwith "Not a register!"

let je l = Jcc (E_ct, Big.of_int l)

let fac = Offset_op (Big.of_int 0)
let l100 = Offset_op (Big.of_int 100)
let l101 = Offset_op (Big.of_int 90)
let main = Offset_op (Big.of_int 200)

(* Instructions *)
let fac_instrs =
  [
    SUB  (true, esp, imm 28);
    LEA  (eax, addr_reg_ofs esp 32);
    MOV  (true, addr_reg_ofs esp 4, eax);
    MOV  (true, addr_reg_ofs esp 8, ebx);
    MOV  (true, ebx, addr_reg_ofs eax 0);
    TEST (true, ebx, ebx);
    je   100;
    LEA  (eax, addr_reg_ofs ebx (-1));
    MOV  (true, addr_reg_ofs esp 0, eax);
    CALL (false, false, fac, None);
    IMUL (true, ebx, None, None);
    JMP  (false, false, l101, None);
  ]
