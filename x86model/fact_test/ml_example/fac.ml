open Printf
open Encode
open X86Syntax
open Elf

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
  | Some bits -> List.map Big.to_int bits

(* Register operands *)
let eax = Reg_op EAX
let ebx = Reg_op EBX
let ecx = Reg_op ECX
let edx = Reg_op EDX
let edi = Reg_op EDI
let ebp = Reg_op EBP
let esp = Reg_op ESP

let imm n = Imm_op (Big.of_int n)
(* offset to DS *)
let offset i = Offset_op (Big.of_int i)

let addr_reg_ofs r ofs = 
  match r with
  | Reg_op r ->
     Address_op {addrDisp = Big.of_int ofs; addrBase = Some r; addrIndex = None}
  | _ -> failwith "Not a register!"
let addr_glob i =
  Address_op {addrDisp = Big.of_int i; addrBase = None; addrIndex = None}

let je l = Jcc (E_ct, Big.of_int l)

(* Encode a list of Rocksalt instructions into a list of bytes *)
let encode_accum (einstrs, n) instr =
  try
    let ei = encode instr in
    (ei :: einstrs, n+1)
  with
    Failure msg -> failwith (sprintf "Encoding failed at the %d-th instruction" n)

let encode_instrs instrs =
  let (l, _) = List.fold_left encode_accum ([], 0) instrs in
  List.rev l

(* Write encoded instructions to a binary file *)
let write_ecd_instrs file_name little_endian ecd_instrs = 
  let dump_channel = open_out_bin file_name in
  let ecd_instrs' = if little_endian then ecd_instrs
                    else List.map (fun b -> List.rev b) ecd_instrs 
  in
  List.iter (fun instr -> List.iter (fun b -> output_byte dump_channel b) instr) ecd_instrs';
  close_out dump_channel


(* Code of fac *)
let fac_code =
  [
(* fac: *)
    SUB  (true, esp, imm 28);
    LEA  (eax, addr_reg_ofs esp 32);
    MOV  (true, addr_reg_ofs esp 4, eax);
    MOV  (true, addr_reg_ofs esp 8, ebx);
    MOV  (true, ebx, addr_reg_ofs eax 0);
    TEST (true, ebx, ebx);
    je   0x10;
    LEA  (eax, addr_reg_ofs ebx (-1));
    MOV  (true, addr_reg_ofs esp 0, eax);
    CALL (true, false, imm (-0x20), None);
    IMUL (false, ebx, Some eax, None);
    JMP  (true, false, imm 0x5, None);
(* .L100: *)
    MOV  (true, ebx, (imm 1));
(* .L101: *)
    MOV  (true, eax, ebx);
    MOV  (true, ebx, addr_reg_ofs esp 8);
    ADD  (true, esp, imm 28);
    RET  (true, None);
(* End of fac *)
(* main: *)
    SUB  (true, esp, imm 12);
    LEA  (eax, addr_reg_ofs esp 16);
    MOV  (true, addr_reg_ofs esp 4, eax);
    MOV  (true, eax, imm 4);
    MOV  (true, addr_reg_ofs esp 0, eax);
    CALL (true, false, imm (-0x4C), None);
    MOV  (true, offset 0, eax);
    ADD  (true, esp, imm 12);
    RET  (true, None)
  ]

let fac_bytes = encode_instrs fac_code
  
let fac_dump_file = "fac_rs"
let () = write_ecd_instrs fac_dump_file true fac_bytes

let fac_elf_header = create_386_exec_elf_header 0x80480c9 52 240 2 4 3
let fac_elf = {
    ef_header = fac_elf_header;
    ef_sec_headers = [];
    ef_prog_headers = [];
    ef_sections = [];
  }

let () = write_elf "elf_tst" fac_elf

