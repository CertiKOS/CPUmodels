open X86Syntax
open Big
open Enc_dec
open PrettyPrinter
open Random

let choose_reg() = 
  coq_Z_to_register (of_int (Random.int 8))
  
let choose_word_int () = 
   power two (of_int (Random.int 32))
   
let choose_int8 () = 
   power two (of_int (Random.int 8))

let choose_int16 () = 
   power two (of_int (Random.int 16))
  
let choose_scale () = 
   coq_Z_to_scale (of_int (Random.int 4))

let choose_bool() = 
   Random.bool()

let choose_segment_reg() = 
	match (Random.int 6) with
	| 0 -> ES
	| 1 -> CS
	| 2 -> SS
	| 3 -> DS
	| 4 -> FS
	| _ -> GS

let choose_control_reg() = 
	match (Random.int 4) with
	| 0 -> CR0
	| 1 -> CR2
	| 2 -> CR3 
	| _ -> CR4

let choose_debug_reg() = 
	match (Random.int 6) with
	| 0 -> DR0
	| 1 -> DR1
	| 2 -> DR2
	| 3 -> DR3
	| 4 -> DR6
	| _ -> DR7

let choose_lock_or_rep() = 	
	match (Random.int 3) with 
	| 0 -> Coq_lock
	| 1 -> Coq_rep
	| _ -> Coq_repn

let choose_reg_or_immed() = 
	match (Random.int 2) with 
	| 0 -> Reg_ri (choose_reg()) 
	| _ -> Imm_ri (choose_int8())

let choose_condition_type() =
	match (Random.int 15) with
	| 0 -> O_ct
	| 1 -> NO_ct
	| 2 -> B_ct
	| 3 -> NB_ct
	| 4 -> E_ct
	| 5 -> NE_ct
	| 6 -> BE_ct
	| 7 -> NBE_ct
	| 8 -> S_ct
	| 9 -> NS_ct
	| 10 -> P_ct
	| 11 -> NP_ct
	| 12 -> L_ct
	| 13 -> NL_ct
	| 14 -> LE_ct
	| _ -> NLE_ct
	
let choose_prefix () = 
	let lr_opt_c = choose_bool() in
        let sr_opt_c = choose_bool() in
        let op_c = choose_bool() in
        let addr_c = choose_bool() in

        let lock_rep = if lr_opt_c then Some (choose_lock_or_rep()) else None in
        let seg_override = if sr_opt_c then Some (choose_segment_reg()) else None in

        mkPrefix lock_rep seg_override op_c addr_c 
	(*Create new prefix for each kind of prefix combination defined in Decode.v *)
	
(*	
	match(Random.int 4) with     
       	  0 -> mkPrefix lock_rep None false false 
        | 1 -> mkPrefix None seg_override false false
	| 2 -> mkPrefix None None op_c false	
	| _ -> mkPrefix None None false addr_c
*)

let choose_addr() =
	let c_base = choose_bool() in
	let c_ind_scale_reg = choose_bool() in
	let displ = choose_word_int() in
	let base = if c_base then Some (choose_reg()) else None in
	let index = if c_ind_scale_reg then Some ((choose_scale()), (choose_reg())) else None in

	mkAddress displ base index

let choose_op() = 
  match (Random.int 4) with 
    0 -> let wint = choose_word_int () in Imm_op wint;
  | 1 -> let r = choose_reg () in Reg_op r;
  | 2 -> let addr = choose_addr() in Address_op addr;
  | _ -> let wint = choose_word_int () in Offset_op wint 

let choose_fp_reg() =
	of_int (Random.int 8)

let choose_fp_op() = 
	match (Random.int 5) with
	| 0 -> let fpr = choose_fp_reg() in FPS_op fpr
	| 1 -> FPM16_op (choose_addr())
	| 2 -> FPM32_op (choose_addr())
	| 3 -> FPM64_op (choose_addr())
	| _ -> FPM80_op (choose_addr())

let choose_two_ops() = 
	let op1 = choose_op() in
	let op2 = choose_op() in
	op1, op2

(**General purpose fuzz functions **)

let choose_ADC () = 
  let b = choose_bool()in
  let op1, op2 = choose_two_ops() in
  ADC (b, op1, op2)

let choose_ADD ()= 
  let b = choose_bool() in
  let op1, op2 = choose_two_ops() in
  ADD (b, op1, op2)

let choose_AND () = 
  let b = choose_bool() in
  let op1, op2 = choose_two_ops() in
  AND (b, op1, op2)

let choose_ARPL () = 
  let op1, op2 = choose_two_ops() in
  ARPL (op1, op2)

let choose_BOUND () = 
  let op1, op2 = choose_two_ops() in
  BOUND (op1, op2)

let choose_BSF () = 
  let op1, op2 = choose_two_ops() in
  BSF (op1, op2)

let choose_BSR () = 
  let op1, op2 = choose_two_ops() in
  BSR (op1, op2)

let choose_BSWAP () = 
  let reg = choose_reg () in
  BSWAP reg

let choose_BT () = 
  let op1, op2 = choose_two_ops() in
  BT (op1, op2)

let choose_BTC () = 
  let op1, op2 = choose_two_ops() in
  BTC (op1, op2)

let choose_BTR () = 
  let op1, op2 = choose_two_ops() in
  BTR (op1, op2)

let choose_BTS () = 
  let op1, op2 = choose_two_ops() in
  BTS (op1, op2)

let choose_CALL () = 
	let near_c = choose_bool() in
	let abs_c = choose_bool() in
	let opt_c = choose_bool() in
  	let selector = if opt_c then Some (choose_int16 ()) else None in
	let op1 = choose_op() in
  	CALL (near_c, abs_c, op1, selector)

let choose_CMOVcc () = 
	let ct = choose_condition_type () in
	let op1 = choose_op () in
	let op2 = choose_op () in
	CMOVcc (ct, op1, op2) 

let choose_CMP () = 
	let wc = choose_bool() in
	let op1 = choose_op () in
	let op2 = choose_op () in
	CMP (wc, op1, op2)

let choose_CMPS() = 
	CMPS (choose_bool())

let choose_CMPXCHG () = 
	let wc = choose_bool() in
	let op1, op2 = choose_two_ops() in
	CMPXCHG (wc, op1, op2)

let choose_DEC ()= 
	let wc = choose_bool() in
	let op1 = choose_op () in
	DEC (wc, op1)

let choose_DIV ()= 
	let wc = choose_bool() in
	let op1 = choose_op () in
	DIV (wc, op1)

let choose_IDIV () = 
	let wc = choose_bool() in
	let op1 = choose_op () in
	IDIV (wc, op1)

let choose_IMUL() = 
	let wc = choose_bool() in
	let opt_c = choose_bool() in
	let intopt_c = choose_bool() in
	let op1 = choose_op () in
	let op2 = if opt_c then (Some (choose_op())) else None in
	let i32 = if intopt_c then (Some (choose_word_int())) else None in
	IMUL (wc, op1, op2, i32)

let choose_IN () = 
	let wc = choose_bool() in
	let opt_c = choose_bool() in
	let port_num = if opt_c then (Some (choose_int8())) else None in
	IN (wc, port_num)

let choose_INC ()= 
	let wc = choose_bool() in
	let op1 = choose_op () in
	INC (wc, op1)

let choose_INS () = 
	INS (choose_bool())

let choose_INTn() = 
	let it = choose_word_int() in
	INTn it

let choose_INVLPG () = 
	let op1 = choose_op () in
	INVLPG op1

let choose_Jcc ()= 
	let ct = choose_condition_type () in
	let disp = choose_int8 () in
	Jcc (ct, disp)

let choose_JCXZ () = 
	let b = choose_int8() in
	JCXZ b

let choose_JMP () = 
	let near_c = choose_bool() in
	let abs_c = choose_bool() in
	let opt_c = choose_bool() in
	let op1 = choose_op() in
	let sel = if opt_c then (Some (choose_int16 ())) else None in
	JMP (near_c, abs_c, op1, sel)

let choose_LAR () = 
	let op1, op2 = choose_two_ops() in
	LAR (op1, op2)

let choose_LDS () = 
	let op1, op2 = choose_two_ops() in
	LDS (op1, op2)

let choose_LEA () = 
	let op1 = choose_op() in
	let op2 = choose_op() in
	LEA (op1, op2)

let choose_LES () = 
	let op1, op2 = choose_two_ops() in
	LES (op1, op2)

let choose_LFS () = 
	let op1, op2 = choose_two_ops() in
	LFS (op1, op2)

let choose_LGDT () = 
	let op1, op2 = choose_two_ops() in
	LGDT (op1)

let choose_LGS () = 
	let op1, op2 = choose_two_ops() in
	LGS (op1, op2)

let choose_LIDT () = 
	let op1 = choose_op() in
	LIDT (op1)

let choose_LLDT () = 
	let op1 = choose_op() in
	LLDT (op1)

let choose_LMSW () = 
	let op1 = choose_op() in
	LMSW (op1)

let choose_LODS () = 
	let b = choose_bool() in
	LODS (b)

let choose_LOOP () = 
	let b = choose_int8() in
	LOOP (b)

let choose_LOOPZ () = 
	let b = choose_int8() in
	LOOPZ (b)

let choose_LOOPNZ () = 
	let b = choose_int8() in
	LOOPNZ (b)

let choose_LSL () = 
	let op1, op2 = choose_two_ops() in
	LSL (op1, op2)

let choose_LSS () = 
	let op1, op2 = choose_two_ops() in
	LSS (op1, op2)

let choose_LTR () = 
	let op1 = choose_op() in
	LTR (op1)

let choose_MOV () = 
	let b = choose_bool() in
	let op1, op2 = choose_two_ops() in
	MOV (b, op1, op2)

let choose_MOVCR () = 
	let dir = choose_bool() in
	let cr = choose_control_reg() in
	let r = choose_reg() in
	MOVCR (dir, cr, r)

let choose_MOVDR() = 
	let dir = choose_bool() in
	let dr = choose_debug_reg() in
	let r = choose_reg() in
	MOVDR (dir, dr, r)

let choose_MOVSR () = 
	let d = choose_bool() in
	let sr = choose_segment_reg() in
	let op = choose_op() in
	MOVSR (d, sr, op)

let choose_MOVBE () =
	let (op1, op2) = choose_two_ops() in
	MOVBE (op1, op2)

let choose_MOVS () = 
	let w = choose_bool() in
	MOVS (w)

let choose_MOVSX () = 
	let w = choose_bool() in
	let (op1, op2) = choose_two_ops() in
	MOVSX (w, op1, op2)

let choose_MOVZX () = 
	let w = choose_bool() in
	let (op1, op2) = choose_two_ops() in
	MOVZX (w, op1, op2)
	
let choose_MUL () = 
	let w = choose_bool() in
	let op = choose_op() in
	MUL (w, op)

let choose_NEG () = 
	let w = choose_bool() in
	let op = choose_op() in
	NEG (w, op)

let choose_NOP () = 
	let op = choose_op() in
	NOP (op)

let choose_NOT () = 
	let w = choose_bool() in
	let op = choose_op() in
	NOT (w, op)

let choose_OR () = 
	let w = choose_bool() in
	let (op1, op2) = choose_two_ops() in
	OR (w, op1, op2)

let choose_OUT () = 
	let w  = choose_bool() in
	let p_opt = choose_bool() in
	let port_num = if p_opt then Some (choose_int8()) else None in
	OUT (w, port_num)

let choose_OUTS () = 
	let w = choose_bool() in
	OUTS (w)

let choose_POP () = 
	let op = choose_op() in
	POP (op)

let choose_POPSR () = 
	let sr = choose_segment_reg() in
	POPSR (sr)

let choose_PUSH() = 
	let w = choose_bool() in
	let op = choose_op() in
	PUSH(w, op)

let choose_PUSHSR() = 
	let sr = choose_segment_reg() in
	PUSHSR (sr)

let choose_RCL() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	RCL (w, op, ri)

let choose_RCR() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	RCR (w, op, ri)

let choose_RET () = 
	let same_seg = choose_bool() in
	let opt_c = choose_bool() in
	let dispp = if opt_c then Some (choose_int16()) else None in
	RET (same_seg, dispp)

let choose_ROL() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	ROL (w, op, ri)

let choose_ROR() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	ROR (w, op, ri)

let choose_SAR() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	SAR (w, op, ri)

let choose_SBB () = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	SBB (w, op1, op2)

let choose_SCAS () = 
	let w = choose_bool() in
	SCAS (w)

let choose_SETcc() = 
	let ct = choose_condition_type() in
	let op1 = choose_op () in
	SETcc (ct, op1)

let choose_SGDT () = 
	let op1 = choose_op() in
	SGDT (op1)

let choose_SHL() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	SHL (w, op, ri)

let choose_SHLD() = 
	let op = choose_op() in
	let reg = choose_reg() in
	let ri = choose_reg_or_immed() in
	SHLD (op, reg, ri)

let choose_SHR() = 
	let w = choose_bool() in
	let op = choose_op() in
	let ri = choose_reg_or_immed() in
	SHR (w, op, ri) 
	
let choose_SHRD() = 
	let op = choose_op() in
	let reg = choose_reg() in
	let ri = choose_reg_or_immed() in
	SHRD (op, reg, ri)

let choose_SIDT() = 
	let op = choose_op() in
	SIDT (op)

let choose_SLDT() = 
	let op = choose_op() in
	SLDT (op)

let choose_SMSW() = 
	let op = choose_op() in
	SMSW (op)

let choose_STOS() = 
	let w = choose_bool() in
	STOS (w)

let choose_STR() = 
	let op = choose_op() in
	STR (op)

let choose_SUB() = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	SUB (w, op1, op2)

let choose_TEST() = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	TEST (w, op1, op2)

let choose_VERR() = 
	let op = choose_op() in
	VERR (op)

let choose_VERW() = 
	let op = choose_op() in
	VERW (op)

let choose_XADD() = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	XADD (w, op1, op2)

let choose_XCHG() = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	XCHG (w, op1, op2)

let choose_XOR() = 
	let w = choose_bool() in
	let op1, op2 = choose_two_ops() in
	XOR (w, op1, op2)

(**End of general-purpose, start floating-point fuzz functions **)
let choose_FADD() = 
	let w = choose_bool() in
	let op1 = choose_fp_op() in
	FADD (w, op1)

let choose_FADDP() = 
	let op1 = choose_fp_op() in
	FADDP (op1)

let choose_FBLD() = 
	let op1 = choose_fp_op() in
	FBLD (op1)

let choose_FBSTP() = 
	let op1 = choose_fp_op() in
	FBSTP (op1)

let choose_FCOM() = 
	let opt_c = choose_bool() in
	let op = choose_fp_op() in
	let op1 = if opt_c then (Some op) else None in
	FCOM (op1)

let choose_FCOMP() = 
	let opt_c = choose_bool() in
	let op = choose_fp_op() in
	let op1 = if opt_c then (Some op) else None in
	FCOMP (op1)

let choose_FCOMIP() = 
	let op1 = choose_fp_op() in
	FCOMIP (op1)

let choose_FDIV() = 
	let op1 = choose_fp_op() in
	let op2 = choose_fp_op() in
	FDIV (op1, op2)

let choose_FDIVP() = 
	let op1 = choose_fp_op() in
	FDIVP (op1)

let choose_FDIVR() = 
	let op1 = choose_fp_op() in
	let op2 = choose_fp_op() in
	FDIVR (op1, op2)

let choose_FDIVRP() = 
	let op1 = choose_fp_op() in
	FDIVRP (op1)

let choose_FFREE() = 
	let op1 = choose_fp_op() in
	FFREE (op1)

let choose_FIADD() = 
	let op1 = choose_fp_op() in
	FIADD (op1)

let choose_FICOM() = 
	let op1 = choose_fp_op() in
	FICOM (op1)

let choose_FICOMP() = 
	let op1 = choose_fp_op() in
	FICOMP (op1)

let choose_FIDIV() = 
	let op1 = choose_fp_op() in
	FIDIV (op1)

let choose_FIDIVR() = 
	let op1 = choose_fp_op() in
	FIDIVR (op1)

let choose_FILD() = 
	let op1 = choose_fp_op() in
	FILD (op1)

let choose_FIMUL() = 
	let op1 = choose_fp_op() in
	FIMUL (op1)

let choose_FIST() = 
	let op1 = choose_fp_op() in
	FIST (op1)

let choose_FISTP() = 
	let op1 = choose_fp_op() in
	FISTP (op1)

let choose_FISUB() = 
	let op1 = choose_fp_op() in
	FISUB (op1)

let choose_FISUBR() = 
	let op1 = choose_fp_op() in
	FISUBR (op1)

let choose_FLD() = 
	let op1 = choose_fp_op() in
	FLD (op1)

let choose_FLDCW() = 
	let op1 = choose_fp_op() in
	FLDCW (op1)

let choose_FLDENV() = 
	let op1 = choose_fp_op() in
	FLDENV (op1)

let choose_FMUL() = 
	let d = choose_bool() in
	let op1 = choose_fp_op() in
	FMUL(d, op1)

let choose_FMULP() = 
	let op1 = choose_fp_op() in
	FMULP (op1)

let choose_FNSAVE() = 
	let op1 = choose_fp_op() in
	FNSAVE (op1)

let choose_FNSTCW() = 
	let op1 = choose_fp_op() in
	FNSTCW (op1)

(*let choose_FNSTSW() = 
	let opt_c = choose_bool() in
	let op = choose_fp_op() in
	let op1 = if opt_c then (Some op) else None in
	FNSTSW (op1) *)

let choose_FRSTOR() = 
	let op1 = choose_fp_op() in
	FRSTOR (op1)

let choose_FST() = 
	let op1 = choose_fp_op() in
	FST (op1)

let choose_FSTENV() = 
	let op1 = choose_fp_op() in
	FSTENV (op1)

let choose_FSTP() = 
	let op1 = choose_fp_op() in
	FSTP (op1)

let choose_FSUB() = 
	let op1 = choose_fp_op() in
	let op2 = choose_fp_op() in
	FSUB (op1, op2)

let choose_FSUBR() = 
	let op1 = choose_fp_op() in
	let op2 = choose_fp_op() in
	FSUBR (op1, op2)

let choose_FSUBP() = 
	let op1 = choose_fp_op() in
	FSUBP (op1)

let choose_FSUBRP() = 
	let op1 = choose_fp_op() in
	FSUBRP (op1)

let choose_FUCOM() = 
	let op1 = choose_fp_op() in
	FUCOM (op1)

let choose_FUCOMP() = 
	let op1 = choose_fp_op() in
	FUCOMP (op1)

let choose_FUCOMI() = 
	let op1 = choose_fp_op() in
	FUCOMI (op1)

let choose_FUCOMIP() = 
	let op1 = choose_fp_op() in
	FUCOMIP (op1)

let choose_FXCH() = 
	let op1 = choose_fp_op() in
	FXCH (op1)

(** returns a random gp instruction  **)
let choose_gp_instr lb ub  = 
   let diff = ub - lb in
   match (lb + Random.int diff) with 
      0 -> choose_ADC()
    | 1 -> choose_ADD ()
    | 2 -> choose_AND ()
    | 3 -> AAA
    | 4 -> AAD
    | 5 -> AAM
    | 6 -> AAS
    | 7 -> choose_ARPL ()
    | 8 -> choose_BOUND ()
    | 9 -> choose_BSF ()
    | 10 -> choose_BSR ()
    | 11 -> choose_BSWAP ()
    | 12 -> choose_BT ()
    | 13 -> choose_BTC ()
    | 14 -> choose_BTR ()
    | 15 -> choose_BTS ()
    | 16 -> choose_CALL ()
    | 17 -> CDQ
    | 18 -> CLC
    | 19 -> CLD
    | 20 -> CLI
    | 21 -> CLTS
    | 22 -> CMC
    | 23 -> choose_CMOVcc ()
    | 24 -> choose_CMP ()
    | 25 -> choose_CMPS ()
    | 26 -> choose_CMPXCHG ()
    | 27 -> CWDE
    | 28 -> DAA
    | 29 -> DAS
    | 30 -> choose_DEC ()
    | 31 -> choose_DIV ()
    | 32 -> CPUID
    | 33 -> CWDE
    | 34 -> HLT
    | 35 -> choose_IDIV ()
    | 36 -> choose_IMUL ()
    | 37 -> choose_IN () 
    | 38 -> choose_INC ()
    | 39 -> choose_INS ()
    | 40 -> choose_INTn ()
    | 41 -> INT
    | 42 -> INTO
    | 43 -> INVD
    | 44 -> choose_INVLPG ()
    | 45 -> IRET
    | 46 -> choose_Jcc ()
    | 47 -> choose_JCXZ ()
    | 48 -> choose_JMP ()
    | 49 -> LAHF 
    | 50 -> choose_LAR ()
    | 51 -> choose_LDS ()
    | 52 -> choose_LEA ()
    | 53 -> LEAVE
    | 54 -> choose_LES ()
    | 56 -> choose_LFS ()
    | 57 -> choose_LGDT ()
    | 58 -> choose_LGS ()
    | 59 -> choose_LIDT () 
    | 60 -> choose_LLDT ()
    | 61 -> choose_LMSW ()
    | 62 -> choose_LODS ()
    | 63 -> choose_LOOP ()
    | 64 -> choose_LOOPNZ ()
    | 65 -> choose_LSL ()
    | 66 -> choose_LSS ()
    | 67 -> choose_LTR ()
    | 68 -> choose_MOV ()
    | 69 -> choose_MOVCR ()
    | 70 -> choose_MOVDR ()
    | 71 -> choose_MOVSR ()
    | 72 -> choose_MOVBE ()
    | 73 -> choose_MOVS ()
    | 74 -> choose_MOVSX ()
    | 75 -> choose_MOVZX ()
    | 76 -> choose_MUL ()
    | 77 -> choose_NEG ()
    | 78 -> choose_NOP ()
    | 79 -> choose_NOT ()
    | 80 -> choose_OR () 
    | 81 -> choose_OUT () 
    | 82 -> choose_OUTS () 
    | 83 -> choose_POP ()
    | 84 -> choose_POPSR ()
    | 85 -> POPA
    | 86 -> POPF
    | 87 -> choose_PUSH ()
    | 88 -> choose_PUSHSR ()
    | 89 -> PUSHA
    | 90 -> PUSHF
    | 91 -> choose_RCL ()
    | 92 -> choose_RCR ()
    | 93 -> RDMSR
    | 94 -> RDPMC
    | 95 -> RDTSC
    | 96 -> RDTSCP
    | 97 -> choose_RET ()
    | 98 -> choose_ROL ()
    | 99 -> choose_ROR ()
    | 100 -> RSM
    | 101 -> SAHF
    | 102 -> choose_SAR ()
    | 103 -> choose_SBB () 
    | 104 -> choose_SCAS () 
    | 105 -> choose_SETcc ()
    | 106 -> choose_SGDT ()
    | 107 -> choose_SHL ()
    | 108 -> choose_SHLD ()
    | 109 -> choose_SHR ()
    | 110 -> choose_SHRD () 
    | 111 -> choose_SIDT ()
    | 112 -> choose_SLDT ()
    | 113 -> choose_SMSW ()
    | 115 -> STC
    | 116 -> STD
    | 117 -> STI
    | 118 -> choose_STOS ()
    | 119 -> choose_STR ()
    | 120 -> choose_SUB ()
    | 121 -> choose_TEST ()
    | 122 -> UD2
    | 123 -> choose_VERR ()
    | 124 -> choose_VERW ()
    | 125 -> WBINVD
    | 126 -> WRMSR
    | 127 -> choose_XADD ()
    | 128 -> choose_XCHG ()
    | 129 -> XLAT
    | 130 -> choose_XOR ()
    | _ -> choose_NOP () 

(**Returns a randomly generated floating-point instuction**)
let choose_fp_instr lb ub = 
    let diff = ub - lb in
    match (lb + Random.int diff) with 
    | 0 -> F2XM1
    | 1 -> FABS
    | 2 -> choose_FADD()
    | 3 -> choose_FADDP()
    | 4 -> choose_FBLD()
    | 5 -> choose_FBSTP()
    | 6 -> FCHS
    | 7 -> FCLEX
    | 8 -> choose_FCOM()
    | 9 -> choose_FCOMP()
    | 10 -> FCOMPP
    | 11 -> choose_FCOMIP()
    | 12 -> FCOS
    | 13 -> FDECSTP
    | 14 -> choose_FDIV() 
    | 15 -> choose_FDIVP()
    | 16 -> choose_FDIVR()
    | 17 -> choose_FDIVRP()
    | 18 -> choose_FFREE()
    | 19 -> choose_FIADD()
    | 20 -> choose_FICOM()
    | 21 -> choose_FICOMP()
    | 22 -> choose_FIDIV()
    | 23 -> choose_FIDIVR()
    | 24 -> choose_FILD()
    | 25 -> choose_FIMUL()
    | 26 -> FINCSTP
    | 27 -> FINIT
    | 28 -> choose_FIST()
    | 29 -> choose_FISTP()
    | 30 -> choose_FISUB()
    | 31 -> choose_FISUBR()
    | 32 -> choose_FLD()
    | 33 -> FLD1
    | 34 -> choose_FLDCW()
    | 35 -> choose_FLDENV()
    | 36 -> FLDL2E
    | 37 -> FLDL2T
    | 38 -> FLDLG2
    | 39 -> FLDLN2
    | 40 -> FLDPI
    | 41 -> FLDZ
    | 42 -> choose_FMUL()
    | 43 -> choose_FMULP()
    | 44 -> FNOP
    | 45 -> choose_FNSAVE()
    | 46 -> choose_FNSTCW()
   (* | 47 -> choose_FNSTSW() *)
    | 48 -> FPATAN
    | 49 -> FPREM
    | 50 -> FPREM1
    | 51 -> FPTAN
    | 52 -> FRNDINT
    | 53 -> choose_FRSTOR()
    | 54 -> FSCALE
    | 55 -> FSIN
    | 56 -> FSINCOS
    | 57 -> FSQRT
    | 58 -> choose_FST()
    | 59 -> choose_FSTENV()
    | 60 -> choose_FSTP()
    | 61 -> choose_FSUB()
    | 62 -> choose_FSUBP()
    | 63 -> choose_FSUBR()
    | 64 -> choose_FSUBRP()
    | 65 -> FTST
    | 66 -> choose_FUCOM()
    | 67 -> choose_FUCOMP()
    | 68 -> FUCOMPP
    | 69 -> choose_FUCOMI()
    | 70 -> choose_FUCOMIP()
    | 71 -> FXAM
    | 72 -> choose_FXCH()
    | 73 -> FXTRACT
    | 74 -> FYL2X
    | 75 -> FYL2XP1
    | _ ->  FNOP 
    
let rec fuzz_gp lb ub n = 
  match n with 
    | 0 -> ()
    | x ->   
     let prefix = choose_prefix () in
     let instr = choose_gp_instr lb ub in 
     
     F.printf "------------\nTesting %a\n"
     pp_prefix_instr (emptyPrefix,instr);
     
     test_encode_decode_instr (*prefix*) emptyPrefix instr;
     Printf.printf "------------\n";
     F.print_newline (); (* flush *)
     fuzz_gp lb ub (x - 1)
  ;;

let rec fuzz_fp lb ub n = 
  match n with
    | 0 -> ()
    | x ->   
     let instr = choose_fp_instr lb ub in 
     
     F.printf "------------\nTesting %a\n"
     pp_prefix_instr (emptyPrefix,instr);
     
     test_encode_decode_instr (*prefix*) emptyPrefix instr;
     Printf.printf "------------\n";
     F.print_newline (); (* flush *)
     fuzz_fp lb ub (x - 1)
  ;;

Random.self_init();;

let some_lb = ref 0
let some_ub = ref 100
let some_n = ref 500
let some_type = ref "gp"

let usage = "usage: " ^ Sys.argv.(0) ^ " [type string] [lb int] [ub int] [n int]"

let speclist = [
    ("-type", Arg.String (fun d -> some_type := d), ": specify instruction set to fuzz (gp or fp)"); 
    ("-lb", Arg.Int (fun a -> some_lb := a), ": set the lowerbound");
    ("-ub", Arg.Int (fun b -> some_ub := b), ": set the upperbound");
    ("-n", Arg.Int (fun c -> some_n := c), ": set number of runs");
   ]

(*let check = check that command line args are ok

*)

let gp_or_fp() = 
	match !some_type with 
	| "gp" -> fuzz_gp !some_lb !some_ub !some_n
	| "fp" -> fuzz_fp !some_lb !some_ub !some_n
	| _ -> fuzz_fp !some_lb !some_ub !some_n

let main () = 
  print_string("running syntaxfuzzer:\n");

  Arg.parse 
	speclist 
	(fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
	usage;

  gp_or_fp();

  let s_fl = float_of_int !succ_count in
  let e_d_fl = float_of_int !enc_dec_fails_count in
  let d_fl = float_of_int !dec_fails_count in
  let e_fl = float_of_int !enc_fails_count in
  let n_fl = float_of_int !some_n in

  Printf.printf "Percentage successful:                  %f\n" (s_fl /. n_fl);
  Printf.printf "Percentage decode step failures:        %f\n" (d_fl /. n_fl);
  Printf.printf "Percentage encode-decode loop failures: %f\n" (e_d_fl /. n_fl);
  Printf.printf "Percentage encode failures:             %f\n" (e_fl /. n_fl);;
  

main();;