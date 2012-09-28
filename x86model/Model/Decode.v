(* Copyright (c) 2011. Greg Morrisett, Gang Tan, Joseph Tassarotti, 
   Jean-Baptiste Tristan, and Edward Gan.

   This file is part of RockSalt.

   This file is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.
*)

(* This file provides simple bit-level parsing combinators for disassembling
 * Intel IA32 (x86) 32-bit binaries. *)
Require Coqlib.
Require Import Coq.Init.Logic.
Require Import Bool.
Require Import List.
Require Import String.
Require Import Maps.
Require Import Ascii.
Require Import ZArith.
Require Import Eqdep.
Require Import Parser.
Unset Automatic Introduction.
Set Implicit Arguments.
Local Open Scope Z_scope.


Require ExtrOcamlString.
Require ExtrOcamlNatBigInt.


(* a module for generating the parser for x86 instructions *)
Module X86_PARSER_ARG.
  Require Import X86Syntax.
  Require Import Bits.
  
  Definition char_p : Set := bool.
  Definition char_eq : forall (c1 c2:char_p), {c1=c2}+{c1<>c2} := bool_dec.
  Inductive type : Set := 
  | Int_t : type
  | Register_t : type
  | Byte_t : type
  | Half_t : type
  | Word_t : type
  | Double_Word_t : type
  | Ten_Byte_t : type
  | Scale_t : type
  | Condition_t : type
  | Address_t : type
  | Operand_t : type
  | Fpu_Register_t : type
  | Fp_Status_Register_t : type
  | Fp_Control_Register_t : type 
  | Fp_TagWord_Register_t : type
  | Fp_LastOperPtr_Register_t : type
  | Fp_LastInstrPtr_Register_t : type
  | Fp_Opcode_Register_t : type
  | Fpu_TagWords_t : type
  | Fp_Debug_Register_t : type
  | Fp_Operand_t : type 
  | MMX_Granularity_t : type
  | MMX_Register_t : type
  | MMX_Operand_t : type
  | SSE_Register_t : type
  | SSE_Operand_t : type
  | Instruction_t : type
  | Control_Register_t : type
  | Debug_Register_t : type
  | Segment_Register_t : type
  | Lock_or_Rep_t : type
  | Bool_t : type
  | Prefix_t : type
  | Option_t (t: type) : type
  (* Need pairs at this level if I want to have options of pairs*)
  | Pair_t (t1 t2: type) : type. 

  Definition tipe := type.
  Definition tipe_eq : forall (t1 t2:tipe), {t1=t2} + {t1<>t2}.
    intros ; decide equality.
  Defined.

  Fixpoint tipe_m (t:tipe) := 
    match t with 
      | Int_t => Z
      | Register_t => register
      | Byte_t => int8
      | Half_t => int16
      | Word_t => int32
      | Double_Word_t => int64
      | Ten_Byte_t => int80
      | Scale_t => scale
      | Condition_t => condition_type
      | Address_t => address
      | Operand_t => operand
      | Fpu_Register_t => int3
      | Fp_Status_Register_t => fp_status_register
      | Fp_Control_Register_t => fp_control_register
      | Fp_TagWord_Register_t => fp_tagWord_register
      | Fp_LastOperPtr_Register_t => fp_lastOperandPtr_register
      | Fp_LastInstrPtr_Register_t => fp_lastInstrPtr_register
      | Fp_Opcode_Register_t => fp_opcode_register
      | Fpu_TagWords_t => fpu_tagWords
      | Fp_Debug_Register_t => fp_debug_register
      | Fp_Operand_t => fp_operand  
      | MMX_Granularity_t => mmx_granularity
      | MMX_Register_t => mmx_register
      | MMX_Operand_t => mmx_operand
      | SSE_Register_t => sse_register
      | SSE_Operand_t => sse_operand
      | Instruction_t => instr
      | Control_Register_t => control_register
      | Debug_Register_t => debug_register
      | Segment_Register_t => segment_register
      | Lock_or_Rep_t => lock_or_rep
      | Bool_t => bool
      | Prefix_t => prefix
      | Option_t t => option (tipe_m t)
      | Pair_t t1 t2 => ((tipe_m t1) * (tipe_m t2))%type
    end.
End X86_PARSER_ARG.

Module X86_PARSER.
  Module X86_BASE_PARSER := Parser.Parser(X86_PARSER_ARG).
  Require Import X86Syntax.
  Require Import Bits.
  Import X86_PARSER_ARG.
  Import X86_BASE_PARSER.

  Definition option_t x := tipe_t (Option_t x).
  Definition int_t := tipe_t Int_t.
  Definition register_t := tipe_t Register_t.
  Definition byte_t := tipe_t Byte_t.
  Definition half_t := tipe_t Half_t.
  Definition word_t := tipe_t Word_t.
  Definition double_word_t := tipe_t Double_Word_t.
  Definition ten_byte_t := tipe_t Ten_Byte_t.
  Definition scale_t := tipe_t Scale_t.
  Definition condition_t := tipe_t Condition_t.
  Definition fpu_register_t := tipe_t Fpu_Register_t.
  Definition fp_status_register_t := tipe_t Fp_Status_Register_t.
  Definition fp_control_register_t := tipe_t Fp_Control_Register_t. 
  Definition fp_tagWord_register_t := tipe_t Fp_TagWord_Register_t.
  Definition fp_lastOperPtr_register_t := tipe_t Fp_LastOperPtr_Register_t.
  Definition fp_lastInstrPtr_register_t := tipe_t Fp_LastInstrPtr_Register_t.
  Definition fp_opcode_register_t := tipe_t Fp_Opcode_Register_t.
  Definition fpu_tagWords_t := tipe_t Fpu_TagWords_t.
  Definition fp_debug_register_t := tipe_t Fp_Debug_Register_t.
  Definition mmx_granularity_t := tipe_t MMX_Granularity_t.
  Definition mmx_operand_t := tipe_t MMX_Operand_t.
  Definition mmx_register_t := tipe_t MMX_Register_t.
  Definition sse_operand_t := tipe_t SSE_Operand_t.
  Definition sse_register_t := tipe_t SSE_Register_t.
  Definition address_t := tipe_t Address_t.
  Definition operand_t := tipe_t Operand_t.
  Definition fp_operand_t := tipe_t Fp_Operand_t.  
  Definition instruction_t := tipe_t Instruction_t.
  Definition control_register_t := tipe_t Control_Register_t.
  Definition debug_register_t := tipe_t Debug_Register_t.
  Definition segment_register_t := tipe_t Segment_Register_t.
  Definition lock_or_rep_t := tipe_t Lock_or_Rep_t.
  Definition bool_t := tipe_t Bool_t.
  Definition prefix_t := tipe_t Prefix_t.
  (* combinators for building parsers *)
  Definition bit(x:bool) : parser char_t := Char_p x.
  Definition never t : parser t := Zero_p t.
  Definition always t (x:result_m t) : parser t := @Map_p unit_t t (fun (_:unit) => x) Eps_p.
  Definition alt t (p1 p2:parser t) : parser t := Alt_p p1 p2.
  Definition alts t (ps: list (parser t)) : parser t := List.fold_right (@alt t) (@never t) ps.
  Definition map t1 t2 (p:parser t1) (f:result_m t1 -> result_m t2) : parser t2 := 
    @Map_p t1 t2 f p.
  Implicit Arguments map [t1 t2].
  Definition seq t1 t2 (p1:parser t1) (p2:parser t2) : parser (pair_t t1 t2) := Cat_p p1 p2.
  Definition cons t (pair : result_m (pair_t t (list_t t))) : result_m (list_t t) := 
    (fst pair)::(snd pair).
  Definition seqs t (ps:list (parser t)) : parser (list_t t) := 
    List.fold_right (fun p1 p2 => map (seq p1 p2) (@cons t)) 
      (@always (list_t t) (@nil (result_m t))) ps.

  Fixpoint string_to_bool_list (s:string) : list bool := 
    match s with
      | EmptyString => nil
      | String a s => 
        (if ascii_dec a "0"%char then false else true)::(string_to_bool_list s)
    end.

  Fixpoint bits_n (n:nat) : result := 
    match n with 
      | 0%nat => unit_t
      | S n => pair_t char_t (bits_n n)
    end.
  Fixpoint field'(n:nat) : parser (bits_n n) := 
    match n with 
      | 0%nat => Eps_p
      | S n => Cat_p Any_p (field' n)
    end.
  Fixpoint bits2Z(n:nat)(a:Z) : result_m (bits_n n) -> result_m int_t := 
    match n with 
      | 0%nat => fun _ => a
      | S n => fun p => bits2Z n (2*a + (if (fst p) then 1 else 0)) (snd p)
    end.
  Definition bits2int(n:nat)(bs:result_m (bits_n n)) : result_m int_t := bits2Z n 0 bs.
  Fixpoint bits (x:string) : parser (bits_n (String.length x)) := 
    match x with 
      | EmptyString => Eps_p
      | String c s => 
        (Cat_p (Char_p (if ascii_dec c "0"%char then false else true)) (bits s))
    end.

  (* notation for building parsers *)
  Infix "|+|" := alt (right associativity, at level 80).
  Infix "$" := seq (right associativity, at level 70).
  Infix "@" := map (right associativity, at level 75).
  Notation "e %% t" := (e : result_m t) (at level 80).
  Definition bitsleft t (s:string)(p:parser t) : parser t := 
    bits s $ p @ (@snd _ _).
  Infix "$$" := bitsleft (right associativity, at level 70).

  Definition anybit : parser char_t := Any_p.
  Definition field(n:nat) := (field' n) @ (bits2int n).
  Definition reg := (field 3) @ (Z_to_register : _ -> result_m register_t). 
  Definition fpu_reg := (field 3) @ (@Word.repr 2 :_ -> result_m fpu_register_t).
  Definition mmx_reg := (field 3) @ (Z_to_mmx_register : _ -> result_m mmx_register_t).
  Definition sse_reg := (field 3) @ (Z_to_sse_register : _ -> result_m sse_register_t).
  Definition byte := (field 8) @ (@Word.repr 7 : _ -> result_m byte_t).
 (* Definition halfword := (field 16) @ (@Word.repr 15 : _ -> result_m half_t).
  Definition word := (field 32) @ (@Word.repr 31 : _ -> result_m word_t). *)
  Definition halfword := (byte $ byte) @ ((fun p =>
      let b0 := Word.repr (Word.unsigned (fst p)) in
      let b1 := Word.repr (Word.unsigned (snd p)) in
        Word.or (Word.shl b1 (Word.repr 8)) b0): _ -> result_m half_t).
  Definition word := (byte $ byte $ byte $ byte) @
    ((fun p => 
        let b0 := zero_extend8_32 (fst p) in
        let b1 := zero_extend8_32 (fst (snd p)) in
        let b2 := zero_extend8_32 (fst (snd (snd p))) in
        let b3 := zero_extend8_32 (snd (snd (snd p))) in
         let w1 := Word.shl b1 (Word.repr 8) in
         let w2 := Word.shl b2 (Word.repr 16) in
         let w3 := Word.shl b3 (Word.repr 24) in
          Word.or w3 (Word.or w2 (Word.or w1 b0)))
    : _ -> result_m word_t).

  Definition scale_p := (field 2) @ (Z_to_scale : _ -> result_m scale_t).
  Definition tttn := (field 4) @ (Z_to_condition_type : _ -> result_m condition_t).

  (* This is used in a strange edge-case for modrm parsing. See the
     footnotes on p37 of the manual in the repo This is a case where I
     think intersections/complements would be nice operators *)

  (* JGM: we can handle this in the semantic action instead of the parser, 
     so I replaced si, which used this and another pattern for [bits "100"]
     to the simpler case below -- helps to avoid some explosions in the 
     definitions. *)
  Definition reg_no_esp : parser register_t :=
     (bits "000" |+| bits "001" |+| bits "010" |+|
     bits "011" |+| (* bits "100" <- this is esp *)  bits "101" |+|
     bits "110" |+| bits "111") @ 
       ((fun bs => Z_to_register (bits2int 3 bs)) : _ -> result_m register_t).

  Definition reg_no_ebp : parser register_t :=
     (bits "000" |+| bits "001" |+| bits "010" |+|
     bits "011" |+|  bits "100"  (* |+| bits "101" <- this is ebp *) |+|
     bits "110" |+| bits "111") @ 
       ((fun bs => Z_to_register (bits2int 3 bs)) : _ -> result_m register_t).

  Definition si := 
    (scale_p $ reg) @ (fun p => match snd p with 
                                  | ESP => None
                                  | _ => Some p
                                end %% option_t (Pair_t Scale_t Register_t)).

  Definition sib := si $ reg.

  (* These next 4 parsers are used in the definition of the mod/rm parser *)
  Definition rm00 : parser address_t := 
    (     bits "000" 
      |+| bits "001" 
      |+| bits "010" 
      |+| bits "011" 
      |+| bits "110"
      |+| bits "111" ) @ 
          (fun bs => (mkAddress (Word.repr 0) 
            (Some (Z_to_register(bits2int 3 bs))) None) %% address_t)
      |+| bits "100" $ si $ reg_no_ebp @ 
          (fun p => match p with
                      | (_,(si,base)) => 
                        (mkAddress (Word.repr 0) (Some base) si)
                    end : result_m address_t)     
      |+| bits "100" $ si $ bits "101" $ word @
          (fun p => match p with
                      | (_,(si,(_, disp))) => 
                        (mkAddress disp (None) si)
                    end : result_m address_t)
      |+| bits "101" $ word @
          (fun p => match p with 
                      | (_, disp) => 
                        (mkAddress disp None None)
                    end %% address_t).  

  Definition rm01 : parser address_t := 
    ((    bits "000" 
      |+| bits "001" 
      |+| bits "010" 
      |+| bits "011"
      |+| bits "101" 
      |+| bits "110"
      |+| bits "111") $ byte) @ 
          (fun p => 
            match p with 
              | (bs, disp) =>
                (mkAddress (sign_extend8_32 disp) 
                  (Some (Z_to_register(bits2int 3 bs))) None)
            end %% address_t)
      |+| bits "100" $ sib $ byte @ 
          (fun p => 
            match p with
              | (_,((si,base),disp)) => 
                (mkAddress (sign_extend8_32 disp) (Some base) (si))
            end %% address_t).

  Definition rm10 : parser address_t := 
    ((    bits "000" 
      |+| bits "001" 
      |+| bits "010" 
      |+| bits "011"
      |+| bits "101" 
      |+| bits "110"
      |+| bits "111") $ word) @ 
          (fun p => 
            match p with 
              | (bs, disp) =>
                (mkAddress disp (Some (Z_to_register(bits2int 3 bs))) None)
            end %% address_t)
      |+|  bits "100" $ sib $ word @ 
          (fun p => 
            match p with
              | (_,((si,base),disp)) => 
                (mkAddress disp (Some base) si)
            end %% address_t).

  Definition rm11 : parser operand_t := reg @ (fun x => Reg_op x : result_m operand_t).

  Definition rm11_sse : parser sse_operand_t := reg @ (fun x => SSE_GP_Reg_op x : result_m sse_operand_t).

  Definition modrm : parser (pair_t operand_t operand_t) := 
    (     ("00" $$ reg $ rm00) 
      |+| ("01" $$ reg $ rm01)
      |+| ("10" $$ reg $ rm10)) @
            (fun p => match p with
                      | (r,addr) => (Reg_op r, Address_op addr)
                      end %% (pair_t operand_t operand_t))
   |+| ("11" $$ reg $ rm11) @ 
          (fun p => match p with 
                      | (r, op) => (Reg_op r, op)
                    end %% (pair_t operand_t operand_t)).

  Definition modrm_mmx : parser (pair_t mmx_operand_t mmx_operand_t) := 
    (     ("00" $$ mmx_reg $ rm00)
      |+| ("01" $$ mmx_reg $ rm01)
      |+| ("10" $$ mmx_reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (MMX_Reg_op r, MMX_Addr_op addr)
                      end %% (pair_t mmx_operand_t mmx_operand_t))
      |+| ("11" $$ mmx_reg $ mmx_reg) @ 
          (fun p => match p with 
                      | (r1, r2) => (MMX_Reg_op r1, MMX_Reg_op r2)
                    end %% (pair_t mmx_operand_t mmx_operand_t)).

   (* mod xmmreg r/m in manual*)
   Definition modrm_xmm : parser (pair_t sse_operand_t sse_operand_t) := 
    (     ("00" $$ sse_reg $ rm00)
      |+| ("01" $$ sse_reg $ rm01)
      |+| ("10" $$ sse_reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (SSE_XMM_Reg_op r, SSE_Addr_op addr)
                      end %% (pair_t sse_operand_t sse_operand_t))
      |+| ("11" $$ sse_reg $ sse_reg) @ 
          (fun p => match p with 
                      | (r1, r2) => (SSE_XMM_Reg_op r1, SSE_XMM_Reg_op r2)
                    end %% (pair_t sse_operand_t sse_operand_t)).

   (* mod mmreg r/m (no x) in manual; this uses mmx regs in sse instrs *)
   Definition modrm_mm : parser (pair_t sse_operand_t sse_operand_t) := 
    (     ("00" $$ mmx_reg $ rm00)
      |+| ("01" $$ mmx_reg $ rm01)
      |+| ("10" $$ mmx_reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (SSE_MM_Reg_op r, SSE_Addr_op addr)
                      end %% (pair_t sse_operand_t sse_operand_t))
      |+| ("11" $$ mmx_reg $ mmx_reg) @ 
          (fun p => match p with 
                      | (r1, r2) => (SSE_MM_Reg_op r1, SSE_MM_Reg_op r2)
                    end %% (pair_t sse_operand_t sse_operand_t)).

  (* same as modrm but disallows the register case; 
     that is, the second operand must be a mem operand*)
  Definition modrm_noreg :=
  (     ("00" $$ reg $ rm00)
    |+| ("01" $$ reg $ rm01)
    |+| ("10" $$ reg $ rm10)) @ 
           (fun p => match p with
                      | (r,addr) => (r, Address_op addr)
                     end %% (pair_t register_t operand_t)).

   (* modrm_xmm, but disallows the register case *)
   Definition modrm_xmm_noreg : parser (pair_t sse_operand_t sse_operand_t) := 
    (    ("00" $$ sse_reg $ rm00)
      |+| ("01" $$ sse_reg $ rm01)
      |+| ("10" $$ sse_reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (SSE_XMM_Reg_op r, SSE_Addr_op addr)
                      end %% (pair_t sse_operand_t sse_operand_t)).
   
   Definition modrm_xmm_r32_noreg : parser (pair_t sse_operand_t sse_operand_t) := 
    (    ("00" $$ reg $ rm00)
      |+| ("01" $$ reg $ rm01)
      |+| ("10" $$ reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (SSE_GP_Reg_op r, SSE_Addr_op addr)
                      end %% (pair_t sse_operand_t sse_operand_t)).

   Definition modrm_mm_noreg : parser (pair_t sse_operand_t sse_operand_t) := 
    (     ("00" $$ mmx_reg $ rm00)
      |+| ("01" $$ mmx_reg $ rm01)
      |+| ("10" $$ mmx_reg $ rm10)) @ 
            (fun p => match p with 
                      | (r, addr) => (SSE_MM_Reg_op r, SSE_Addr_op addr)
                      end %% (pair_t sse_operand_t sse_operand_t)).

  (* Similar to mod/rm parser except that the register field is fixed to a
   * particular bit-pattern, and the pattern starting with "11" is excluded. *)
  Definition ext_op_modrm(bs:string) : parser operand_t := 
    (      (bits "00" $ bits bs $ rm00)
     |+|   (bits "01" $ bits bs $ rm01)
     |+|   (bits "10" $ bits bs $ rm10) ) @
           (fun p => match p with 
                       | (_,(_,addr)) => Address_op addr
                     end %% operand_t).
  
  (*mod^A "bbb" mem in manual for SSE instructions*)
  Definition ext_op_modrm_sse(bs:string) : parser sse_operand_t := 
    (      (bits "00" $ bits bs $ rm00)
     |+|   (bits "01" $ bits bs $ rm01)
     |+|   (bits "10" $ bits bs $ rm10) ) @
           (fun p => match p with 
                       | (_,(_,addr)) => SSE_Addr_op addr
                     end %% sse_operand_t).

  Definition ext_op_modrm2(bs:string) : parser operand_t :=
    (     ("00" $$ bits bs $ rm00) 
      |+| ("01" $$ bits bs $ rm01)
      |+| ("10" $$ bits bs $ rm10)) @
            (fun p => match p with
                      | (_,addr) => Address_op addr
                      end %% operand_t)
   |+| ("11" $$ bits bs $ rm11) @ 
          (fun p => match p with 
                      | (_, op) => op
                    end %% operand_t).

  (* address operands that hold 32-bit floats *)
  Definition ext_op_modrm_FPM32(bs:string) : parser fp_operand_t := 
    (      (bits "00" $ bits bs $ rm00)
     |+|   (bits "01" $ bits bs $ rm01)
     |+|   (bits "10" $ bits bs $ rm10) ) @
           (fun p => match p with 
                       | (_,(_,addr)) => FPM32_op addr
                     end %% fp_operand_t).

  Definition ext_op_modrm_FPM64 (bs:string) : parser fp_operand_t := 
    (      (bits "00" $ bits bs $ rm00)
     |+|   (bits "01" $ bits bs $ rm01)
     |+|   (bits "10" $ bits bs $ rm10) ) @
           (fun p => match p with 
                       | (_,(_,addr)) => FPM64_op addr
                     end %% fp_operand_t).

  Definition ext_op_modrm_FPM80 (bs:string) : parser fp_operand_t := 
    (      (bits "00" $ bits bs $ rm00)
     |+|   (bits "01" $ bits bs $ rm01)
     |+|   (bits "10" $ bits bs $ rm10) ) @
           (fun p => match p with 
                       | (_,(_,addr)) => FPM80_op addr
                     end %% fp_operand_t).

  (* Parsers for the individual instructions *)
  Definition AAA_p := bits "00110111" @ (fun _ => AAA %% instruction_t).
  Definition AAD_p := bits "1101010100001010" @ (fun _ => AAD %% instruction_t).
  Definition AAM_p := bits "1101010000001010" @ (fun _ => AAM %% instruction_t).
  Definition AAS_p := bits "00111111" @ (fun _ => AAS %% instruction_t).

  (* The parsing for ADC, ADD, AND, CMP, OR, SBB, SUB, and XOR can be shared *)

  Definition imm_op (opsize_override: bool) : parser operand_t :=
    match opsize_override with
      | false => word @ (fun w => Imm_op w %% operand_t)
      | true => halfword @ (fun w => Imm_op (sign_extend16_32 w) %% operand_t)
    end.
      
  Definition logic_or_arith_p (opsize_override: bool)
    (op1 : string) (* first 5 bits for most cases *)
    (op2 : string) (* when first 5 bits are 10000, the next byte has 3 bits
                      that determine the opcode *)
    (InstCon : bool->operand->operand->instr) (* instruction constructor *)
    : parser instruction_t
    :=
  (* register/memory to register and vice versa -- the d bit specifies
   * the direction. *)
  op1 $$ "0" $$ anybit $ anybit $ modrm @
    (fun p => match p with 
                | (d, (w, (op1, op2))) => 
                  if d then InstCon w op1 op2 else InstCon w op2 op1
              end %% instruction_t)
  |+|
  (* sign extend immediate byte to register *)
  "1000" $$ "0011" $$ "11" $$ op2 $$ reg $ byte @ 
    (fun p => 
      let (r,imm) := p in InstCon true (Reg_op r) (Imm_op (sign_extend8_32 imm)) %%
    instruction_t)
  |+|
  (* zero-extend immediate byte to register *)
  "1000" $$ "0000" $$ "11" $$ op2 $$ reg $ byte @ 
    (fun p => 
      let (r,imm) := p in InstCon false (Reg_op r) (Imm_op (zero_extend8_32 imm)) %%
    instruction_t)
  |+|
  (* immediate word to register *)
  "1000" $$ "0001" $$ "11" $$ op2 $$ reg $ imm_op opsize_override @ 
    (fun p => let (r,imm) := p in InstCon true (Reg_op r) imm %% instruction_t)
  |+|
  (* zero-extend immediate byte to EAX *)
  op1 $$ "100" $$ byte @
    (fun imm => InstCon false (Reg_op EAX) (Imm_op (zero_extend8_32 imm)) %% instruction_t)
  |+|
  (* word to EAX *)
  op1 $$ "101" $$ imm_op opsize_override @
    (fun imm => InstCon true (Reg_op EAX)  imm %% instruction_t)
  |+|
  (* zero-extend immediate byte to memory *)
  "1000" $$ "0000" $$ ext_op_modrm op2 $ byte @ 
    (fun p => let (op,imm) := p in InstCon false op (Imm_op (zero_extend8_32 imm)) %% 
    instruction_t)
  |+|
  (* sign-extend immediate byte to memory *)
  "1000" $$ "0011" $$ ext_op_modrm op2 $ byte @ 
    (fun p => let (op,imm) := p in InstCon true op (Imm_op (sign_extend8_32 imm)) %%
    instruction_t)
  |+|
  (* immediate word to memory *)
  "1000" $$ "0001" $$ ext_op_modrm op2 $ imm_op opsize_override @ 
    (fun p => let (op,imm) := p in InstCon true op imm %% instruction_t).

  Definition ADC_p s := logic_or_arith_p s "00010" "010" ADC.
  Definition ADD_p s := logic_or_arith_p s "00000" "000" ADD.
  Definition AND_p s := logic_or_arith_p s "00100" "100" AND.
  Definition CMP_p s := logic_or_arith_p s "00111" "111" CMP.
  Definition OR_p  s := logic_or_arith_p s "00001" "001" OR.
  Definition SBB_p s := logic_or_arith_p s "00011" "011" SBB.
  Definition SUB_p s := logic_or_arith_p s "00101" "101" SUB.
  Definition XOR_p s := logic_or_arith_p s "00110" "110" XOR.

  Definition ARPL_p := 
  "0110" $$ "0011" $$ modrm @ 
    (fun p => let (op1,op2) := p in ARPL op1 op2 %% instruction_t).

  Definition BOUND_p := 
  "0110" $$ "0010" $$ modrm @ 
    (fun p => let (op1,op2) := p in BOUND op1 op2 %% instruction_t).

  Definition BSF_p := 
  "0000" $$ "1111" $$ "1011" $$ "1100" $$ modrm @ 
    (fun p => let (op1,op2) := p in BSF op1 op2 %% instruction_t).

  Definition BSR_p := 
  "0000" $$ "1111" $$ "1011" $$ "1101" $$ modrm @ 
    (fun p => let (op1,op2) := p in BSR op1 op2 %% instruction_t).

  Definition BSWAP_p := 
  "0000" $$ "1111" $$ "1100" $$ "1" $$ reg @ (fun x => BSWAP x %% instruction_t).

  (* The various bit-testing operations can also share a parser *)
  Definition bit_test_p (opcode1:string) (opcode2:string)
    (Instr : operand -> operand -> instr) := 
    "0000" $$ "1111" $$ "1011" $$ "1010" $$ "11" $$ opcode1 $$ reg $ byte @ 
    (fun p => 
      let (r,imm) := p in Instr (Reg_op r) (Imm_op (zero_extend8_32 imm)) %% instruction_t)
  |+| 
    "0000" $$ "1111" $$ "1011" $$ "1010" $$ ext_op_modrm opcode1 $ byte @
    (fun p => 
      let (op1,imm) := p in Instr op1 (Imm_op (zero_extend8_32 imm)) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "101" $$ opcode2 $$ "011" $$ modrm @
    (fun p => let (op2,op1) := p in Instr op1 op2 %% instruction_t).

  Definition BT_p := bit_test_p "100" "00" BT.
  Definition BTC_p := bit_test_p "111" "11" BTC.
  Definition BTR_p := bit_test_p "110" "10" BTR.
  Definition BTS_p := bit_test_p "101" "01" BTS.

  Definition CALL_p := 
    "1110" $$ "1000" $$ word  @ 
    (fun w => CALL true false (Imm_op w) None %% instruction_t)
  |+|
    "1111" $$ "1111" $$ ext_op_modrm2 "010" @ 
    (fun op => CALL true true op None %% instruction_t)
  |+| 
    "1001" $$ "1010" $$ halfword $ word @ 
    (fun p => CALL false true (Imm_op (snd p)) (Some (fst p)) %% instruction_t)
  |+|
    "1111" $$ "1111" $$ ext_op_modrm2 "011" @ 
    (fun op => CALL false true op None %% instruction_t).

  Definition CDQ_p := "1001" $$ bits "1001" @ (fun _ => CDQ %% instruction_t).
  Definition CLC_p := "1111" $$ bits "1000" @ (fun _ => CLC %% instruction_t).
  Definition CLD_p := "1111" $$ bits "1100" @ (fun _ => CLD %% instruction_t).
  Definition CLI_p := "1111" $$ bits "1010" @ (fun _ => CLI %% instruction_t).
  Definition CLTS_p := "0000" $$ "1111" $$ "0000" $$ bits "0110" @ 
    (fun _ => CLTS %% instruction_t).
  Definition CMC_p := "1111" $$ bits "0101" @ (fun _ => CMC %% instruction_t).
  Definition CMPS_p := "1010" $$ "011" $$ anybit @ (fun x => CMPS x %% instruction_t).
  Definition CMPXCHG_p := 
   "0000" $$ "1111" $$ "1011" $$ "000" $$ anybit $ modrm @ 
    (fun p => match p with 
                | (w,(op1,op2)) => CMPXCHG w op2 op1
              end %% instruction_t).

  Definition CPUID_p := "0000" $$ "1111" $$ "1010" $$ bits "0010" @ 
    (fun _ => CPUID %% instruction_t).
  Definition CWDE_p := "1001" $$ bits "1000" @ (fun _ => CWDE %% instruction_t).
  Definition DAA_p := "0010" $$ bits "0111" @ (fun _ => DAA %% instruction_t).
  Definition DAS_p := "0010" $$ bits "1111" @ (fun _ => DAS %% instruction_t).

  Definition DEC_p := 
    "1111" $$ "111" $$ anybit $ "11001" $$ reg @ 
      (fun p => let (w,r) := p in DEC w (Reg_op r) %% instruction_t)
  |+|
    "0100" $$ "1" $$ reg @ 
      (fun r => DEC true (Reg_op r) %% instruction_t)
  |+| 
    "1111" $$ "111" $$ anybit $ ext_op_modrm "001" @
      (fun p => let (w,op1) := p in DEC w op1 %% instruction_t).

  Definition DIV_p := 
    "1111" $$ "011" $$ anybit $ "11110" $$ reg @ 
      (fun p => let (w,r) := p in DIV w (Reg_op r) %% instruction_t)
  |+| 
    "1111" $$ "011" $$ anybit $ ext_op_modrm "110" @ 
      (fun p => let (w,op1) := p in DIV w op1 %% instruction_t).

  Definition HLT_p := "1111" $$ bits "0100" @ (fun _ => HLT %% instruction_t).

  Definition IDIV_p := 
    "1111" $$ "011" $$ anybit $ "11111" $$ reg @ 
    (fun p => let (w,r) := p in IDIV w (Reg_op r) %% instruction_t)
  |+|
    "1111" $$ "011" $$ anybit $ ext_op_modrm "111" @ 
     (fun p => let (w,op1) := p in IDIV w op1 %% instruction_t).

  Definition IMUL_p opsize_override := 
    "1111" $$ "011" $$ anybit $ ext_op_modrm2 "101" @
    (fun p => let (w,op1) := p in IMUL w op1 None None %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "1010" $$ "1111" $$ modrm @
    (fun p => let (op1,op2) := p in IMUL true op1 (Some op2) None %% instruction_t)
  |+|
    "0110" $$ "1011" $$ modrm $ byte @
    (fun p => match p with 
                | ((op1,op2),imm) => 
                  IMUL true op1 (Some op2) (Some (sign_extend8_32 imm))
              end %% instruction_t)
  |+|
    match opsize_override with
      | false =>
          "0110" $$ "1001" $$ modrm $ word @
           (fun p => match p with 
                | ((op1,op2),imm) => 
                  IMUL true op1 (Some op2) (Some imm)
              end  %% instruction_t)
      | true => 
          "0110" $$ "1001" $$ modrm $ halfword @
           (fun p => match p with 
                | ((op1,op2),imm) => 
                  IMUL true op1 (Some op2) (Some (sign_extend16_32 imm))
              end  %% instruction_t)
    end.

  Definition IN_p := 
    "1110" $$ "010" $$ anybit $ byte @ 
    (fun p => let (w,pt) := p in IN w (Some pt) %% instruction_t)
  |+|
    "1110" $$ "110" $$ anybit @ (fun w => IN w None %% instruction_t).

  Definition INC_p := 
    "1111" $$ "111" $$ anybit  $ "11000" $$ reg @ 
      (fun p => let (w,r) := p in INC w (Reg_op r) %% instruction_t)
  |+|
    "0100" $$ "0" $$ reg @ (fun r => INC true (Reg_op r) %% instruction_t)
  |+|
    "1111" $$ "111" $$ anybit $ ext_op_modrm "000" @ 
       (fun p => let (w,op1) := p in INC w op1 %% instruction_t).

  Definition INS_p := "0110" $$ "110" $$ anybit @ (fun x => INS x %% instruction_t).

  Definition INTn_p := "1100" $$ "1101" $$ byte @ (fun x => INTn x %% instruction_t).
  Definition INT_p := "1100" $$ bits "1100" @ (fun _ => INT %% instruction_t).

  Definition INTO_p := "1100" $$ bits "1110" @ (fun _ => INTO %% instruction_t).
  Definition INVD_p := "0000" $$ "1111" $$ "0000" $$ bits "1000" @ 
    (fun _ => INVD %% instruction_t).

  Definition INVLPG_p := 
    "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "111" @ 
    (fun x => INVLPG x %% instruction_t).

  Definition IRET_p := "1100" $$ bits "1111" @ (fun _ => IRET %% instruction_t).

  Definition Jcc_p := 
    "0111" $$ tttn $ byte @ 
    (fun p => let (ct,imm) := p in Jcc ct (sign_extend8_32 imm) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "1000" $$ tttn $ word @ 
    (fun p => let (ct,imm) := p in Jcc ct imm %% instruction_t).

  Definition JCXZ_p := "1110" $$ "0011" $$ byte @ (fun x => JCXZ x %% instruction_t).

  Definition JMP_p := 
    "1110" $$ "1011" $$ byte @
    (fun b => JMP true false (Imm_op (sign_extend8_32 b)) None %% instruction_t)
  |+|
    "1110" $$ "1001" $$ word @ 
    (fun w => JMP true false (Imm_op w) None %% instruction_t)
  |+|
    "1111" $$ "1111" $$ ext_op_modrm2 "100" @ 
    (fun op => JMP true true op None %% instruction_t)
  |+|
    "1110" $$ "1010" $$ halfword $ word @ 
      (fun p => JMP false true (Imm_op (snd p)) (Some (fst p)) %% instruction_t)
  |+|
    "1111" $$ "1111" $$ ext_op_modrm2 "101" @ 
    (fun op => JMP false true op None %% instruction_t).

  Definition LAHF_p := "1001" $$ bits "1111" @ (fun _ => LAHF %% instruction_t).

  Definition LAR_p := 
    "0000" $$ "1111" $$ "0000" $$ "0010" $$ modrm @ 
      (fun p => LAR (fst p) (snd p) %% instruction_t).

  Definition LDS_p := "1100" $$ "0101" $$ modrm @ 
    (fun p => LDS (fst p) (snd p) %% instruction_t).
  Definition LEA_p := "1000" $$ "1101" $$ modrm_noreg @ 
    (fun p => LEA (Reg_op (fst p)) (snd p) %% instruction_t).
  Definition LEAVE_p := "1100" $$ bits "1001" @ 
    (fun _ => LEAVE %% instruction_t).
  Definition LES_p := "1100" $$ "0100" $$ modrm @ 
    (fun p => LES (fst p) (snd p) %% instruction_t).
  Definition LFS_p := "0000" $$ "1111" $$ "1011" $$ "0100" $$ modrm @ 
    (fun p => LFS (fst p) (snd p) %% instruction_t).
  Definition LGDT_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "010" @ 
    (fun x => LGDT x %% instruction_t).
  Definition LGS_p := "0000" $$ "1111" $$ "1011" $$ "0101" $$ modrm @ 
    (fun p => LGS (fst p) (snd p) %% instruction_t).
  Definition LIDT_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "011" @ 
    (fun x => LIDT x %% instruction_t).
  Definition LLDT_p := 
    "0000" $$ "1111" $$ "0000" $$ "0000" $$ "11" $$ "010" $$ reg @ 
    (fun r => LLDT (Reg_op r) %% instruction_t)
  |+| 
    "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "010" @ 
    (fun x => LLDT x %% instruction_t).

  Definition LMSW_p := 
    "0000" $$ "1111" $$ "0000" $$ "0001" $$ "11" $$ "110" $$ reg @ 
      (fun r => LMSW (Reg_op r) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0000" $$ "0001" $$ "11" $$ ext_op_modrm "110" @ 
      (fun x => LMSW x %% instruction_t).

  (* JGM: note, this isn't really an instruction, but rather a prefix.  So it
     shouldn't be included in the list of instruction parsers. *)
(*  Definition LOCK_p := "1111" $$ bits "0000" @ (fun _ => LOCK %% instruction_t). *)
  Definition LODS_p := "1010" $$ "110" $$ anybit @ (fun x => LODS x %% instruction_t).
  Definition LOOP_p := "1110" $$ "0010" $$ byte @ (fun x => LOOP x %% instruction_t).
  Definition LOOPZ_p := "1110" $$ "0001" $$ byte @ (fun x => LOOPZ x %% instruction_t).
  Definition LOOPNZ_p := "1110" $$ "0000" $$ byte @ (fun x => LOOPNZ x %% instruction_t).
  Definition LSL_p := "0000" $$ "1111" $$ "0000" $$ "0011" $$ modrm @ 
    (fun p => LSL (fst p) (snd p) %% instruction_t).
  Definition LSS_p := "0000" $$ "1111" $$ "1011" $$ "0010" $$ modrm @ 
    (fun p => LSS (fst p) (snd p) %% instruction_t).
  Definition LTR_p := "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "011" @ 
    (fun x => LTR x %% instruction_t).

  (* This is may not be right. Need to test this thoroughly. 
     There is no 8bit mode for CMOVcc *)

  Definition CMOVcc_p :=
    "0000" $$ "1111" $$ "0100" $$ tttn $ modrm @
    (fun p => match p with | (tttn, (op1, op2))=>CMOVcc tttn op1 op2 end %% instruction_t).

  Definition MOV_p opsize_override := 
    "1000" $$ "101" $$ anybit $ modrm @ 
      (fun p => match p with | (w,(op1,op2)) => MOV w op1 op2 end %% instruction_t)
  |+|
    "1000" $$ "100" $$ anybit $ modrm @ 
      (fun p => match p with | (w,(op1,op2)) => MOV w op2 op1 end %% instruction_t)
  |+|
   "1100" $$ "0111" $$ "11" $$ "000" $$ reg $ imm_op opsize_override @
     (fun p => match p with | (r,w) => MOV true  (Reg_op r) w end %% instruction_t)
  |+|
   "1100" $$ "0110" $$ "11" $$ "000" $$ reg $ byte @
     (fun p => match p with
                 | (r,b) => MOV false (Reg_op r) (Imm_op (zero_extend8_32 b)) 
               end %% instruction_t)
  |+|
    "1011" $$ "1" $$ reg $ imm_op opsize_override @ 
      (fun p => match p with | (r,w) => MOV true (Reg_op r)  w
                end %% instruction_t)
  |+| 
    "1011" $$ "0" $$ reg $ byte @ 
      (fun p => match p with 
                  | (r,b) => MOV false (Reg_op r) (Imm_op (zero_extend8_32 b))
                end %% instruction_t)
  |+|
    "1100" $$ "0111" $$ ext_op_modrm "000" $ imm_op opsize_override @ 
      (fun p => match p with | (op,w) => MOV true op w end %% instruction_t)
  |+|
    "1100" $$ "0110" $$ ext_op_modrm "000" $ byte @ 
    (fun p => match p with | (op,b) => MOV false op (Imm_op (zero_extend8_32 b)) end %% instruction_t)
  |+|
    "1010" $$ "0001" $$ word @ (fun w => MOV true  (Reg_op EAX) (Offset_op w) %% instruction_t)
  |+|
    "1010" $$ "0000" $$ word @ (fun w => MOV false (Reg_op EAX) (Offset_op w)  %% instruction_t)
  |+|
    "1010" $$ "0011" $$ word @ (fun w => MOV true (Offset_op w) (Reg_op EAX) %% instruction_t)
  |+|
    "1010" $$ "0010" $$ word @ (fun w => MOV false (Offset_op w) (Reg_op EAX) %% instruction_t).
  

  Definition control_reg_p := 
      bits "000" @ (fun _ => CR0 %% control_register_t) 
  |+| bits "010" @ (fun _ => CR2 %% control_register_t) 
  |+| bits "011" @ (fun _ => CR3 %% control_register_t) 
  |+| bits "100" @ (fun _ => CR4 %% control_register_t).
  
  Definition MOVCR_p := 
    "0000" $$ "1111" $$ "0010" $$ "0010" $$ "11" $$ control_reg_p $ reg @ 
    (fun p => MOVCR false (fst p) (snd p) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0010" $$ "0000" $$ "11" $$ control_reg_p $ reg @ 
    (fun p => MOVCR true (fst p) (snd p) %% instruction_t).

  (* Note:  apparently, the bit patterns corresponding to DR4 and DR5 either
   * (a) get mapped to DR6 and DR7 respectively or else (b) cause a fault,
   * depending upon the value of some control register.  My guess is that it's
   * okay for us to just consider this a fault. Something similar seems to
   * happen with the CR registers above -- e.g., we don't have a CR1. *)
  Definition debug_reg_p := 
      bits "000" @ (fun _ => DR0 %% debug_register_t) 
  |+| bits "001" @ (fun _ => DR1 %% debug_register_t) 
  |+| bits "010" @ (fun _ => DR2 %% debug_register_t) 
  |+| bits "011" @ (fun _ => DR3 %% debug_register_t) 
  |+| bits "110" @ (fun _ => DR6 %% debug_register_t) 
  |+| bits "111" @ (fun _ => DR7 %% debug_register_t).

  Definition MOVDR_p := 
    "0000" $$ "1111" $$ "0010" $$ "0011" $$ "11" $$ debug_reg_p $ reg @
    (fun p => MOVDR false (fst p) (snd p) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0010" $$ "0001" $$ "11" $$ debug_reg_p $ reg @
    (fun p => MOVDR true (fst p) (snd p) %% instruction_t).

  Definition segment_reg_p := 
      bits "000" @ (fun _ => ES %% segment_register_t) 
  |+| bits "001" @ (fun _ => CS %% segment_register_t) 
  |+| bits "010" @ (fun _ => SS %% segment_register_t) 
  |+| bits "011" @ (fun _ => DS %% segment_register_t) 
  |+| bits "100" @ (fun _ => FS %% segment_register_t) 
  |+| bits "101" @ (fun _ => GS %% segment_register_t).

  Definition seg_modrm : parser (pair_t segment_register_t operand_t) := 
    (     ("00" $$ segment_reg_p $ rm00) 
      |+| ("01" $$ segment_reg_p $ rm01)
      |+| ("10" $$ segment_reg_p $ rm10)) @
            (fun p => match p with
                      | (sr, addr) => (sr, Address_op addr)
                      end %% (pair_t segment_register_t operand_t))
   |+| ("11" $$ segment_reg_p $ rm11).

  Definition MOVSR_p := 
    "1000" $$ "1110" $$ seg_modrm @ 
      (fun p => MOVSR false (fst p) (snd p) %% instruction_t)
  |+|
    "1000" $$ "1100" $$ seg_modrm @ 
     (fun p => MOVSR true (fst p) (snd p) %% instruction_t).

  Definition MOVBE_p := 
    "0000" $$ "1111" $$ "0011" $$ "1000" $$ "1111" $$ "0000" $$ modrm @
    (fun p => MOVBE (snd p) (fst p) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0011" $$ "1000" $$ "1111" $$ "0001" $$ modrm @ 
    (fun p => MOVBE (fst p) (snd p) %% instruction_t).

  Definition MOVS_p := "1010" $$ "010" $$ anybit @ (fun x => MOVS x %% instruction_t).

  Definition MOVSX_p := "0000" $$ "1111" $$ "1011" $$ "111" $$ anybit $ modrm @
    (fun p => match p with | (w,(op1,op2)) => MOVSX w op1 op2 end %% instruction_t).

  Definition MOVZX_p := "0000" $$ "1111" $$ "1011" $$ "011" $$ anybit $ modrm @
    (fun p => match p with | (w,(op1,op2)) => MOVZX w op1 op2 end %% instruction_t).

  Definition MUL_p := 
  "1111" $$ "011" $$ anybit $ ext_op_modrm2 "100" @ 
    (fun p => MUL (fst p) (snd p) %% instruction_t).

  Definition NEG_p := 
  "1111" $$ "011" $$ anybit $ ext_op_modrm2 "011" @ 
    (fun p => NEG (fst p) (snd p) %% instruction_t).

  (*
  Definition NOP_p := 
    "1001" $$ bits "0000" @ (fun _ => NOP None %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0001" $$ "1111" $$ ext_op_modrm "000" @ 
    (fun op => NOP (Some op) %% instruction_t).
  *)

  Definition NOT_p := 
    "1111" $$ "011" $$ anybit $ ext_op_modrm2 "010" @ 
    (fun p => NOT (fst p) (snd p) %% instruction_t).

  Definition OUT_p := 
    "1110" $$ "011" $$ anybit $ byte @ 
      (fun p => OUT (fst p) (Some (snd p)) %% instruction_t)
  |+|
    "1110" $$ "111" $$ anybit @ (fun w => OUT w None %% instruction_t).

  Definition OUTS_p := "0110" $$ "111" $$ anybit @ (fun x => OUTS x %% instruction_t).

  Definition POP_p := 
  "1000" $$ "1111" $$ ext_op_modrm2 "000" @ (fun x => POP x %% instruction_t)
  |+|
    "0101" $$ "1" $$ reg @ (fun r => POP (Reg_op r) %% instruction_t).

  Definition POPSR_p := 
    "000" $$ "00" $$ bits "111" @ (fun _ => POPSR ES %% instruction_t)
  |+|
    "000" $$ "10" $$ bits "111" @ (fun _ => POPSR SS %% instruction_t)
  |+|
    "000" $$ "11" $$ bits "111" @ (fun _ => POPSR DS %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "10" $$ "100" $$ bits "001" @ 
      (fun _ => POPSR FS %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "10" $$ "101" $$ bits "001" @ 
      (fun _ => POPSR GS %% instruction_t).

  Definition POPA_p := "0110" $$ bits "0001" @ (fun _ => POPA %% instruction_t).
  Definition POPF_p := "1001" $$ bits "1101" @ (fun _ => POPF %% instruction_t).
  
  Definition PUSH_p := 
    "1111" $$ "1111" $$ ext_op_modrm "110" @ (fun x => PUSH true x %% instruction_t)
  |+|
    "0101" $$ "0" $$ reg @ (fun r => PUSH true (Reg_op r) %% instruction_t)
  |+|
    "0110" $$ "1010" $$ byte @ 
    (fun b => PUSH false (Imm_op (sign_extend8_32 b)) %% instruction_t)
  |+|
    "0110" $$ "1000" $$ word @ (fun w => PUSH true (Imm_op w) %% instruction_t).

  Definition segment_reg2_p := 
        bits "00" @ (fun _ => ES %% segment_register_t) 
    |+| bits "01" @ (fun _ => CS %% segment_register_t) 
    |+| bits "10" @ (fun _ => SS %% segment_register_t) 
    |+| bits "11" @ (fun _ => DS %% segment_register_t).

  Definition PUSHSR_p := 
    "000" $$ segment_reg2_p $ bits "110" @ 
    (fun p => PUSHSR (fst p) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "10" $$ "100" $$ bits "000" @ 
    (fun _ => PUSHSR FS %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "10" $$ "101" $$ bits "000" @ 
    (fun _ => PUSHSR GS %% instruction_t).

  Definition PUSHA_p := "0110" $$ bits "0000" @ (fun _ => PUSHA %% instruction_t).
  Definition PUSHF_p := "1001" $$ bits "1100" @ (fun _ => PUSHF %% instruction_t).

  Definition rotate_p extop (inst : bool -> operand -> reg_or_immed -> instr) := 
    "1101" $$ "000" $$ anybit $ ext_op_modrm2 extop @ 
    (fun p => inst (fst p) (snd p) (Imm_ri (Word.repr 1)) %% instruction_t)
  |+|
    "1101" $$ "001" $$ anybit $ ext_op_modrm2 extop @
    (fun p => inst (fst p) (snd p) (Reg_ri ECX) %% instruction_t)
  |+|
    "1100" $$ "000" $$ anybit $ ext_op_modrm2 extop $ byte @
    (fun p => match p with | (w, (op,b)) => inst w op (Imm_ri b) end %% instruction_t).

  Definition RCL_p := rotate_p "010" RCL.
  Definition RCR_p := rotate_p "011" RCR.

  Definition RDMSR_p := "0000" $$ "1111" $$ "0011" $$ bits "0010" @ 
    (fun _ => RDMSR %% instruction_t).
  Definition RDPMC_p := "0000" $$ "1111" $$ "0011" $$ bits "0011" @ 
    (fun _ => RDPMC %% instruction_t).
  Definition RDTSC_p := "0000" $$ "1111" $$ "0011" $$ bits "0001" @ 
    (fun _ => RDTSC %% instruction_t).
  Definition RDTSCP_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ "1111" $$ bits "1001" @
    (fun _ => RDTSCP %% instruction_t).

  (*
  Definition REPINS_p := "1111" $$ "0011" $$ "0110" $$ "110" $$ anybit @ 
    (fun x => REPINS x %% instruction_t).
  Definition REPLODS_p := "1111" $$ "0011" $$ "1010" $$ "110" $$ anybit @ 
    (fun x => REPLODS x %% instruction_t).
  Definition REPMOVS_p := "1111" $$ "0011" $$ "1010" $$ "010" $$ anybit @ 
    (fun x => REPMOVS x %% instruction_t).
  Definition REPOUTS_p := "1111" $$ "0011" $$ "0110" $$ "111" $$ anybit @ 
    (fun x => REPOUTS x %% instruction_t).
  Definition REPSTOS_p := "1111" $$ "0011" $$ "1010" $$ "101" $$ anybit @ 
    (fun x => REPSTOS x %% instruction_t).
  Definition REPECMPS_p := "1111" $$ "0011" $$ "1010" $$ "011" $$ anybit @ 
    (fun x => REPECMPS x %% instruction_t).
  Definition REPESCAS_p := "1111" $$ "0011" $$ "1010" $$ "111" $$ anybit @ 
    (fun x => REPESCAS x %% instruction_t).
  Definition REPNECMPS_p := "1111" $$ "0010" $$ "1010" $$ "011" $$ anybit @ 
    (fun x => REPNECMPS x %% instruction_t).
  Definition REPNESCAS_p := "1111" $$ "0010" $$ "1010" $$ "111" $$ anybit @ 
    (fun x => REPNESCAS x %% instruction_t).
  *)

  Definition RET_p := 
    "1100" $$ bits "0011" @ (fun _ => RET true None %% instruction_t)
  |+|
    "1100" $$ "0010" $$ halfword @ (fun h => RET true (Some h) %% instruction_t)
  |+|
    "1100" $$ bits "1011" @ (fun _ => RET false None %% instruction_t)
  |+|
    "1100" $$ "1010" $$ halfword @ (fun h => RET false (Some h) %% instruction_t).

  Definition ROL_p := rotate_p "000" ROL.
  Definition ROR_p := rotate_p "001" ROR.
  Definition RSM_p := "0000" $$ "1111" $$ "1010" $$ bits "1010" @ 
    (fun _ => RSM %% instruction_t).
  Definition SAHF_p := "1001" $$ bits "1110" @ 
    (fun _ => SAHF %% instruction_t).
  Definition SAR_p := rotate_p "111" SAR.
  Definition SCAS_p := "1010" $$ "111" $$ anybit @ (fun x => SCAS x %% instruction_t).
  Definition SETcc_p := 
  "0000" $$ "1111" $$ "1001" $$ tttn $ modrm @ 
    (fun p => SETcc (fst p) (snd (snd p)) %% instruction_t).
  Definition SGDT_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "000" @ 
    (fun x => SGDT x %% instruction_t).
  Definition SHL_p := rotate_p "100" SHL.

  Definition shiftdouble_p opcode inst :=
    ("0000" $$ "1111" $$ "1010" $$ opcode $$ "00" $$ "11" $$ reg $ reg $ byte) @
    (fun p => match p with | (r2,(r1,b)) => inst (Reg_op r1) r2 (Imm_ri b) end %% instruction_t)
  |+|
    ("0000" $$ "1111" $$ "1010" $$ opcode $$ "00" $$ modrm_noreg $ byte) @
    (fun p => match p with | ((r,op), b) => inst op r (Imm_ri b) end %% instruction_t)
  |+|
    ("0000" $$ "1111" $$ "1010" $$ opcode $$ "01" $$ "11" $$ reg $ reg) @
    (fun p => match p with | (r2,r1) => inst (Reg_op r1) r2 (Reg_ri ECX) end %% instruction_t)
  |+|
    ("0000" $$ "1111" $$ "1010" $$ opcode $$ "01" $$ modrm_noreg) @
    (fun p => match p with | (r,op) => inst op r (Reg_ri ECX) end %% instruction_t).
 
  Definition SHLD_p := shiftdouble_p "01" SHLD.
  Definition SHR_p := rotate_p "101" SHR.
  Definition SHRD_p := shiftdouble_p "11" SHRD.
  Definition SIDT_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "001" @ 
    (fun x => SIDT x %% instruction_t).
  Definition SLDT_p := "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "000" @ 
    (fun x => SLDT x %% instruction_t).
  Definition SMSW_p := "0000" $$ "1111" $$ "0000" $$ "0001" $$ ext_op_modrm "100" @ 
    (fun x => SMSW x %% instruction_t).
  Definition STC_p := "1111" $$ bits "1001" @ (fun _ => STC %% instruction_t).
  Definition STD_p := "1111" $$ bits "1101" @ (fun _ => STD %% instruction_t).
  Definition STI_p := "1111" $$ bits "1011" @ (fun _ => STI %% instruction_t).
  Definition STOS_p := "1010" $$ "101" $$ anybit @ 
    (fun x => STOS x %% instruction_t).
  Definition STR_p := "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "001" @ 
    (fun x => STR x %% instruction_t).

  Definition TEST_p (opsize_override: bool) := 
    "1111" $$ "0111" $$ ext_op_modrm2 "000" $ imm_op opsize_override @ 
    (fun p => TEST true (fst p) (snd p) %% instruction_t)
  |+| 
    "1111" $$ "0110" $$ ext_op_modrm2 "000" $ byte @ 
    (fun p => TEST false (fst p) (Imm_op (zero_extend8_32 (snd p))) %% instruction_t)
  |+|
    "1000" $$ "010" $$ anybit $ modrm @
    (fun p => match p with | (w,(op1,op2)) => TEST w op1 op2 end %% instruction_t)
  |+|
    "1010" $$ "1001" $$ imm_op opsize_override @ (fun w => TEST true w (Reg_op EAX) %% instruction_t)
  |+|
    "1010" $$ "1000" $$ byte @ 
    (fun b => TEST true (Imm_op (zero_extend8_32 b)) (Reg_op EAX) %% instruction_t).
  
  Definition UD2_p := "0000" $$ "1111" $$ "0000" $$ bits "1011" @ 
    (fun _ => UD2 %% instruction_t).

  Definition VERR_p := "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "100" @ 
    (fun x => VERR x %% instruction_t).
  Definition VERW_p := "0000" $$ "1111" $$ "0000" $$ "0000" $$ ext_op_modrm "101" @ 
    (fun x => VERW x %% instruction_t).
  Definition WBINVD_p := "0000" $$ "1111" $$ "0000" $$ bits "1001" @ 
    (fun _ => WBINVD %% instruction_t).
  Definition WRMSR_p := "0000" $$ "1111" $$ "0011" $$ bits "0000" @ 
    (fun _ => WRMSR %% instruction_t).
  Definition XADD_p := 
    "0000" $$ "1111" $$ "1100" $$ "000" $$ anybit $ modrm @ 
    (fun p => match p with | (w,(op1,op2)) => XADD w op2 op1 end %% instruction_t).
  Definition XCHG_p := 
    "1000" $$ "011" $$ anybit $ modrm @ 
    (fun p => match p with | (w,(op1,op2)) => XCHG w op1 op2 end %% instruction_t)
  |+|
    "1001" $$ "0" $$ reg @ (fun r => XCHG true (Reg_op EAX) (Reg_op r) %% instruction_t).

  Definition XLAT_p := "1101" $$ bits "0111" @ (fun _ => XLAT %% instruction_t).

(*Floating-Point parsers, based on tables B.17 and B-39*)
  Definition F2XM1_p := "11011" $$ "001111" $$ bits "10000" @ (fun _ => F2XM1 %% instruction_t).
  Definition FABS_p :=  "11011" $$ "001111" $$ bits "00001" @ (fun _ => FABS %% instruction_t). 

  Definition FADD_p := 
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "000" @ 
      (fun x => FADD true x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "000" @
      (fun x => FADD true x %% instruction_t) 
  |+|  
    "11011" $$ anybit $ "0011000" $$ fpu_reg @ (fun p => let (d,s) := p in FADD d (FPS_op s) %% instruction_t).

  Definition FADDP_p := "11011" $$ "110" $$ "11000" $$ fpu_reg @ (fun x => FADDP (FPS_op x) %% instruction_t).
  Definition FBLD_p := "11011" $$ "111" $$ ext_op_modrm_FPM64 "100" @
                          (fun x => FBLD x %% instruction_t).
  Definition FBSTP_p := "11011" $$ "111" $$ ext_op_modrm_FPM64 "110" @
                          (fun x => FBSTP x %% instruction_t).
  Definition FCHS_p := "11011" $$ "001111" $$ bits "00000" @ (fun _ => FCHS %% instruction_t).
  Definition FCLEX_p := "11011" $$ "011111" $$ bits "00010" @ (fun _ => FCLEX %% instruction_t).

  Definition FCOM_p :=
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "010" @
        (fun x => FCOM (Some x) %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "010" @
        (fun x => FCOM (Some x) %% instruction_t) 
  |+|  
    "11011" $$ "000" $$ "11010" $$ fpu_reg @ (fun x => FCOM (Some (FPS_op x)) %% instruction_t).

  Definition FCOMP_p :=
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "011" @
       (fun x => FCOMP (Some x) %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "011" @
       (fun x => FCOMP (Some x) %% instruction_t) 
  |+|  
    "11011" $$ "000" $$ "11011" $$ fpu_reg @ (fun x => FCOMP (Some (FPS_op x)) %% instruction_t).

  Definition FCOMPP_p := "11011" $$ "110" $$ "11011" $$ bits "001" @ (fun _ => FCOMPP %% instruction_t).
  Definition FCOMIP_p := "11011" $$ "111" $$ "11110" $$ fpu_reg @ (fun x => FCOMIP (FPS_op x) %% instruction_t).
  Definition FCOS_p := "11011" $$ "001" $$ "111" $$ bits "11111" @ (fun _ => FCOS %% instruction_t).  
  Definition FDECSTP_p := "11011" $$ "001" $$ "111" $$ bits "10110" @ (fun _=> FDECSTP %% instruction_t).

  Definition FDIV_p :=
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "110" @ 
       (fun x => FDIV (FPS_op Word.zero) x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "110" @
       (fun x => FDIV (FPS_op Word.zero) x %% instruction_t)
  |+|  
    "11011" $$ "0" $$ "00" $$ "1111" $$ "0" $$ fpu_reg @ 
    (fun i => FDIV (FPS_op Word.zero) (FPS_op i) %% instruction_t)
  |+| 
    "11011" $$ "1" $$ "00" $$ "111" $$ "1" $$ "1" $$ fpu_reg @ 
    (fun i => FDIV (FPS_op i) (FPS_op Word.zero) %% instruction_t).

  Definition FDIVP_p := "11011" $$ "110" $$ "11111" $$ fpu_reg @ (fun x => FDIVP (FPS_op x) %% instruction_t).

  Definition FDIVR_p :=
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "111" @
       (fun x => FDIVR (FPS_op Word.zero) x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "111" @
       (fun x => FDIVR (FPS_op Word.zero) x  %% instruction_t)
  |+|  
    "11011" $$ "0" $$ "00" $$ "111" $$ "1" $$ "1" $$ fpu_reg @ 
    (fun i => FDIVR (FPS_op Word.zero) (FPS_op i) %% instruction_t)
  |+|  
    "11011" $$ "1" $$ "00" $$ "111" $$ "1" $$ "0" $$ fpu_reg @ 
    (fun i => FDIVR (FPS_op i) (FPS_op Word.zero) %% instruction_t).

  Definition FDIVRP_p := "11011" $$ "110" $$ "11110" $$ fpu_reg @ (fun x => FDIVRP (FPS_op x) %% instruction_t).
  Definition FFREE_p := "11011" $$ "101" $$ "11000" $$ fpu_reg @ (fun x => FFREE (FPS_op x) %% instruction_t).
  Definition FIADD_p := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "000" @ (fun x => FIADD x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "000" @ (fun x => FIADD x %% instruction_t).
  
  Definition FICOM_p  := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "010" @ (fun x => FICOM x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "010" @ (fun x => FICOM x %% instruction_t).

  Definition FICOMP_p  := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "011" @ (fun x => FICOMP x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "011" @ (fun x => FICOMP x %% instruction_t).

  Definition FIDIV_p  := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "110" @ (fun x => FIDIV x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "110" @ (fun x => FIDIV x %% instruction_t).

  Definition FIDIVR_p  := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "111" @ (fun x => FIDIVR x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "111" @ (fun x => FIDIVR x %% instruction_t).

  Definition FILD_p  := 
    "11011" $$ "111" $$ ext_op_modrm_FPM32 "000" @ (fun x => FILD x %% instruction_t)
  |+|
    "11011" $$ "011" $$ ext_op_modrm_FPM32 "000" @ (fun x => FILD x %% instruction_t)
  |+|
    "11011" $$ "111" $$ ext_op_modrm_FPM64 "101" @ (fun x => FILD x %% instruction_t).
  Definition FIMUL_p := 
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "001" @ (fun x => FIMUL x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM64 "001" @ (fun x => FIMUL x %% instruction_t).
  Definition FINCSTP_p := "11011" $$ "001111" $$ bits "10111" @ (fun _ => FINCSTP %% instruction_t).
  (*Definition FINIT := "".  Nothing provided in table B-39 *) 
  Definition FIST_p :=
    "11011" $$ "111" $$ ext_op_modrm_FPM32 "010" @ (fun x => FIST x %% instruction_t)
  |+|
    "11011" $$ "011" $$ ext_op_modrm_FPM64 "010" @ (fun x => FIST x %% instruction_t).

  Definition FISTP_p :=
    "11011" $$ "111" $$ ext_op_modrm_FPM32 "011" @ (fun x => FISTP x %% instruction_t)
  |+|
    "11011" $$ "011" $$ ext_op_modrm_FPM32 "011" @ (fun x => FISTP x %% instruction_t)
  |+|
    "11011" $$ "111" $$ ext_op_modrm_FPM64 "111" @ (fun x => FISTP x %% instruction_t).

  Definition FISUB_p :=
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "100" @ (fun x => FISUB x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "100" @ (fun x => FISUB x %% instruction_t).

  Definition FISUBR_p :=
    "11011" $$ "110" $$ ext_op_modrm_FPM32 "101" @ (fun x => FISUBR x %% instruction_t)
  |+|
    "11011" $$ "010" $$ ext_op_modrm_FPM32 "101" @ (fun x => FISUBR x %% instruction_t).

  Definition FLD_p :=
    "11011" $$ "001" $$ ext_op_modrm_FPM32 "000" @ (fun x => FLD x %% instruction_t)
  |+|
    "11011" $$ "101" $$ ext_op_modrm_FPM64 "000" @ (fun x => FLD x %% instruction_t)
  |+|
    "11011" $$ "011" $$ ext_op_modrm_FPM80 "101" @ (fun x => FLD x %% instruction_t)
  |+|
    "11011" $$ "001" $$ "11000" $$ fpu_reg @ (fun x => FLD (FPS_op x) %% instruction_t).

  Definition FLD1_p := "11011" $$ "001111" $$ bits "01000" @ (fun _ => FLD1 %% instruction_t).
  Definition FLDCW_p := "11011" $$ "001" $$ ext_op_modrm_FPM32 "101" @ (fun x => FLDCW x %% instruction_t).
  Definition FLDENV_p := "11011" $$ "001" $$ ext_op_modrm_FPM32 "100" @ (fun x => FLDENV x %% instruction_t).
  Definition FLDL2E_p := "11011" $$ "001111" $$ bits "01010" @ (fun _ => FLDL2E %% instruction_t). 
  Definition FLDL2T_p := "11011" $$ "001111" $$ bits "01001" @ (fun _ => FLDL2T %% instruction_t). 
  Definition FLDLG2_p := "11011" $$ "001111" $$ bits "01100" @ (fun _ => FLDLG2 %% instruction_t). 
  Definition FLDLN2_p := "11011" $$ "001111" $$ bits "01101" @ (fun _ => FLDLN2 %% instruction_t). 
  Definition FLDPI_p := "11011" $$ "001111" $$ bits "01011" @ (fun _ => FLDPI %% instruction_t).
  Definition FLDZ_p := "11011" $$ "001111" $$ bits "01110" @ (fun _ => FLDZ %% instruction_t).

  Definition FMUL_p := 
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "001" @ (fun x => FMUL true x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "001" @ (fun x => FMUL true x %% instruction_t) 
  |+|  
    "11011" $$ anybit $ "00" $$ "11001" $$ fpu_reg @ (fun p => let (d,s) := p in FMUL d (FPS_op s) %% instruction_t).

  Definition FMULP_p := "11011" $$ "110" $$ "11001" $$ fpu_reg @ (fun x => FMULP (FPS_op x) %% instruction_t).
  Definition FNOP_p := "11011" $$ "001110" $$ bits "10000" @ (fun _ => FNOP %% instruction_t).
  Definition FNSAVE_p := "11011101" $$ ext_op_modrm_FPM64 "110" @ (fun x => FNSAVE x %% instruction_t).
  Definition FNSTCW_p := "11011" $$ "001" $$ ext_op_modrm_FPM32 "111" @ (fun x => FNSTCW x %% instruction_t).
  Definition FPATAN_p := "11011" $$ "001111" $$ bits "10011" @ (fun _ => FPATAN %% instruction_t).
  Definition FPREM_p := "11011" $$ "001111" $$ bits "11000" @ (fun _ => FPREM %% instruction_t).
  Definition FPREM1_p := "11011" $$ "001111" $$ bits "10101" @ (fun _ => FPREM1 %% instruction_t).
  Definition FPTAN_p := "11011" $$ "001111" $$ bits "10010" @ (fun _ => FPTAN %% instruction_t).
  Definition FRNDINT_p := "11011" $$ "001111" $$ bits "11100" @ (fun _ => FRNDINT %% instruction_t).

  Definition FRSTOR_p := "11011" $$ "101" $$ ext_op_modrm_FPM32 "100" @ (fun x => FRSTOR x %% instruction_t).

  Definition FSCALE_p := "11011" $$ "001111" $$ bits "11101" @ (fun _ => FSCALE %% instruction_t).
  Definition FSIN_p := "11011" $$ "001111" $$ bits "11110" @ (fun _ => FSIN %% instruction_t).
  Definition FSINCOS_p := "11011" $$ "001111" $$ bits "11011" @ (fun _ => FSINCOS %% instruction_t).
  Definition FSQRT_p := "11011" $$ "001111" $$ bits "11010" @ (fun _ => FSQRT %% instruction_t).

  Definition FST_p := 
    "11011" $$ "001" $$ ext_op_modrm_FPM32 "010" @ (fun x => FST x %% instruction_t)
  |+|
    "11011" $$ "101" $$ ext_op_modrm_FPM64 "010" @ (fun x => FST x %% instruction_t)
  |+|
    "11011" $$ "101" $$ "11010" $$ fpu_reg @ (fun x => FST (FPS_op x) %% instruction_t).

  (* FSTCW's encoding is the same as FWAIT followed by FNSTCW *)
  (* Definition FSTCW_p := "10011011" $$ "11011" $$ "001" $$ ext_op_modrm_FPM32 "111" @ (fun x => FSTCW x %% instruction_t). *)
  Definition FSTENV_p := "11011" $$ "001" $$ ext_op_modrm_FPM32 "110" @ (fun x => FSTENV x %% instruction_t).
  Definition FSTP_p := 
    "11011" $$ "001" $$ ext_op_modrm_FPM32 "011" @ (fun x => FSTP x %% instruction_t)
  |+|
    "11011" $$ "101" $$ ext_op_modrm_FPM64 "011" @ (fun x => FSTP x %% instruction_t)
  |+|
    "11011" $$ "011" $$ ext_op_modrm_FPM80 "111" @ (fun x => FSTP x %% instruction_t) 
  |+|  
    "11011" $$ "101" $$ "11011" $$ fpu_reg @ (fun x => FSTP (FPS_op x) %% instruction_t). 

  Definition FSTSW_p := 
    "11011" $$ "111" $$ "111" $$ bits "00000" @ (fun _ => FSTSW None %% instruction_t)
  |+|
    "11011" $$ "101" $$ ext_op_modrm_FPM32 "111" @ (fun x => FSTSW (Some x) %% instruction_t).

  Definition FSUB_p :=
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "100" @ (fun x => FSUB (FPS_op Word.zero) x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "100" @ (fun x => FSUB (FPS_op Word.zero) x %% instruction_t) 
  |+|  
    "11011" $$ "0" $$ "00" $$ "111" $$ "0" $$ "0" $$ fpu_reg @ 
    (fun i => FSUB (FPS_op Word.zero) (FPS_op i) %% instruction_t)
  |+|  
    "11011" $$ "1" $$ "00" $$ "111" $$ "0" $$ "1" $$ fpu_reg @ 
    (fun i => FSUB (FPS_op i) (FPS_op Word.zero) %% instruction_t).

  Definition FSUBP_p := "11011" $$ "110" $$ "11101" $$ fpu_reg @ (fun x => FSUBP (FPS_op x) %% instruction_t).

  Definition FSUBR_p := 
    "11011" $$ "000" $$ ext_op_modrm_FPM32 "101" @ (fun x => FSUBR (FPS_op Word.zero) x %% instruction_t)
  |+|
    "11011" $$ "100" $$ ext_op_modrm_FPM64 "101" @ (fun x => FSUBR (FPS_op Word.zero) x %% instruction_t)
  |+|  
    "11011" $$ "0" $$ "00" $$ "111" $$ "0" $$ "1" $$ fpu_reg @ 
    (fun i => FSUBR (FPS_op Word.zero) (FPS_op i) %% instruction_t)
  |+|  
    "11011" $$ "1" $$ "00" $$ "111" $$ "0" $$ "0" $$ fpu_reg @ 
    (fun i => FSUBR (FPS_op i) (FPS_op Word.zero) %% instruction_t).

  Definition FSUBRP_p := "11011" $$ "110" $$ "11100" $$ fpu_reg @ (fun x => FSUBRP (FPS_op x) %% instruction_t). 
  Definition FTST_p := "11011" $$ "001111" $$ bits "00100" @ (fun _ => FTST %% instruction_t).
  Definition FUCOM_p := "11011" $$ "101" $$ "11100" $$ fpu_reg @ (fun x => FUCOM (FPS_op x) %% instruction_t). 
  Definition FUCOMP_p := "11011" $$ "101" $$ "11101" $$ fpu_reg @ (fun x => FUCOMP (FPS_op x) %% instruction_t). 
  Definition FUCOMPP_p := "11011" $$ "010111" $$ bits "01001" @ (fun _ => FUCOMPP %% instruction_t).
  Definition FUCOMI_p := "11011" $$ "011" $$ "11101" $$ fpu_reg @ (fun x => FUCOMI (FPS_op x) %% instruction_t).  
  Definition FUCOMIP_p := "11011" $$ "111" $$ "11101" $$ fpu_reg @ (fun x => FUCOMIP (FPS_op x) %% instruction_t). 
  Definition FXAM_p := "11011" $$ "001111" $$ bits "00101" @ (fun _ => FXAM %% instruction_t).
  Definition FXCH_p := "11011" $$ "001" $$ "11001" $$ fpu_reg @ (fun x => FXCH (FPS_op x) %% instruction_t). 

  Definition FXTRACT_p := "11011" $$ "001" $$ "1111" $$ bits "0100" @ (fun _ => FXTRACT %% instruction_t).
  Definition FYL2X_p := "11011" $$ "001111" $$ bits "10001" @ (fun _ => FYL2X %% instruction_t).
  Definition FYL2XP1_p := "11011" $$ "001111" $$ bits "11001" @ (fun _ => FYL2XP1 %% instruction_t).
  Definition FWAIT_p := bits "10011011" @ (fun _ => FWAIT %% instruction_t).
(*End of Floating-Point parsers*)

(*MMX Parsers*)

  (* parser for the mmx granularity bits; the byte granularity is allowed
     iff when byte is true; same as twob, fourb and eightb *)
  Definition mmx_gg_p (byte twob fourb eightb : bool) := 
    let byte_p := if byte then 
      bits "00" @ (fun _ => MMX_8 %% mmx_granularity_t)
      else @never mmx_granularity_t in
    let twobytes_p := if twob then 
      bits "01" @ (fun _ => MMX_16 %% mmx_granularity_t)
      else @never mmx_granularity_t in
    let fourbytes_p := if fourb then 
      bits "10" @ (fun _ => MMX_32 %% mmx_granularity_t)
      else @never mmx_granularity_t in
    let eightbytes_p := if eightb then 
      bits "11" @ (fun _ => MMX_64 %% mmx_granularity_t)
      else @never mmx_granularity_t in
    byte_p |+| twobytes_p |+| fourbytes_p |+| eightbytes_p.

  Definition EMMS_p := "0000" $$ "1111" $$ "0111" $$ bits "0111" @ (fun _ => EMMS %% instruction_t).
  Definition MOVD_p := 
    "0000" $$ "1111" $$ "0110" $$ "1110" $$ "11" $$ mmx_reg $ reg @ (*reg to mmxreg*)
    (fun p => let (m, r) := p in MOVD (GP_Reg_op r) (MMX_Reg_op m) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0111" $$ "1110" $$ "11" $$ mmx_reg $ reg @ (*reg from mmxreg*)
    (fun p => let (m, r) := p in MOVD (GP_Reg_op r) (MMX_Reg_op m) %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0110" $$ "1110" $$ modrm_mmx @ (*mem to mmxreg *)
    (fun p => let (op1, op2) := p in MOVD op1 op2 %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0111" $$ "1110" $$ modrm_mmx @ (*mem from mmxreg *)
    (fun p => let (mem, mmx) := p in MOVD mmx mem %% instruction_t).

  Definition MOVQ_p :=
    "0000" $$ "1111" $$ "0110" $$ "1111" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in MOVQ op1 op2 %% instruction_t)
  |+|
    "0000" $$ "1111" $$ "0111" $$ "1111" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in MOVQ op2 op1 %% instruction_t).

  Definition PACKSSDW_p := 
    "0000" $$ "1111" $$ "0110" $$ "1011" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PACKSSDW op1 op2 %% instruction_t).

  Definition PACKSSWB_p := 
    "0000" $$ "1111" $$ "0110" $$ "0011" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PACKSSWB op1 op2 %% instruction_t).

  Definition PACKUSWB_p := 
  "0000" $$ "1111" $$ "0110" $$ "0111" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PACKUSWB op1 op2 %% instruction_t).

  Definition PADD_p := 
  "0000" $$ "1111" $$ "1111" $$ "11" $$ mmx_gg_p true true true false $ modrm_mmx @ 
    (fun p => match p with (gg, (op1, op2)) => PADD gg op1 op2 end %% instruction_t).

  Definition PADDS_p := 
  "0000" $$ "1111" $$ "1110" $$ "11" $$ mmx_gg_p true true false false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PADDS gg op1 op2 end %% instruction_t).

  Definition PADDUS_p := 
  "0000" $$ "1111" $$ "1101" $$ "11" $$ mmx_gg_p true true false false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PADDUS gg op1 op2 end %% instruction_t).

  Definition PAND_p := 
  "0000" $$ "1111" $$ "1101" $$ "1011" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PAND op1 op2 %% instruction_t).

  Definition PANDN_p := 
  "0000" $$ "1111" $$ "1101" $$ "1111" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PANDN op1 op2 %% instruction_t).

  Definition PCMPEQ_p :=
  "0000" $$ "1111" $$ "0111" $$ "01" $$ mmx_gg_p true true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PCMPEQ gg op1 op2 end %% instruction_t).

  Definition PCMPGT_p := 
  "0000" $$ "1111" $$ "0110" $$ "01" $$ mmx_gg_p true true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PCMPGT gg op1 op2 end %% instruction_t).

  Definition PMADDWD_p := 
  "0000" $$ "1111" $$ "1111" $$ "0101" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PMADDWD op1 op2 %% instruction_t).

  Definition PMULHUW_p := 
  "0000" $$ "1111" $$ "1110" $$ "0100" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PMULHUW op1 op2 %% instruction_t).

  Definition PMULHW_p := 
  "0000" $$ "1111" $$ "1110" $$ "0101" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PMULHW op1 op2 %% instruction_t).

  Definition PMULLW_p := 
  "0000" $$ "1111" $$ "1101" $$ "0101" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PMULLW op1 op2 %% instruction_t).

  Definition POR_p := 
  "0000" $$ "1111" $$ "1110" $$ "1011" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in POR op1 op2 %% instruction_t).

  Definition PSLL_p := 
  "0000" $$ "1111" $$ "1111" $$ "00" $$ mmx_gg_p false true true true $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSLL gg op1 op2 end %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0111" $$ "00" $$ mmx_gg_p false true true true 
    $ "11110" $$ mmx_reg $ byte @ 
    (fun p => match p with (gg, (r, imm)) => PSLL gg (MMX_Reg_op r) (MMX_Imm_op (zero_extend8_32 imm)) end %% instruction_t).

  Definition PSRA_p :=
  "0000" $$ "1111" $$ "1110" $$ "00" $$ mmx_gg_p false true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSRA gg op1 op2 end %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0111" $$ "00" $$ mmx_gg_p false true true false 
    $ "11100" $$ mmx_reg $ byte @ 
    (fun p => match p with (gg, (r, imm)) => PSRA gg (MMX_Reg_op r) (MMX_Imm_op (zero_extend8_32 imm)) end %% instruction_t).

  Definition PSRL_p := 
  "0000" $$ "1111" $$ "1101" $$ "00" $$ mmx_gg_p false true true true $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSRL gg op1 op2 end %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0111" $$ "00" $$ mmx_gg_p false true true true
    $ "11010" $$ mmx_reg $ byte @ 
    (fun p => match p with (gg, (r, imm)) => PSRL gg (MMX_Reg_op r) (MMX_Imm_op (zero_extend8_32 imm)) end %% instruction_t).

  Definition PSUB_p := 
  "0000" $$ "1111" $$ "1111" $$ "10" $$ mmx_gg_p true true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSUB gg op1 op2 end %% instruction_t).

  Definition PSUBS_p := 
  "0000" $$ "1111" $$ "1110" $$ "10" $$ mmx_gg_p true true false false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSUBS gg op1 op2 end %% instruction_t).

  Definition PSUBUS_p := 
  "0000" $$ "1111" $$ "1101" $$ "10" $$ mmx_gg_p true true false false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PSUBUS gg op1 op2 end %% instruction_t).

  Definition PUNPCKH_p := 
  "0000" $$ "1111" $$ "0110" $$ "10" $$ mmx_gg_p true true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PUNPCKH gg op1 op2 end %% instruction_t).

  Definition PUNPCKL_p := 
  "0000" $$ "1111" $$ "0110" $$ "00" $$ mmx_gg_p true true true false $ modrm_mmx @
    (fun p => match p with (gg, (op1, op2)) => PUNPCKL gg op1 op2 end %% instruction_t).

  Definition PXOR_p := 
  "0000" $$ "1111" $$ "1110" $$ "1111" $$ modrm_mmx @ 
    (fun p => let (op1, op2) := p in PXOR op1 op2 %% instruction_t).
(*End of MMX parsers *)

(*SSE parsers*)
Definition ADDPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "1000" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in ADDPS op1 op2 %% instruction_t).

Definition ADDSS_p := 
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1000" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in ADDSS op1 op2 %% instruction_t).

Definition ANDNPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "0101" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in ANDNPS op1 op2 %% instruction_t).

Definition ANDPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "0100" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in ANDPS op1 op2 %% instruction_t).

Definition CMPPS_p := 
  "0000" $$ "1111" $$ "1100" $$ "0010" $$ modrm_xmm $ byte @ 
    (fun p => match p with ((op1, op2), imm)
                => CMPPS op1 op2 (zero_extend8_32 imm) end %% instruction_t).

Definition CMPSS_p := 
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "1100" $$ "0010" $$ modrm_xmm $ byte @ 
    (fun p => match p with ((op1, op2), imm)
                => CMPSS op1 op2 (zero_extend8_32 imm) end %% instruction_t).

Definition COMISS_p :=
  "0000" $$ "1111" $$ "0010" $$ "1111" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in COMISS op1 op2 %% instruction_t).

Definition CVTPI2PS_p :=
  "0000" $$ "1111" $$ "0010" $$ "1010" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in CVTPI2PS op1 op2 %% instruction_t).

Definition CVTPS2PI_p := 
  "0000" $$ "1111" $$ "0010" $$ "1101" $$ "11" $$ sse_reg $ mmx_reg @
    (fun p => let (sr, mr) := p in CVTPS2PI (SSE_XMM_Reg_op sr) (SSE_MM_Reg_op mr) %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0010" $$ "1101" $$ modrm_xmm_noreg @ 
    (fun p => let (xmm, mem) := p in CVTPS2PI xmm mem %% instruction_t).

Definition CVTSI2SS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1010" $$ "11" $$ sse_reg $ reg @
    (fun p => let (sr, r) := p in CVTSI2SS (SSE_XMM_Reg_op sr) (SSE_GP_Reg_op r) %% instruction_t)
  |+|
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1010" $$ modrm_xmm_noreg @ 
    (fun p => let (xmm, mem) := p in CVTSI2SS xmm mem %% instruction_t).

Definition CVTSS2SI_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1101" $$ "11" $$ reg $ sse_reg @
    (fun p => let (r, sr) := p in CVTSS2SI (SSE_GP_Reg_op r) (SSE_XMM_Reg_op sr) %% instruction_t)
  |+|
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1101" $$ modrm_xmm_r32_noreg @ 
    (fun p => let (op1, mem) := p in CVTSS2SI op1 mem %% instruction_t).

Definition CVTTPS2PI_p :=
  "0000" $$ "1111" $$ "0010" $$ "1100" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in CVTTPS2PI op1 op2 %% instruction_t).

Definition CVTTSS2SI_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1100" $$ "11" $$ reg $ sse_reg @
    (fun p => let (r, sr) := p in CVTTSS2SI (SSE_GP_Reg_op r) (SSE_XMM_Reg_op sr) %% instruction_t)
  |+|
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0010" $$ "1100" $$ modrm_xmm_r32_noreg @ 
    (fun p => let (op1, mem) := p in CVTTSS2SI op1 mem %% instruction_t).

Definition DIVPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "1110" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in DIVPS op1 op2 %% instruction_t).

Definition DIVSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1110" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in DIVSS op1 op2 %% instruction_t).

Definition LDMXCSR_p := 
  "0000" $$ "1111" $$ "1010" $$ "1110" $$ ext_op_modrm_sse "010" @ (fun x => LDMXCSR x %% instruction_t).

Definition MAXPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "1111" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MAXPS op1 op2 %% instruction_t).

Definition MAXSS_p := 
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1111" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MAXSS op1 op2 %% instruction_t).

Definition MINPS_p := 
  "0000" $$ "1111" $$ "0101" $$ "1101" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MINPS op1 op2 %% instruction_t).

Definition MINSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1101" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MINSS op1 op2 %% instruction_t).

Definition MOVAPS_p :=
  "0000" $$ "1111" $$ "0010" $$ "1000" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVAPS op1 op2 %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0010" $$ "1001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVAPS op1 op2 %% instruction_t).

Definition MOVHLPS_p :=
  "0000" $$ "1111" $$ "0001" $$ "0010" $$ "11" $$ sse_reg $ sse_reg @
    (fun p => let (sr1, sr2) := p in MOVHLPS (SSE_XMM_Reg_op sr1) (SSE_XMM_Reg_op sr2) %% instruction_t).

Definition MOVHPS_p := 
  "0000" $$ "1111" $$ "0001" $$ "0110" $$ modrm_xmm_noreg @ 
    (fun p => let (op1, mem) := p in MOVHPS op1 mem %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0001" $$ "0111" $$ modrm_xmm_noreg @ 
    (fun p => let (op1, mem) := p in MOVHPS mem op1 %% instruction_t).

Definition MOVLHPS_p :=
  "0000" $$ "1111" $$ "0001" $$ "0110" $$ "11" $$ sse_reg $ sse_reg @
    (fun p => let (sr1, sr2) := p in MOVLHPS (SSE_XMM_Reg_op sr1) (SSE_XMM_Reg_op sr2) %% instruction_t).

Definition MOVLPS_p :=
  "0000" $$ "1111" $$ "0001" $$ "0010" $$ modrm_xmm_noreg @ 
    (fun p => let (op1, mem) := p in MOVLPS op1 mem %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0001" $$ "0011" $$ modrm_xmm_noreg @ 
    (fun p => let (op1, mem) := p in MOVLPS mem op1 %% instruction_t).

Definition MOVMSKPS_p := 
  "0000" $$ "1111" $$ "0001" $$ "0110" $$ "11" $$ reg $ sse_reg @
    (fun p => let (r, sr) := p in MOVMSKPS (SSE_GP_Reg_op r) (SSE_XMM_Reg_op sr) %% instruction_t).

Definition MOVSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0001" $$ "0000" $$  modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVSS op1 op2 %% instruction_t)
  |+|
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0001" $$ "0001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVSS op2 op1 %% instruction_t).

Definition MOVUPS_p := 
  "0000" $$ "1111" $$ "0001" $$ "0000" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVUPS op1 op2 %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "0001" $$ "0001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MOVUPS op2 op1 %% instruction_t).

Definition MULPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "1001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MULPS op1 op2 %% instruction_t).

Definition MULSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in MULSS op1 op2 %% instruction_t).

Definition ORPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "0110" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in ORPS op1 op2 %% instruction_t).

Definition RCPPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "0011" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in RCPPS op1 op2 %% instruction_t).

Definition RCPSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "0011" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in RCPSS op1 op2 %% instruction_t).

Definition RSQRTPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "0010" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in RSQRTPS op1 op2 %% instruction_t).

Definition RSQRTSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "0010" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in RSQRTSS op1 op2 %% instruction_t).

Definition SHUFPS_p :=
  "0000" $$ "1111" $$ "1100" $$ "0110" $$ modrm_xmm $ byte @ 
    (fun p => match p with ((op1, op2), imm)
                => SHUFPS op1 op2 (zero_extend8_32 imm) end %% instruction_t).

Definition SQRTPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "0001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in SQRTPS op1 op2 %% instruction_t).

Definition SQRTSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "0001" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in SQRTSS op1 op2 %% instruction_t).

Definition STMXCSR_p := 
  "0000" $$ "1111" $$ "1010" $$ "1110" $$ ext_op_modrm_sse "011" @ (fun x => STMXCSR x %% instruction_t).

Definition SUBPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "1100" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in SUBPS op1 op2 %% instruction_t).

Definition SUBSS_p :=
  "1111" $$ "0011" $$ "0000" $$ "1111" $$ "0101" $$ "1100" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in SUBSS op1 op2 %% instruction_t).

Definition UCOMISS_p :=
  "0000" $$ "1111" $$ "0010" $$ "1110" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in UCOMISS op1 op2 %% instruction_t).

Definition UNPCKHPS_p :=
  "0000" $$ "1111" $$ "0001" $$ "0101" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in UNPCKHPS op1 op2 %% instruction_t).

Definition UNPCKLPS_p :=
  "0000" $$ "1111" $$ "0001" $$ "0100" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in UNPCKLPS op1 op2 %% instruction_t).

Definition XORPS_p :=
  "0000" $$ "1111" $$ "0101" $$ "0111" $$ modrm_xmm @ 
    (fun p => let (op1, op2) := p in XORPS op1 op2 %% instruction_t).

(* todo: this needs to take operand-override prefix into account *)
Definition PAVGB_p :=
  "0000" $$ "1111" $$ "1110" $$ "0000" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PAVGB op1 op2 %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "1110" $$ "0011" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PAVGB op2 op1 %% instruction_t).

Definition PEXTRW_p :=
  "0000" $$ "1111" $$ "1100" $$ "0101" $$ "11" $$ reg $ mmx_reg $ byte @
    (fun p => match p with (r32, (mmx, imm))
                => PEXTRW (SSE_GP_Reg_op r32) (SSE_MM_Reg_op mmx) (zero_extend8_32 imm) end %% instruction_t).

Definition PINSRW_p :=
  "0000" $$ "1111" $$ "1100" $$ "0100" $$ "11" $$ mmx_reg $ reg $ byte @
    (fun p => match p with (mmx, (r32, imm)) => PINSRW (SSE_MM_Reg_op mmx) (SSE_GP_Reg_op r32) (zero_extend8_32 imm) end %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "1100" $$ "0100" $$ modrm_mm_noreg $ byte @ 
    (fun p => match p with ((op1, mem), imm) => PINSRW op1 mem (zero_extend8_32 imm) end %% instruction_t).

Definition PMAXSW_p :=
  "0000" $$ "1111" $$ "1110" $$ "1110" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PMAXSW op1 op2 %% instruction_t).

Definition PMAXUB_p :=
  "0000" $$ "1111" $$ "1101" $$ "1110" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PMAXUB op1 op2 %% instruction_t).

Definition PMINSW_p :=
  "0000" $$ "1111" $$ "1110" $$ "1010" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PMINSW op1 op2 %% instruction_t).

Definition PMINUB_p :=
  "0000" $$ "1111" $$ "1101" $$ "1010" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PMINUB op1 op2 %% instruction_t).

Definition PMOVMSKB_p :=
  "0000" $$ "1111" $$ "1101" $$ "0111" $$ "11" $$ reg $ mmx_reg @
    (fun p => let (r, mr) := p in PMOVMSKB (SSE_GP_Reg_op r) (SSE_MM_Reg_op mr) %% instruction_t).

(*
  Already done in MMX parser section

 Definition PMULHUW_p :=
  "0000" $$ "1111" $$ "1110" $$ "0100" $$ "11" $$ mmx_reg $ mmx_reg @
    (fun p => let (a, b) := p in PMULHUW (SSE_MM_Reg_op a) (SSE_MM_Reg_op b) %% instruction_t)
  |+|
  "0000" $$ "1111" $$ "1110" $$ "0100" $$ modrm_mm @ 
    (fun p => let (mem, mmx) := p in PMULHUW mem mmx %% instruction_t).
*)
Definition PSADBW_p :=
  "0000" $$ "1111" $$ "1111" $$ "0110" $$ modrm_mm @ 
    (fun p => let (op1, op2) := p in PSADBW op1 op2 %% instruction_t).

Definition PSHUFW_p :=
  "0000" $$ "1111" $$ "0111" $$ "0000" $$ modrm_mm $ byte @ 
    (fun p => match p with ((op1, op2), imm)
                => PSHUFW op1 op2 (zero_extend8_32 imm) end %% instruction_t).

Definition MASKMOVQ_p :=
  "0000" $$ "1111" $$ "1111" $$ "0111" $$ "11" $$ mmx_reg $ mmx_reg @
    (fun p => let (mr1, mr2) := p in MASKMOVQ (SSE_MM_Reg_op mr1) (SSE_MM_Reg_op mr2) %% instruction_t).

Definition MOVNTPS_p :=
  "0000" $$ "1111" $$ "0010" $$ "1011" $$ modrm_xmm_noreg @ 
    (fun p => let (op1, mem) := p in MOVNTPS mem op1 %% instruction_t).

Definition MOVNTQ_p :=
  "0000" $$ "1111" $$ "1110" $$ "0111" $$ modrm_mm_noreg @ 
    (fun p => let (op1, mem) := p in MOVNTQ mem op1 %% instruction_t).

Definition PREFETCHT0_p :=
  "0000" $$ "1111" $$ "0001" $$ "1000" $$ ext_op_modrm_sse "001" @ (fun x => PREFETCHT0 x %% instruction_t).

Definition PREFETCHT1_p :=
  "0000" $$ "1111" $$ "0001" $$ "1000" $$ ext_op_modrm_sse "010" @ (fun x => PREFETCHT1 x %% instruction_t).

Definition PREFETCHT2_p := 
  "0000" $$ "1111" $$ "0001" $$ "1000" $$ ext_op_modrm_sse "011" @ (fun x => PREFETCHT2 x %% instruction_t).

Definition PREFETCHNTA_p :=
  "0000" $$ "1111" $$ "0001" $$ "1000" $$ ext_op_modrm_sse "000" @ (fun x => PREFETCHNTA x %% instruction_t).

Definition SFENCE_p := "0000" $$ "1111" $$ "1010" $$ "1110" $$ "1111" $$ 
                                   bits "1000" @ (fun _ => SFENCE %% instruction_t).

  (* Now glue all of the individual instruction parsers together into 
     one big parser.  *)
  
  Fixpoint list2pair_t (l: list result) :=
    match l with
      | nil => unit_t
      | r::r'::nil => pair_t r r'
      | r::l' => pair_t r (list2pair_t l')
    end.
 
  Definition lock_p : parser lock_or_rep_t :=
    "1111" $$ bits "0000" @ (fun _ => lock %% lock_or_rep_t).

  Definition repn_p : parser lock_or_rep_t :=
    "1111" $$ bits "0010" @ (fun _ => repn %% lock_or_rep_t).

  Definition rep_p : parser lock_or_rep_t :=
    "1111" $$ bits "0011" @ (fun _ => rep  %% lock_or_rep_t).

  Definition lock_or_rep_p : parser lock_or_rep_t :=
    ("1111" $$ ( bits "0000" @ (fun _ => lock %% lock_or_rep_t)
                 |+| bits "0010" @ (fun _ => repn %% lock_or_rep_t)
                 |+| bits "0011" @ (fun _ => rep  %% lock_or_rep_t))).

  Definition segment_override_p : parser segment_register_t :=
  ("0010" $$ bits "1110" @ (fun _ => CS %% segment_register_t)
    |+| "0011" $$ bits "0110" @ (fun _ => SS %% segment_register_t)
    |+| "0011" $$ bits "1110" @ (fun _ => DS %% segment_register_t)
    |+| "0010" $$ bits "0110" @ (fun _ => ES %% segment_register_t)
    |+| "0110" $$ bits "0100" @ (fun _ => FS %% segment_register_t)
    |+| "0110" $$ bits "0101" @ (fun _ => GS %% segment_register_t)).

  Definition op_override_p : parser bool_t :=
    "0110" $$ bits "0110" @ (fun _ => true %% bool_t).
  Definition addr_override_p : parser bool_t :=
    "0110" $$ bits "0111" @ (fun _ => true %% bool_t).

  (* Ok, now I want all permutations of the above four parsers. 
     I make a little perm2 combinator that takes two parsers and gives you
     p1 $ p2 |+| p2 $ p1, making sure to swap the results in the second case *)
  
  Definition perm2 t1 t2 (p1: parser t1) (p2: parser t2) : parser (pair_t t1 t2) :=
      p1 $ p2 |+|
      p2 $ p1 @ (fun p => match p with (a, b) => (b, a) %% pair_t t1 t2 end).

  (* Then I build that up into a perm3 and perm4. One could make a recursive
     function to do this, but I didn't want to bother with the necessary
     proofs and type-system juggling.*) 

  Definition perm3 t1 t2 t3 (p1: parser t1) (p2: parser t2) (p3: parser t3)
    : parser (pair_t t1 (pair_t t2 t3)) :=
    let r_t := pair_t t1 (pair_t t2 t3) in
       p1 $ (perm2 p2 p3)
   |+| p2 $ (perm2 p1 p3) @ (fun p => match p with (b, (a, c)) => (a, (b, c)) %% r_t end)
   |+| p3 $ (perm2 p1 p2) @ (fun p => match p with (c, (a, b)) => (a, (b, c)) %% r_t end).

  Definition perm4 t1 t2 t3 t4 (p1: parser t1) (p2: parser t2) (p3: parser t3)
    (p4: parser t4) : parser (pair_t t1 (pair_t t2 (pair_t t3 t4))) :=
    let r_t := pair_t t1 (pair_t t2 (pair_t t3 t4)) in
       p1 $ (perm3 p2 p3 p4)
   |+| p2 $ (perm3 p1 p3 p4) @ 
         (fun p => match p with (b, (a, (c, d))) => (a, (b, (c, d))) %% r_t end)
   |+| p3 $ (perm3 p1 p2 p4) @ 
         (fun p => match p with (c, (a, (b, d))) => (a, (b, (c, d))) %% r_t end)
   |+| p4 $ (perm3 p1 p2 p3) @ 
         (fun p => match p with (d, (a, (b, c))) => (a, (b, (c, d))) %% r_t end). 

  (* In this case, prefixes are optional. Before, each of the above
     parsing rules for the prefixes accepted Eps, and this was how we
     handled this.  However, if the parsers you join with perm can
     each accept Eps, then the result is a _highly_ ambiguous parser.

     Instead we have a different combinator, called option_perm, that 
     handles this without introducing extra ambiguity *)

  Definition option_perm t1 (p1: parser (tipe_t t1)) 
     : parser (option_t t1) :=
     let r_t := option_t t1 in 
         Eps_p @ (fun p => None %% r_t)  
     |+| p1 @ (fun p => (Some p) %% r_t ).


  (* This signature is slightly awkward - because there's no result
     type corresponding to option (and I'm hesitant to add it to
     Parser at the moment) we can't just have a signature like parser
     t1 -> parser t2 -> parser (option_t t1) (option_t t2)) *)
    
  Definition option_perm2 t1 t2 (p1: parser (tipe_t t1)) (p2: parser (tipe_t t2)) 
     : parser (pair_t (option_t t1) (option_t t2)) :=
     let r_t := pair_t (option_t t1) (option_t t2) in 
         Eps_p @ (fun p => (None, None) %% r_t)  
     |+| p1 @ (fun p => (Some p, None) %% r_t ) 
     |+| p2 @ (fun p => (None, Some p) %% r_t) 
     |+| perm2 p1 p2 @ (fun p => match p with (a, b) => (Some a, Some b) %%r_t end). 

  Definition option_perm3 t1 t2 t3 (p1:parser(tipe_t t1)) (p2:parser(tipe_t t2))
    (p3:parser(tipe_t t3)): parser(pair_t(option_t t1)(pair_t(option_t t2) (option_t t3)))
    :=
    let r_t := pair_t(option_t t1)(pair_t(option_t t2) (option_t t3))  in
        Eps_p @ (fun p => (None, (None, None)) %% r_t)
    |+| p1 @ (fun p => (Some p, (None, None)) %% r_t)
    |+| p2 @ (fun p => (None, (Some p, None)) %% r_t)
    |+| p3 @ (fun p => (None, (None, Some p)) %% r_t)
    |+| perm2 p1 p2 @(fun p => match p with (a, b) => (Some a, (Some b, None)) %%r_t end)
    |+| perm2 p1 p3 @(fun p => match p with (a, c) => (Some a, (None, Some c)) %%r_t end)
    |+| perm2 p2 p3 @(fun p => match p with (b, c) => (None, (Some b, Some c)) %%r_t end)
    |+| perm3 p1 p2 p3 @ (fun p => match p with (a, (b, c))
                                    => (Some a, (Some b, Some c)) %%r_t end).

  (* t1 is optional, but t2 is a must *)
  Definition option_perm2_variation t1 t2 (p1: parser (tipe_t t1))
    (p2: parser (tipe_t t2)) 
     : parser (pair_t (option_t t1) (tipe_t t2)) :=
     let r_t := pair_t (option_t t1) (tipe_t t2) in 
         p2 @ (fun p => (None, p) %% r_t) 
     |+| perm2 p1 p2 @ (fun p => match p with (a, b) => (Some a, b) %%r_t end). 

  (* in this def, t1 and t2 are optional, but t3 is a must *)
  Definition option_perm3_variation t1 t2 t3 (p1:parser(tipe_t t1)) (p2:parser(tipe_t t2))
    (p3:parser(tipe_t t3)): parser(pair_t(option_t t1)(pair_t(option_t t2) (tipe_t t3)))
    :=
    let r_t := pair_t(option_t t1)(pair_t(option_t t2) (tipe_t t3))  in
        p3 @ (fun p => (None, (None, p)) %% r_t)
    |+| perm2 p1 p3 @(fun p => match p with (a, c) => (Some a, (None, c)) %%r_t end)
    |+| perm2 p2 p3 @(fun p => match p with (b, c) => (None, (Some b, c)) %%r_t end)
    |+| perm3 p1 p2 p3 @ (fun p => match p with (a, (b, c))
                                    => (Some a, (Some b, c)) %%r_t end).

  (* This is beginning to get quite nasty. Someone should write a form for arbitrary
     n and prove it's correct :) *)
  Definition option_perm4 t1 t2 t3 t4 (p1:parser(tipe_t t1)) (p2: parser(tipe_t t2))
    (p3: parser(tipe_t t3)) (p4: parser(tipe_t t4)) :
      parser(pair_t(option_t t1) (pair_t(option_t t2) (pair_t(option_t t3) (option_t t4))))
      := 
    let r_t := pair_t(option_t t1) (pair_t(option_t t2)
      (pair_t(option_t t3)(option_t t4))) in
        Eps_p @ (fun p => (None, (None, (None, None))) %% r_t)
    |+| p1 @ (fun p => (Some p, (None, (None, None))) %% r_t)
    |+| p2 @ (fun p => (None, (Some p, (None, None))) %% r_t)
    |+| p3 @ (fun p => (None, (None, (Some p, None))) %% r_t)
    |+| p4 @ (fun p => (None, (None, (None, Some p))) %% r_t)
    |+| perm2 p1 p2 @ (fun p => match p with (a, b)
                                  => (Some a, (Some b, (None, None))) %% r_t end)
    |+| perm2 p1 p3 @ (fun p => match p with (a, c)
                                  => (Some a, (None, (Some c, None))) %% r_t end)
    |+| perm2 p1 p4 @ (fun p => match p with (a, d)
                                  => (Some a, (None, (None, Some d))) %% r_t end)
    |+| perm2 p2 p3 @ (fun p => match p with (b, c)
                                  => (None, (Some b, (Some c, None))) %% r_t end)
    |+| perm2 p2 p4 @ (fun p => match p with (b, d)
                                  => (None, (Some b, (None, Some d))) %% r_t end)
    |+| perm2 p3 p4 @ (fun p => match p with (c, d)
                                  => (None, (None, (Some c, Some d))) %% r_t end)
    |+| perm3 p1 p2 p3 @ (fun p => match p with (a, (b, c))
                                    => (Some a, (Some b, (Some c, None))) %%r_t end)
    |+| perm3 p1 p3 p4 @ (fun p => match p with (a, (c, d))
                                    => (Some a, (None, (Some c, Some d))) %%r_t end)
    |+| perm3 p1 p2 p4 @ (fun p => match p with (a, (b, d))
                                    => (Some a, (Some b, (None, Some d))) %%r_t end)
    |+| perm3 p2 p3 p4 @ (fun p => match p with (b, (c, d))
                                    => (None, (Some b, (Some c, Some d))) %%r_t end)
    |+| perm4 p1 p2 p3 p4 @ (fun p => match p with (a, (b, (c, d)))
                                        => (Some a, (Some b, (Some c, Some d))) %% r_t end).
                                      
  Definition opt2b (a: option bool) (default: bool) :=
    match a with
      | Some b => b
      | None => default
    end.


  Definition prefix_parser_rep :=
    option_perm3 rep_p segment_override_p op_override_p @
     (fun p => match p with (l, (s, op)) =>
                 mkPrefix l s (opt2b op false) false %% prefix_t end).

  (* this set of instructions can take prefixes in prefix_parser_rep;
     that is, in lock_or_rep, only rep can be used *)
  Definition instr_parsers_rep :=
    INS_p :: OUTS_p :: MOVS_p :: LODS_p :: STOS_p :: nil.

  Definition prefix_parser_repn :=
    option_perm3 repn_p segment_override_p op_override_p @
     (fun p => match p with (l, (s, op)) =>
                 mkPrefix l s (opt2b op false) false %% prefix_t end).

  (* this set of instructions can take prefixes in prefix_parser_repn;
     that is, in lock_or_rep, only repn can be used *)
  Definition instr_parsers_repn := CMPS_p :: SCAS_p :: nil.

  Definition prefix_parser_lock_with_op_override :=
    option_perm3_variation lock_p segment_override_p op_override_p @
     (fun p => match p with (l, (s, op)) =>
                 mkPrefix l s op false %% prefix_t end).

  (* this set of instructions can take prefixes in 
     prefix_parser_lock_with_op_override;
     that is, in lock_or_rep, only lock can be used;
     and op_override prefix *must* be used *)
  Definition instr_parsers_lock_with_op_override := 
    ADD_p true :: ADC_p true :: AND_p true :: NEG_p :: NOT_p :: OR_p true
    :: SBB_p true :: SUB_p true :: XOR_p true :: XCHG_p :: nil.

  Definition prefix_parser_lock_no_op_override :=
    option_perm2 lock_p segment_override_p @
     (fun p => match p with (l, s) =>
                 mkPrefix l s false false %% prefix_t end).

  (* this set of instructions can take prefixes in 
     prefix_parser_lock_no_op_override;
     that is, in lock_or_rep, only lock can be used;
     and op_override prefix *must not* be used *)
  Definition instr_parsers_lock_no_op_override := 
    ADD_p false :: ADC_p false :: AND_p false :: BTC_p :: BTR_p :: 
    BTS_p :: CMPXCHG_p :: DEC_p :: INC_p :: NEG_p :: NOT_p :: OR_p false
    :: SBB_p false :: SUB_p false :: XOR_p false :: XADD_p :: XCHG_p :: nil.

  Definition prefix_parser_seg_with_op_override := 
    option_perm2_variation segment_override_p op_override_p @
     (fun p => match p with (s, op) =>
                 mkPrefix None s op false %% prefix_t end).

  (* this set of instructions can take prefixes in 
     prefix_parser_seg_with_op_override;
     that is, it cannot take a lock_or_rep prefix, must take op_override
     prefix, can optionally take segment-override prefix *)
  Definition instr_parsers_seg_with_op_override := 
    CMP_p true ::  IMUL_p true :: MOV_p true :: TEST_p true :: nil.

  Definition prefix_parser_seg_op_override :=
    option_perm2 segment_override_p op_override_p @
     (fun p => match p with (s, op) =>
                 mkPrefix None s (opt2b op false) false %% prefix_t end).

  (* this set of instructions can take prefixes in 
     prefix_parser_seg_op_override;
     that is, it cannot take a lock_or_rep prefix, but can
     optionally take segment or op override prefix *)
  Definition instr_parsers_seg_op_override := 
    SAR_p :: SHL_p :: SHLD_p :: SHR_p :: SHRD_p :: 
    MOVSX_p :: MOVZX_p :: DIV_p :: IDIV_p :: 
    CDQ_p :: CWDE_p :: MUL_p :: nil.

  Definition prefix_parser_seg_override :=
    option_perm segment_override_p @
     (fun s => mkPrefix None s false false %% prefix_t).

  (* this set of instructions can take only the seg_override prefix *)
  Definition instr_parsers_seg_override := 
    AAA_p :: AAD_p :: AAM_p :: AAS_p :: CMP_p false ::
    ARPL_p :: BOUND_p :: BSF_p :: BSR_p :: BSWAP_p :: BT_p :: 
    CALL_p :: CLC_p :: CLD_p :: CLI_p :: CMOVcc_p :: CMC_p :: CPUID_p :: DAA_p :: 
    HLT_p :: IMUL_p false :: IN_p :: INTn_p :: INT_p :: INTO_p :: INVD_p :: INVLPG_p :: IRET_p :: Jcc_p :: JCXZ_p :: JMP_p :: 
    LAHF_p :: LAR_p :: LDS_p :: LEA_p :: LEAVE_p :: LES_p :: LFS_p :: LGDT_p :: LGS_p :: LIDT_p :: LLDT_p :: LMSW_p :: 
    LOOP_p :: LOOPZ_p :: LOOPNZ_p :: LSL_p :: LSS_p :: LTR_p :: MOV_p false :: MOVCR_p :: MOVDR_p :: 
    MOVSR_p :: MOVBE_p :: (* NOP_p :: *)  OUT_p :: POP_p :: POPSR_p :: POPA_p :: POPF_p ::
    PUSH_p :: PUSHSR_p :: PUSHA_p :: PUSHF_p :: RCL_p :: RCR_p :: RDMSR_p :: RDPMC_p :: RDTSC_p :: RDTSCP_p :: 
    RET_p :: ROL_p :: ROR_p ::
    RSM_p :: SAHF_p :: SETcc_p :: SGDT_p :: SIDT_p :: SLDT_p :: SMSW_p :: STC_p :: STD_p :: STI_p :: 
    STR_p :: TEST_p false :: UD2_p :: VERR_p :: VERW_p :: WBINVD_p :: WRMSR_p :: XLAT_p :: F2XM1_p ::
    FABS_p :: FADD_p :: FADDP_p :: FBLD_p :: FBSTP_p :: FCHS_p :: FCLEX_p :: FCOM_p :: FCOMP_p :: FCOMPP_p :: FCOMIP_p :: FCOS_p :: FDECSTP_p ::
    FDIV_p :: FDIVP_p :: FDIVR_p :: FDIVRP_p :: FFREE_p :: FIADD_p :: FICOM_p :: FICOMP_p :: FIDIV_p :: FIDIVR_p :: FILD_p :: FIMUL_p :: FINCSTP_p
    (*:: FINIT_p *) :: FIST_p :: FISTP_p :: FISUB_p :: FISUBR_p :: FLD_p :: FLD1_p :: FLDCW_p :: FLDENV_p :: FLDL2E_p :: FLDL2T_p :: FLDLG2_p :: FLDLN2_p
    :: FLDPI_p :: FLDZ_p :: FMUL_p :: FMULP_p :: FNOP_p :: FNSAVE_p :: FNSTCW_p :: FPATAN_p :: FPREM_p :: FPREM1_p :: FPTAN_p :: FRNDINT_p :: FRSTOR_p :: (* FSAVE_p :: *) 
    FSCALE_p :: 
    FSIN_p :: FSINCOS_p :: FSQRT_p :: FST_p :: (* FSTCW_p :: *) FSTENV_p :: FSTP_p :: FSTSW_p :: FSUB_p :: FSUBP_p :: FSUBR_p :: FSUBRP_p ::FTST_p ::
    FUCOM_p :: FUCOMP_p :: FUCOMPP_p :: FUCOMI_p :: FUCOMIP_p :: FXAM_p :: FXCH_p :: FXTRACT_p :: FYL2X_p :: FYL2XP1_p :: FWAIT_p :: 
    EMMS_p :: MOVD_p :: MOVQ_p :: PACKSSDW_p :: PACKSSWB_p :: PACKUSWB_p :: PADD_p :: PADDS_p :: PADDUS_p :: PAND_p :: PANDN_p :: PCMPEQ_p :: PCMPGT_p :: 
    PMADDWD_p :: PMULHUW_p :: PMULHW_p :: PMULLW_p :: POR_p :: PSLL_p :: PSRA_p :: PSRL_p :: PSUB_p :: PSUBS_p :: PSUBUS_p :: PUNPCKH_p :: PUNPCKL_p :: PXOR_p :: 
    ADDPS_p :: ADDSS_p :: ANDNPS_p :: ANDPS_p :: CMPPS_p :: CMPSS_p :: COMISS_p :: CVTPI2PS_p :: CVTPS2PI_p :: CVTSI2SS_p :: CVTSS2SI_p :: CVTTPS2PI_p :: CVTTSS2SI_p ::
    DIVPS_p :: DIVSS_p :: LDMXCSR_p :: MAXPS_p :: MAXSS_p :: MINPS_p :: MINSS_p :: MOVAPS_p :: MOVHLPS_p :: MOVLPS_p :: MOVMSKPS_p :: MOVSS_p :: MOVUPS_p :: MULPS_p ::
    MULSS_p :: ORPS_p :: RCPPS_p :: RCPSS_p :: RSQRTPS_p :: RSQRTSS_p :: SHUFPS_p :: SQRTPS_p :: SQRTSS_p :: STMXCSR_p :: SUBPS_p :: SUBSS_p :: UCOMISS_p :: UNPCKHPS_p ::
    UNPCKLPS_p :: XORPS_p :: PAVGB_p :: PEXTRW_p :: PINSRW_p :: PMAXSW_p :: PMAXUB_p :: PMINSW_p :: PMINUB_p :: PMOVMSKB_p :: PSADBW_p :: PSHUFW_p :: MASKMOVQ_p ::
    MOVNTPS_p :: MOVNTQ_p :: PREFETCHT0_p :: PREFETCHT1_p :: PREFETCHT2_p :: PREFETCHNTA_p :: SFENCE_p :: nil.


  Definition instruction_parser_list := 
    (List.map (fun (p:parser instruction_t) => prefix_parser_rep $ p)
      instr_parsers_rep) ++
    (List.map (fun (p:parser instruction_t) => prefix_parser_repn $ p)
      instr_parsers_repn) ++
    (List.map (fun (p:parser instruction_t)
                => prefix_parser_lock_with_op_override $ p)
      instr_parsers_lock_with_op_override) ++
    (List.map (fun (p:parser instruction_t)
                => prefix_parser_lock_no_op_override $ p)
      instr_parsers_lock_no_op_override) ++
    (List.map (fun (p:parser instruction_t)
                => prefix_parser_seg_with_op_override $ p)
      instr_parsers_seg_with_op_override) ++
    (List.map (fun (p:parser instruction_t)
                => prefix_parser_seg_op_override $ p)
      instr_parsers_seg_op_override) ++
    (List.map (fun (p:parser instruction_t)
                => prefix_parser_seg_override $ p)
      instr_parsers_seg_override).

  Definition instruction_parser := alts instruction_parser_list.

  Definition instruction_regexp_pair := parser2regexp instruction_parser.
  Record instParserState := mkPS { 
    inst_ctxt : ctxt_t ; 
    inst_regexp : regexp (pair_t prefix_t instruction_t) ; 
    inst_regexp_wf : wf_regexp inst_ctxt inst_regexp 
  }.

  Definition initial_parser_state : instParserState := 
    mkPS (snd instruction_regexp_pair) (fst instruction_regexp_pair) 
    (p2r_wf instruction_parser _).

  Definition byte_explode (b:int8) : list bool := 
  let bs := Word.bits_of_Z 8 (Word.unsigned b) in
    (bs 7)::(bs 6)::(bs 5)::(bs 4)::(bs 3)::(bs 2)::(bs 1)::(bs 0)::nil.

  Definition parse_byte (ps:instParserState) (b:int8) : 
    instParserState * list (prefix * instr) := 
    let cs := byte_explode b in
    let r' := deriv_parse' (inst_regexp ps) cs in
    let wf' := wf_derivs (inst_ctxt ps) cs (inst_regexp ps) (inst_regexp_wf ps) in
      (mkPS (inst_ctxt ps) r' wf', apply_null (inst_ctxt ps) r' wf').

End X86_PARSER.
