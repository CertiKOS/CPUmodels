(* Copyright (c) 2011. Greg Morrisett, Gang Tan, Joseph Tassarotti, 
   Jean-Baptiste Tristan, and Edward Gan.

   This file is part of RockSalt.

   This file is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.
*)

   
(* This file provides abstract syntax definitions for the MIPS32
 * instruction set architecture. *)
Require Import List.
Require Import ZArith.
Set Implicit Arguments.
Local Open Scope Z_scope.

Require Import Shared.Bits.

(********************************************)
(* Type definitions for MIPS abstract syntax *)
(********************************************)
Definition zero_extend8_32(w:int8) : int32 := Word.repr (Word.unsigned w).
Definition sign_extend8_32(w:int8) : int32 := Word.repr (Word.signed w).
Definition zero_extend16_32(w:int16) : int32 := Word.repr (Word.unsigned w).
Definition sign_extend16_32(w:int16) : int32 := Word.repr (Word.signed w).

Inductive register : Set := 
| Reg : int5 -> register.

Definition register_eq_dec : forall (x y:register), {x=y} + {x<>y}.
  intros.
  decide equality.
  apply Word.eq_dec.
Defined.

Inductive ioperand : Set := 
| Iop : register -> register -> int16 -> ioperand
.
Inductive joperand : Set :=
| Jop : int26 -> joperand
.
Inductive roperand : Set :=
| Rop : register -> register -> register -> int5 -> roperand
.

(* Operands for bgez, bgezal, ...; they compare a register with zero
   and conditionally make a pc-relative jump based on an offset
   operand *)
Inductive bz_operand : Set :=
| BZop : register -> int16 -> bz_operand.

Inductive instr : Set :=
| ADD : roperand -> instr
| ADDI : ioperand -> instr
| ADDIU : ioperand -> instr
| ADDU : roperand -> instr
| AND : roperand -> instr
| ANDI : ioperand -> instr
| BEQ : ioperand -> instr
| BGEZ : bz_operand -> instr
| BGEZAL : bz_operand -> instr
| BGTZ : bz_operand -> instr
| BLEZ : bz_operand -> instr
| BLTZ : bz_operand -> instr
| BLTZAL : bz_operand -> instr
| BNE : ioperand -> instr
| DIV : roperand -> instr
| DIVU : roperand -> instr
| J : joperand -> instr
| JAL : joperand -> instr
| JALR : roperand -> instr
| JR : roperand -> instr
| LB : ioperand -> instr
| LBU : ioperand -> instr
| LH : ioperand -> instr
| LHU : ioperand -> instr
| LUI : ioperand -> instr
| LW : ioperand -> instr
| MFHI : roperand -> instr
| MFLO : roperand -> instr
| MUL : roperand -> instr
| MULT : roperand -> instr
| MULTU : roperand -> instr
| NOR : roperand -> instr
| OR : roperand -> instr
| ORI : ioperand -> instr
| SB : ioperand -> instr
| SEB : roperand -> instr
| SEH : roperand -> instr
| SH : ioperand -> instr
| SLL : roperand -> instr
| SLLV : roperand -> instr
| SLT : roperand -> instr
| SLTI : ioperand -> instr
| SLTIU : ioperand -> instr
| SLTU : roperand -> instr
| SRA : roperand -> instr
| SRAV : roperand -> instr
| SRL : roperand -> instr
| SRLV : roperand -> instr
| SUB : roperand -> instr
| SUBU : roperand -> instr
| SW : ioperand -> instr
| XOR : roperand -> instr
| XORI : ioperand -> instr
.
