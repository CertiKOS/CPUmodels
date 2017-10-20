(**** Generate the ELF file *****)

(* ELF header *)
type elf_data_encoding = | ELFDATANONE 
                         | ELFDATA2LSB   (* little endian *) 
                         | ELFDATA2MSB   (* big endian *)
let ef_encoding_to_bytes ecd = 
  match ecd with
  | ELFDATANONE -> 0
  | ELFDATA2LSB -> 1
  | ELFDATA2MSB -> 2

type elf_file_class = | ELFCLASSNONE 
                      | ELFCLASS32  (* 32-bit file *) 
                      | ELFCLASS64  (* 64-bit file *)
let ef_class_to_bytes cls =
  match cls with
  | ELFCLASSNONE -> 0
  | ELFCLASS32 -> 1
  | ELFCLASS64 -> 2

type elf_file_type = | ET_NONE 
                     | ET_REL    (* Relocatable *)
                     | ET_EXEC   (* Executable *)
                     | ET_DYN    (* Shared object *)
                     | ET_CORE   (* Core file *)
let ef_type_to_bytes typ =
  match typ with
  | ET_NONE -> 0
  | ET_REL -> 1
  | ET_EXEC -> 2
  | ET_DYN -> 3
  | ET_CORE -> 4

type elf_machine = | EM_NONE | EM_386
let ef_machine_to_bytes mach =
  match mach with
  | EM_NONE -> 0
  | EM_386 -> 3

type elf_header =
  {
    e_encoding   : elf_data_encoding;
    e_class      : elf_file_class;
    e_type       : elf_file_type;
    e_machine    : elf_machine;
    e_entry      : int; (* entry point of the program *)
    e_phoff      : int; (* offset to the first program header *)
    e_shoff      : int; (* offset to the first section header *)
    e_flags      : int; 
    e_ehsize     : int; (* size of the elf header, i.e., 52 bytes *)
    e_phentsize  : int; (* size of a program header *)
    e_phnum      : int; (* number of program headers *)
    e_shentsize  : int; (* size of a section header *)
    e_shnum      : int; (* number of section headers *)
    e_shstrndx   : int; (* index to the section header for the string table *)
  }
(* indexes to the array e_ident *)
let ei_mag0 = 0
let ei_mag1 = 1
let ei_mag2 = 2
let ei_mag3 = 3
let ei_class = 4
let ei_data = 5
let ei_version = 6
let ei_pad = 7
let ei_size = 16

type section_type = | SHT_NULL
                    | SHT_PROGBITS  (* program *)
                    | SHT_STRTAB    (* string table *)
                    | SHT_NOBITS    (* unintialized data *)

type section_flag = | SHF_WRITE       (* writable section *)
                    | SHF_ALLOC       (* section will be allocated in memory *)
                    | SHF_EXECINSTR   (* executable section *)

type section_header =
  {
    sh_name        : int;   (* offset in the string table to the name of the section *)
    sh_type        : section_type; 
    sh_flags       : section_flag list;
    sh_addr        : int;   (* starting address of the section in the memory *)
    sh_offset      : int;   (* offset to the beginning of the section in the file *)
    sh_size        : int;   (* size of the section *)
    sh_addralign   : int;   (* alignment of the section *)
  }

type segment_type = | PT_NULL 
                    | PT_LOAD  (* the program segment is loadable to the memory *)

type segment_flag = | PF_EXEC   (* executable *)
                    | PF_WRITE  (* writable *)
                    | PF_READ   (* readable *)

type program_header =
  {
    p_type   : segment_type;
    p_offset : int;   (* offset to the beginning of the segment in the file *)
    p_vaddr  : int;   (* the starting address of the segment in the virtual memory *)
    p_paddr  : int;   (* the starting address of the segment in the physical memory *)  
    p_filesz : int;   (* the size of the segment *)
    p_memsz  : int;   (* the size of the memory region the segment occupies, must be no less than p_filesz *)
    p_flags  : segment_flag list;
    p_align  : int;   (* alignment of the segment *)
  }


type elf_file = {
    ef_header       : elf_header;           (* ELF header *)
    ef_sec_headers  : section_header list;  (* section headers *)
    ef_prog_headers : program_header list;  (* program headers *)
    ef_sections     : int list;             (* contents of the sections *)
  }

let create_386_exec_elf_header entry phoff shoff phnum shnum shstrndx =
  {
    e_encoding  = ELFDATA2LSB;
    e_class     = ELFCLASS32;
    e_type      = ET_EXEC;
    e_machine   = EM_386;
    e_entry     = entry;
    e_phoff     = phoff;
    e_shoff     = shoff;
    e_flags     = 0;
    e_ehsize    = 52;
    e_phentsize = 32;
    e_phnum     = phnum;
    e_shentsize = 40;
    e_shnum     = shnum;
    e_shstrndx  = shstrndx;
  }

let elf_file_size ef =
  let h = ef.ef_header in
  h.e_shoff + h.e_shentsize * h.e_shnum

let elf_header_to_bytes eh =
  let himg = Array.make eh.e_ehsize 0 in
  Array.set himg ei_mag0 0x7f;
  Array.set himg ei_mag1 (Char.code 'E');
  Array.set himg ei_mag2 (Char.code 'L');
  Array.set himg ei_mag3 (Char.code 'F');
  Array.set himg ei_class (ef_class_to_bytes eh.e_class);
  Array.set himg ei_data (ef_encoding_to_bytes eh.e_encoding);
  Array.set himg ei_version 1;
  Array.fill himg ei_pad (ei_size-ei_pad) 0;
  himg

let elf_program_header_to_bytes ph sz =
  let himg = Array.make sz 0 in
  p

let array_overwrite asrc adest offset =
  Array.iteri (fun i e -> Array.set adest (i+offset) e) asrc

let elf_to_bytes ef = 
  let sz = elf_file_size ef in
  let fimg = Array.make sz 0 in
  (* Write the header *)
  let himg = elf_header_to_bytes ef.ef_header in
  array_overwrite himg fimg 0;
  fimg 
  
(* Write the file image into a binary file *)
let write_elf fname ef =
  let fimg = elf_to_bytes ef in
  let dump_channel = open_out_bin fname in
  Array.iter (fun i -> output_byte dump_channel i) fimg;
  close_out dump_channel
