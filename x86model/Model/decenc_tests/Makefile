#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

.PHONY:	all clean byte native

OCB_FLAGS = -tag bin_annot
OCB = 		ocamlbuild $(OCB_FLAGS)
ENCODE = Encode

all: native byte # profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) $(ENCODE).native

byte:
	$(OCB) $(ENCODE).byte


