# Flags
INCLUDE = -I .

# OCAML commands
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLMKTOP = ocamlmktop
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLLIBR = ocamlc
OCAMLTOCL = ocamltocl
OCAMLCFLAGS = $(INCLUDE)
OCAMLOPTFLAGS = $(INCLUDE)

# Module name
REFINER = refiner

# Library files
LMFILES = \
	util\
	opname\
	term

# Library file names
MLFILES   = $(addsuffix .ml,   $(LMFILES))
MLIFILES  = $(addsuffix .mli,  $(LMFILES))
CMOFILES  = $(addsuffix .cmo,  $(LMFILES))
CMIFILES  = $(addsuffix .cmi,  $(LMFILES))
CMXFILES  = $(addsuffix .cmx,  $(LMFILES))

# Instructions to make
.PHONY: all clean depend
.PRECIOUS: %.zo %.zi %.zx %.cmo %.cmi %.cmx %.ml %.mli %.mly %.mll

.SUFFIXES: .cma .cmx .cmo .cmi\
	.zi .zo .zx .ml .mli .mll .mly\
	.cl .cli .cll .cly

all: $(REFINER).cma

$(REFINER).cma: $(CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -a -o $@ $(CMOFILES)

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLCFLAGS) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $*.mli

%.cmx: %.ml $.mli
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $*.ml

clean:
	rm -f *.cm* *.z* *~ *.aux *.log *.dvi *.ps
	rm -f $(REFINER).cma
	rm -f *.output

depend: $(MLFILES) $(MLIFILES)
	$(OCAMLDEP) $(INCLUDE) $(MLFILES) $(MLIFILES) > Makefile.dep

include Makefile.dep
