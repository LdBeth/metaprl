	
IODIR = /usr/u/nuprl/nuprl4i/nuprl5/sys/io/mathbus
IO = -I $(IODIR)

#	Create test executable
test :	library.cma test.cmo
	ocamlc -o test -custom $(IO) \
		 unix.cma io.cma library.cma test.cmo \
		 -cclib -lunix	

#	Create library.cma
library.cma : utils.cmo basic.cmo orb.cmo library.cmo 
	ocamlc -a -o library.cma utils.cmo basic.cmo orb.cmo library.cmo 

#	Compile test.ml : should only be called when library.cma is up-to-date
test.cmo :	test.ml utils.cmi basic.cmi orb.cmi library.cmi
	ocamlc -c test.ml


library.cmo :	utils.cmi basic.cmi orb.cmi library.cmi library.ml \
		$(IODIR)/io.cma
	ocamlc -c $(IO) library.ml

library.cmi :	utils.cmi basic.cmi orb.cmi library.mli \
		$(IODIR)/object_id.mli $(IODIR)/term.mli
	ocamlc -c $(IO) library.mli


orb.cmo :	utils.cmi basic.cmi orb.cmi orb.ml $(IODIR)/io.cma
	ocamlc -c $(IO) orb.ml

orb.cmi :	utils.cmi basic.cmi orb.mli \
		$(IODIR)/term.mli
	ocamlc -c $(IO) orb.mli


basic.cmo :	utils.cmi basic.cmi basic.ml \
		$(IODIR)/io.cma
	ocamlc -c $(IO) basic.ml

basic.cmi :	utils.cmi basic.mli \
		$(IODIR)/object_id.mli $(IODIR)/term.mli
	ocamlc -c $(IO) basic.mli


utils.cmo :	utils.cmi utils.ml
	ocamlc -c $(IO) utils.ml

utils.cmi :	utils.mli
	ocamlc -c $(IO) utils.mli



