TERMDIR = /usr/u/nuprl/nuprl4i/nuprl5L/nuprl-light/refiner
IODIR = ../refiner
IO = -I $(TERMDIR)

mytop	:	io.cma library.cma test.cmo $(IODIR)/term.cmo
		ocamlmktop -custom -o mytop unix.cma io.cma library.cma test.cmo $(IODIR)/term.cmo -cclib -lunix

	Create test executable
testx :	io.cma library.cma test.cmo
	ocamlc -o test -custom $(IO) \
		unix.cma io.cma library.cma test.cmo \
		-cclib -lunix	

#	Create io.cma
io.cma :  $(TERMDIR)/io_trm.cma $(TERMDIR)/term.cmo socketIo.cmo bigInt.cmo registry.cmo mathBus.cmo mbterm.cmo \
		 nuprl5.cmo link.cmo
	ocamlc -a -o io.cma io_trm.cma socketIo.cmo bigInt.cmo registry.cmo mathBus.cmo mbterm.cmo \
		nuprl5.cmo link.cmo

#	Create io_trm.cma
io_trm.cma :   $(TERMDIR)/list_util.cmo  $(TERMDIR)/debug.cmo  $(TERMDIR)/opname.cmo  $(TERMDIR)/term.cmo 
	ocamlc -a -o io_trm.cma  $(TERMDIR)/list_util.cmo  $(TERMDIR)/debug.cmo  $(TERMDIR)/opname.cmo  $(TERMDIR)/term.cmo 

#	Create library.cma temporarily  removed db.cmo from following.
library.cma :	utils.cmo basic.cmo orb.cmo library.cmo 
	ocamlc -a -o library.cma utils.cmo basic.cmo orb.cmo library.cmo 

#	Compile test.ml : should only be called when library.cma is up-to-date
# removed test.cmi
test.cmo :	 test.ml utils.cmi basic.cmi orb.cmi library.cmi
	ocamlc -c test.ml

test.cmi :	test.mli utils.cmi basic.cmi orb.cmi library.cmi
	ocamlc -c test.ml


db.cmo :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi \
			 nuprl5.cmi link.cmi basic.cmi db.cmi db.ml 
	ocamlc -c $(IO) db.ml

db.cmi :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi \
			 nuprl5.cmi link.cmi basic.cmi db.mli
	ocamlc -c $(IO) db.mli

link.cmo :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi \
			 nuprl5.cmi link.cmi link.ml 
	ocamlc -c $(IO) link.ml

link.cmi :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi \
			 nuprl5.cmi link.mli
	ocamlc -c $(IO) link.mli


nuprl5.cmo :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi \
			 nuprl5.cmi nuprl5.ml
	ocamlc -c $(IO) nuprl5.ml

nuprl5.cmi :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.mli
	ocamlc -c $(IO) nuprl5.mli

 
mbterm.cmo :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi mbterm.ml 
	ocamlc -c $(IO) mbterm.ml

mbterm.cmi :	io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.mli 
	ocamlc -c $(IO) mbterm.mli

library.cmo :	utils.cmi basic.cmi orb.cmi library.cmi library.ml \
		io.cma
	ocamlc -c $(IO) library.ml

library.cmi :	utils.cmi basic.cmi orb.cmi library.mli $(TERMDIR)/term.mli
	ocamlc -c $(IO) library.mli


orb.cmo :	utils.cmi basic.cmi orb.cmi orb.ml io.cma
	ocamlc -c $(IO) orb.ml

orb.cmi :	utils.cmi basic.cmi orb.mli \
		$(TERMDIR)/term.mli
	ocamlc -c $(IO) orb.mli


basic.cmo :	utils.cmi basic.cmi basic.ml \
		io.cma
	ocamlc -c $(IO) basic.ml

basic.cmi :	utils.cmi basic.mli \
		$(TERMDIR)/term.mli
	ocamlc -c $(IO) basic.mli


utils.cmo :	utils.cmi utils.ml
	ocamlc -c $(IO) utils.ml

utils.cmi :	utils.mli
	ocamlc -c $(IO) utils.mli

mathBus.cmo :	mathBus.cmi mathBus.ml registry.cmo bigInt.cmo
	ocamlc -c $(IO) mathBus.ml

mathBus.cmi :	mathBus.mli registry.cmi bigInt.mli
	ocamlc -c $(IO) mathBus.mli

registry.cmo :	registry.cmi registry.ml bigInt.cmo
	ocamlc -c $(IO) registry.ml

registry.cmi :	registry.mli bigInt.mli
	ocamlc -c $(IO) registry.mli


bigInt.cmo :	bigInt.cmi bigInt.ml
	ocamlc -c $(IO) bigInt.ml

bigInt.cmi :	bigInt.mli
	ocamlc -c $(IO) bigInt.mli


socketIo.cmo :	socketIo.cmi socketIo.ml
	ocamlc -c $(IO) socketIo.ml

socketIo.cmi :	socketIo.mli
	ocamlc -c $(IO) socketIo.mli



