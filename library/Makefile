IODIR = /usr/u/nuprl/nuprl4i/nuprl5L/nuprl-light/refiner
IO = -I $(IODIR)

mytop	:	io.cma library.cma test.cmo $(IODIR)/term.cmo
		ocamlmktop -custom -o mytop unix.cma io.cma library.cma test.cmo $(IODIR)/term.cmo -cclib -lunix   

#	Create test executable
#test :	io.cma library.cma test.cmo
	 #ocamlc -o test -custom $(IO) \
		#unix.cma io.cma library.cma test.cmo \
		#-cclib -lunix	

#	Create io_trm.cma
#io_trm.cma :  list_util.cmo debug.cmo opname.cmo term.cmo 
	#ocamlc -a -o io_trm.cma list_util.cmo debug.cmo opname.cmo term.cmo 

#	Create io.cma
io.cma :  $(IODIR)/io_trm.cma $(IODIR)/term.cmo socketIo.cmo bigInt.cmo registry.cmo mathBus.cmo mbterm.cmo nuprl5.cmo link.cmo
	ocamlc -a -o io.cma  $(IODIR)/io_trm.cma socketIo.cmo bigInt.cmo registry.cmo mathBus.cmo mbterm.cmo nuprl5.cmo link.cmo

#	Create library.cma
library.cma : utils.cmo basic.cmo orb.cmo library.cmo db.cmo
	ocamlc -a -o library.cma utils.cmo basic.cmo orb.cmo library.cmo db.cmo

#	Compile test.ml : should only be called when library.cma is up-to-date
test.cmo :	test.cmi test.ml utils.cmi basic.cmi orb.cmi library.cmi
	ocamlc -c test.ml

test.cmi  :	test.mli utils.cmi basic.cmi orb.cmi library.cmi
	ocamlc -c test.ml


db.cmo :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.cmi link.cmi basic.cmi db.cmi db.ml 
	ocamlc -c $(IO) db.ml

db.cmi :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.cmi link.cmi basic.cmi db.mli
	ocamlc -c $(IO) db.mli
link.cmo :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.cmi link.cmi link.ml 
	ocamlc -c $(IO) link.ml

link.cmi :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.cmi link.mli
	ocamlc -c $(IO) link.mli


nuprl5.cmo :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.cmi nuprl5.ml
	ocamlc -c $(IO) nuprl5.ml

nuprl5.cmi :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi nuprl5.mli
	ocamlc -c $(IO) nuprl5.mli

 
mbterm.cmo :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.cmi mbterm.ml 
	ocamlc -c $(IO) mbterm.ml

mbterm.cmi :	$(IODIR)/io_trm.cma socketIo.cmi bigInt.cmi registry.cmi mathBus.cmi mbterm.mli 
	ocamlc -c $(IO) mbterm.mli


library.cmo :	utils.cmi basic.cmi orb.cmi library.cmi library.ml \
		io.cma
	ocamlc -c $(IO) library.ml

library.cmi :	utils.cmi basic.cmi orb.cmi library.mli \
		$(IODIR)/term.mli
	ocamlc -c $(IO) library.mli


orb.cmo :	utils.cmi basic.cmi orb.cmi orb.ml io.cma
	ocamlc -c $(IO) orb.ml

orb.cmi :	utils.cmi basic.cmi orb.mli \
		$(IODIR)/term.mli
	ocamlc -c $(IO) orb.mli


basic.cmo :	utils.cmi basic.cmi basic.ml \
		io.cma
	ocamlc -c $(IO) basic.ml

basic.cmi :	utils.cmi basic.mli \
		$(IODIR)/term.mli
	ocamlc -c $(IO) basic.mli


utils.cmo :	utils.cmi utils.ml
	ocamlc -c $(IO) utils.ml

utils.cmi :	utils.mli
	ocamlc -c $(IO) utils.mli



