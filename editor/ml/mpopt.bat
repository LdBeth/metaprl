@echo off
rem
rem Configure Ensemble
rem
set ENS_PORT=7474
set ENS_MODES="DEERING:UDP"
set ENS_DEERING_PORT=7472
set ENS_GOSSIP_PORT=7470
set ENS_GOSSIP_HOSTS=tulare
set ENS_ID=$USER
set ENS_DOMAIN_PORT=7473

rem
rem Root directory is two directories up
rem
set MP_ROOT=../..

rem
rem Use the local copy of the library
rem
set MPLIB=../../lib

rem
rem Work in Unicode locale.
rem
set LC_ALL=en_US.UTF-8

rem
rem Search path
rem
set INCLUDES=-I . -I ../../support/display -I ../../support/shell -I ../../support/tactics -I ../../theories/sil -I ../../theories/reflect_itt -I ../../theories/tutorial -I ../../theories/tptp -I ../../theories/fol -I ../../theories/czf -I ../../theories/itt -I ../../theories/base -I ../../theories/ocaml_sos -I ../../theories/ocaml_doc -I ../../theories/mc -I ../../theories/fir -I ../../theories/phobos -I ../../theories/experimental/compile -I tests -I $CAMLP4LIB
rem set /P INCLUDES= <theories.dir

set DEBUGINCLUDES=%INCLUDES% -I ../../mllib -I ../../refiner/refbase -I ../../refiner/refsig -I ../../refiner/term_gen -I ../../refiner/term_std -I ../../refiner/term_ds -I ../../refiner/rewrite -I ../../refiner/refiner -I ../../refiner/reflib -I ../../filter/base -I ../../filter/boot -I ../../filter/filter
rem set /P DEBUGINCLUDES= <mldebug.dir

rem
rem Start the executable.
rem
mp.opt %INCLUDES% %*
