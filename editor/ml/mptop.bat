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
set /p INCLUDES <theories.dir
set /p DEBUGINCLUDES <mldebug.dir

rem
rem Start the executable.
rem
mp.top %INCLUDES% %*
