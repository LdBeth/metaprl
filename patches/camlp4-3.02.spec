# $Id$

Name: camlp4
Version: 3.02
Release: 2.rh%{rh_release}
Summary: Objective Caml Preprocessor
Source0: ftp://ftp.inria.fr/INRIA/Projects/cristal/Daniel.de_Rauglaudre/Camlp4/camlp4-%{version}.tar.gz
Source1: ftp://ftp.inria.fr/INRIA/Projects/cristal/Daniel.de_Rauglaudre/Camlp4/camlp4-%{version}-refman.html.tar.gz
Source2: ftp://ftp.inria.fr/INRIA/Projects/cristal/Daniel.de_Rauglaudre/Camlp4/camlp4-%{version}-refman.ps.gz
Patch: camlp4-%{version}-opt.patch
Patch2: camlp4-%{version}-plexer.patch
Patch3: camlp4-%{version}-version.patch
Copyright: BSD
Group: Development/Languages
Vendor: INRIA Rocquencourt
URL: http://caml.inria.fr/
BuildRoot: /tmp/caml-root
BuildPrereq: ocaml
%(LC_ALL="C" rpm -q --queryformat 'Requires:%%{NAME} = %%{VERSION}-%%{RELEASE}' ocaml| grep -v "is not")

%description
Camlp4 is a Pre-Processor-Pretty-Printer for Objective Caml. It offers tools
for syntax (grammars) and the ability to modify the concrete syntax
of the language (quotations, syntax extensions).

Camlp4 can parse normal Ocaml concrete syntax or any other user-definable
syntax. As an example, an alternative syntax is provided, named revised,
because it tries to fix some small problems of the normal syntax.

Camlp4 can pretty print the normal Ocaml concrete syntax or the revised one.
It is therefore always possible to have a version of your sources compilable
by the compiler Objective Caml without preprocessing.

%prep
%setup -q -T -b 0
%setup -q -T -D -a 1
%patch -p0 -b .mp-opt
%patch2 -p0 -b .mp-plex
%patch3 -p0 -b .mp-vers
mv camlp4-%{version}-refman.html refman
cp %{SOURCE2} refman.ps.gz

%build
(
echo OTOP=../ocaml_stuff
echo OPT=.opt
echo OLIBDIR=%{_prefix}/lib/ocaml
echo BINDIR=%{_prefix}/bin
echo LIBDIR=%{_prefix}/lib/camlp4
echo MANDIR=%{_prefix}/man/man1
) > config/Makefile.cnf

rm -f config/Makefile
cat config/Makefile.tpl > config/Makefile
echo >> config/Makefile
cat config/Makefile.cnf >> config/Makefile
chmod -w config/Makefile
make world opt

%install
(
echo OLIBDIR=$RPM_BUILD_ROOT%{_prefix}/lib/ocaml
echo BINDIR=$RPM_BUILD_ROOT%{_prefix}/bin
echo LIBDIR=$RPM_BUILD_ROOT%{_prefix}/lib/camlp4
echo MANDIR=$RPM_BUILD_ROOT%{_prefix}/man/man1
) > config/Makefile.cnf

rm -f config/Makefile
cat config/Makefile.tpl > config/Makefile
echo >> config/Makefile
cat config/Makefile.cnf >> config/Makefile
chmod -w config/Makefile
rm -rf $RPM_BUILD_ROOT
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr (-,root,root)
%doc refman.ps.gz refman LICENSE CHANGES README README-2.01 README-3.00
%{_prefix}/bin/*
%{_prefix}/man/*/*
%{_prefix}/lib/*

%changelog
* Fri May 5 2000 Tim Powers <timp@redhat.com>
- use _prefix wherever possible so that we can make the rebuilds a bit more
  portable
- use globs for the files list, makes sure we pick everything up that we need
  to, also makes for a shorter files list

* Tue Sep  7 1999 Daniel de Rauglaudre <daniel.de_rauglaudre@inria.fr>
Version 2.01.1 Release 1
- Created from Aleksey Nogin's Camlp4 spec file
