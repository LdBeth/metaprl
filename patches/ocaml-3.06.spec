%define debug_package %{nil}

Name: ocaml
Version: 3.06
Release: 1.rh%{rh_release}
Summary: The Objective Caml compiler and programming environment
Source0: ftp://ftp.inria.fr/lang/caml-light/ocaml-%{version}/ocaml-%{version}.tar.gz
Source1: ftp://ftp.inria.fr/lang/caml-light/ocaml-%{version}/ocaml-%{version}-refman.html.tar.gz
Source2: ftp://ftp.inria.fr/lang/caml-light/ocaml-%{version}/ocaml-%{version}-refman.ps.gz
Source3: ftp://ftp.inria.fr/lang/caml-light/ocaml-%{version}/ocaml-%{version}-refman.info.tar.gz
Patch1: camlp4-%{version}-opt.patch

License: part LGPL, part QPL
Group: Development/Languages
BuildRoot: /tmp/ocaml-buildroot
Vendor: INRIA Rocquencourt
URL: http://caml.inria.fr/
BuildRequires: emacs, tcl, tk

Obsoletes: ocaml-emacs, camlp4
Conflicts: ocaml-emacs, camlp4

%description
Objective Caml is a high-level, strongly-typed, functional and
object-oriented programming language from the ML family of languages.

This package comprises two batch compilers (a fast bytecode compiler
and an optimizing native-code compiler), an interactive toplevel system,
parsing tools (Lex,Yacc,Camlp4), a replay debugger, a documentation generator,
and a comprehensive library.

%prep
%setup -T -q -b 0
%setup -T -D -q -a 1
%setup -T -D -q -a 3
cp %{SOURCE2} refman.ps.gz
%patch1 -p0 -b .mp-opt

%build
./configure -bindir %{_bindir} -libdir %{_libdir}/ocaml -mandir %{_mandir}/man1 -with-pthread
make -j1 world opt opt.opt

%install
rm -rf %{buildroot}
make install BINDIR=%{buildroot}%{_bindir} LIBDIR=%{buildroot}%{_libdir}/ocaml MANDIR=%{buildroot}%{_mandir}/man1
mv %{buildroot}%{_libdir}/ocaml/ld.conf %{buildroot}%{_libdir}/ocaml/ld.conf.orig
sed -e "s|^%{buildroot}||" %{buildroot}%{_libdir}/ocaml/ld.conf.orig > %{buildroot}%{_libdir}/ocaml/ld.conf
rm -f %{buildroot}%{_libdir}/ocaml/ld.conf.orig
(cd emacs; make install BINDIR=%{buildroot}%{_bindir} EMACSDIR=%{buildroot}%{_datadir}/emacs/site-lisp)
(mkdir -p %{buildroot}%{_infodir}; cd infoman; cp ocaml*.gz %{buildroot}%{_infodir})
cp -a {parsing/{location,longident,parsetree},typing/typecore}.{cm,ml}i %{buildroot}%{_libdir}/ocaml
mv -f %{buildroot}%{_bindir}/ocamlc %{buildroot}%{_bindir}/ocamlc.byte
mv -f %{buildroot}%{_bindir}/ocamlopt %{buildroot}%{_bindir}/ocamlopt.byte
ln -s ocamlc.opt %{buildroot}%{_bindir}/ocamlc
ln -s ocamlopt.opt %{buildroot}%{_bindir}/ocamlopt

# Disable build root strip policy:
# executables generated by ocamlc -custom MUST NOT BE STRIPPED
%define __spec_install_post /usr/lib/rpm/brp-compress

%files
%defattr(-, root, root)
%{_bindir}/*
%{_mandir}/man1/*
%{_libdir}/ocaml
%{_datadir}/emacs/site-lisp/*
%{_infodir}/*

%doc README LICENSE refman.ps.gz htmlman Changes

%changelog
* Tue Nov 5 2002 <rpm@nogin.org>
- Updated to 3.06
- Filled in the changelog (from the CVS log).

* Fri Oct 4 2002 <rpm@nogin.org>
- Make it relocatable

* Wed Oct 2 2002 <rpm@nogin.org>
- Ocaml should be built in presence of tcl and tk

* Tue May 6 2002 <rpm@nogin.org>
- 3.04 SPEC based on the .spec in the Ocaml distro.
