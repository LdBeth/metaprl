#!/usr/bin/perl -w

$nm{"shape"}=$nm{"string"}="s";
$nm{"param"}="p";
$nm{"var"}="v";
$nm{"int"}="i";
$nm{"term"}="t";
$nm{"refine_error"}="re";
$nm{"address"}="a";
$nm{"match_type"}=$nm{"meta_term"}="mt";
$nm{"opname"}="o";
$nm{"ty_param"}="tp";
$nm{"(string ### refine_error) list"}="sre";

$cnv{"opname"}=$cnv{"string"}=$cnv{"var"}=$cnv{"int"}=$cnv{"shape"}="";
$cnv{"param"}="param_of_param1 ";
$cnv{"term"}="term_of_term1 ";
$cnv{"refine_error"}="re_of_re1 ";
$cnv{"address"}="addr_of_addr1 ";
$cnv{"meta_term"}="mterm_of_mterm1 ";
$cnv{"match_type"}="mtype_of_mtype1 ";
$cnv{"ty_param"}="tp_of_tp1 ";
$cnv{"(string ### refine_error) list"}="List.map sre_of_sre1 ";

open IN, 'grep -A5000 refine_error refiner/refsig/refine_error_sig.ml | egrep -v \'type re|^ *\*|^ *(exception|val|end)|\(\*|^ *$\'|';

while (<IN>) {
    s/^[ |]*//;
    s/[[:space:]]+$//;
    ($name, $typ)=split(/ +of +/,$_);
    if (defined $typ) {
	while ($typ =~ /\([^)]*\*/) {
	    $typ =~ s/(\([^\)]*)\*/$1###/;
	};
	@typs=split(/ *\* */, $typ);
    } else {
	@typs=()
    };
    for ($i=0; $i<= $#typs; $i++) {
	if ((not (defined $nm{$typs[$i]})) || (not (defined $cnv{$typs[$i]}))) {
	    die "Do not know what to do with $typs[$i] in\n$_\n"
	}
    };
    print "    | Err1.$name ";
    if ($#typs == 0) { print $nm{$typs[0]}, "0" };
    if ($#typs > 0) {
	for ($i=0; $i<= $#typs; $i++) {
	    if ($i==0) { print "(" } else { print ", " };
	    print $nm{$typs[$i]}, $i;
	}
	print ")";
    }
    print " -> $name";
    for ($i=0; $i<= $#typs; $i++) {
        if ($i==0) { print " (" } else { print ", " };
	print $cnv{$typs[$i]}, $nm{$typs[$i]}, $i;
    }
    if ($#typs >= 0) { print ")" };
    print "\n";
}
