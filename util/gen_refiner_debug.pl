#!/usr/bin/perl -w

if ($#ARGV != 0) {
    die "Usage: gen_refiner_debug.pl Modname < interface";
};

$modname = shift;

$line = "";

$merges{"operator"}="merge_op";
$merges{"operator'"}="merge_op'";
$merges{"bound_term"}="merge_bterm";
$merges{"bound_term'"}="merge_bterm'";
foreach my $ty ("bool", "unit", "param", "term", "var", "int", "level_exp", "level_exp_var", "opname", "string", "address", "match_param", "match_term", "esequent", "term_subst", "shape", "shape_param", "meta_term") {
    $merges{$ty}="merge_$ty";
    $merges{"$ty list"}="merge_" . $ty . "s";
    $merges{"$ty option"}="merge_" . $ty . "_opt";
    $merges{"$ty list option"}="merge_" . $ty . "_lo";
};
foreach my $ty ("term", "param", "level_exp", "level_exp_var") {
    my $ty2 = $ty . "'";
    $merges{$ty2} = "merge_$ty2";
    $merges{"$ty2 list"} = "merge_$ty" . "s'";
}
$merges{"Lm_num.num"} = "merge_num";
$merges{"SymbolSet.t"} = "merge_ss";
$merges{"string list * term option * term"} = "merge_sltot";
$merges{"'a"} = "merge_poly";
$merges{"'a list"} = "merge_poly";
$merges{"term -> term"} = "merge_ttf";

foreach my $ty ("bool", "int", "var", "opname", "out_channel", "formatter", "string", "Lm_num.num", "SymbolSet.t", "'a", "unit") {
    $splits{$ty} = $splits{"$ty list"} = $splits{"$ty array"} = "";
};
foreach my $ty ("term", "bound_term", "param", "operator", "level_exp", "level_exp_var", "address", "shape") {
    $splits{$ty} = "identity";
    $splits{"$ty list"} = "split";
    $splits{"$ty option"} = "split_opt";
};
foreach my $ty ("term", "param", "level_exp", "level_exp_var") {
    my $ty = $ty . "'";
    $splits{$ty} = "split_$ty";
}
$splits{"esequent"} = "split_eseq";
$splits{"bound_term'"} = "split_bterm'";
$splits{"operator'"} = "split_op'";
$splits{"term_subst"} = "split_term_subst";
$splits{"meta_term"} = "split_meta_term";
$splits{"(term option * term) list"} = "split_popl";
$splits{"term -> term"} = "split_ttf";
$splits{"SymbolSet.t -> term"} = "split_atf";
$splits{"SymbolSet.t -> term -> term"} = "split_attf";
$splits{"term -> term * 'a"} = "split_ttaf";
$splits{"SymbolSet.t -> term -> term * 'a"} = "split_attaf";
$splits{"term -> 'a -> bool"} = "split_taf";

$merges{"object_id"} = $merges{"param list"};
$splits{"object_id"} = $splits{"param list"};

sub do_split($$) {
    my ($type, $arg) = @_;
    if ($type =~ /^\([^()]*\)$/) { $type =~ s/^\([[:space:]]*//; $type =~ s/[[:space:]]*\)$// };
    if (not(defined $splits{$type})) { die "\n\nDo not know how to split $type" };
    my $split = $splits{$type};
    if ($split eq "") {
	return ($arg, $arg);
    } else {
	$arg1 = $arg . "_1";
	$arg2 = $arg . "_2";
	print "         let $arg1, $arg2 =";
	if ($split ne "identity") { print " $split" };
	print " $arg in\n";
	return ($arg1, $arg2);
    }
}

sub process ($) {
    my ($fullline) = ($line) = @_;
    my $args1="";
    my $args2="";
    $line=~s/^[[:space:]]*//;
    $line=~s/[[:space:]]*$//;
    if (not($line =~/^val[[:space:]]/)) { return () };
    $line=~s/^val[[:space:]]*//;
    if ($line=~/\bval\b/) {
	die "\n\nLine has more than one \"val\" on it: $fullline"
    };
    if ($line=~/:.*:/) {
	die "\n\nLine has to many \":\" symbols: $fullline"
    }
    if (not($line=~/:/)) {
	die "\n\nLine has no \":\" on it: $fullline"
    };
    $line=~s/[[:space:]]+/ /g;
    my ($name, $rest) = split(/[[:space:]]*:[[:space:]]*/, $line);
    while ($rest=~/\([^()]*->/) { $rest =~ s/(\([^()]*)->/$1>>/g };
    my @types = split(/[[:space:]]*->[[:space:]]*/, $rest);
    grep { s/>>/->/g } @types;
    my $tyres = pop(@types);
    print "      let $name ";
    for (my $i=0; $i<=$#types; $i++) { print "(p$i : $types[$i]) " };
    print "=\n";
    for (my $i=0; $i<=$#types; $i++) {
	my ($arg1, $arg2) = do_split($types[$i], "p$i");
	$args1 .= " $arg1";
	$args2 .= " $arg2";
    }
    if ($tyres =~ /^\([^()]*\)$/) { $tyres =~ s/^\([[:space:]]*//; $tyres =~ s/[[:space:]]*\)$// };
    if (defined $merges{$tyres}) {
	print "         ", $merges{$tyres}, " \"$modname.$name\" (" , $modname, "1.$name$args1) (", $modname, "2.$name$args2)\n\n";
    } else {
	if ($tyres =~ /\*/) {
	    my @tyress = split(/[[:space:]]*\*[[:space:]]*/,$tyres);
	    my $let1 = $let2 = "         let ";
	    for (my $i=0; $i<=$#tyress; $i++) {
		if ($i > 0) {
		    $let1 .= ", ";
		    $let2 .= ", ";
		}
		$let1 .= "res$i" . "_1";
		$let2 .= "res$i" . "_2";
	    }
	    print $let1, " = " , $modname, "1.$name$args1 in\n";
	    print $let2, " = " , $modname, "2.$name$args2 in\n";
	    for (my $i=0; $i<=$#tyress; $i++) {
		if ($tyress[$i] =~ /^\([^()]*\)$/) { $tyress[$i] =~ s/^\([[:space:]]*//; $tyress[$i] =~ s/[[:space:]]*\)$// };
		if (defined $merges{$tyress[$i]}) {
		    print "         (", $merges{$tyress[$i]}, " \"$modname.$name - $i\" res$i", "_1 res$i", "_2)", (($i<$#tyress)?",":"\n"), "\n";
		} else { die "\n\nDo not know how to merge $tyress[$i]" }
	    }
	} else { die "\n\nDo not know how to merge $tyres" }
    }
}

print "      (* The rest of this module is auto-generated by the util/gen_refiner_debug.pl script *)\n\n";

while (<>) {
    if (/\(\*/ || /\*\)/) {
	die "\n\nComments on input not supported!"
    };
    if (/\btype\b/) {
	die "\n\nTypes on input not supported!"
    };
    if (/\bval\b/) {
	process($line);
	$line=$_
    } else {
	$line .= " ";
	$line .= $_;
    }
}
process($line);


