#!/usr/bin/perl -w

#
# The "user-maintained" part of this script consists of two hash tables -
# "merges" and "splits".
#

#
# The "merges" table defines a "merge" function for each type that can appear
# as a return type for one of the refiner function.
#
# Syntax:
#    $merges{"type"} = "function";
#
# The function should be defined in the "MERGES" section of refiner_debug.ml
# and should have the type
#    string -> type(Refiner1) -> type(Refiner2) -> type(Refiner)
#

$merges{"operator"}="merge_op";
$merges{"operator'"}="merge_op'";
$merges{"bound_term"}="merge_bterm";
$merges{"bound_term'"}="merge_bterm'";
foreach my $ty ("bool", "unit", "param", "term", "var", "int", "level_exp", "level_exp_var", "opname", "string", "address", "match_param", "match_term", "esequent", "term_subst", "shape", "shape_param", "meta_term", "rewrite_item", "msequent", "extract_description", "prim_tactic", "prim_rewrite", "addr_item") {
    $merges{$ty}="merge_$ty";
    $merges{"$ty list"}="merge_" . $ty . "s";
    $merges{"$ty array"}="merge_" . $ty . "_arr";
    $merges{"$ty option"}="merge_" . $ty . "_opt";
    $merges{"$ty list option"}="merge_" . $ty . "_lo";
};
foreach my $ty ("term", "param", "level_exp", "level_exp_var") {
    my $ty2 = $ty . "'";
    $merges{$ty2} = "merge_$ty2";
    $merges{"$ty2 list"} = "merge_$ty" . "s'";
}
# Fully abstract type, from which we have no hope (or need) for consistency checks
foreach my $ty ("rewrite_rule", "rewrite_redex", "sentinal", "tactic", "extract", "refiner", "build", "rw", "cond_rewrite") {
    $merges{"$ty"} = "merge_triv"
};
$merges{"rewrite_args_spec"} = "merge_rwspecs";
$merges{"rw_args"} = "merge_rwargs";
$merges{"rewrite_args"} = "merge_rewrite_args";
$merges{"Lm_num.num"} = "merge_num";
$merges{"SymbolSet.t"} = "merge_ss";
$merges{"(int * int) SymbolTable.t"} = $merges{"(bool * int * int) SymbolTable.t"} = "merge_stables";
$merges{"(int * bool) list"} = "merge_ibl";
$merges{"string list * term option * term"} = "merge_sltot";
$merges{"(rewrite_type * var) list"} = "merge_rwtvl";
$merges{"(dependency * opname) list"} = "merge_dos";
$merges{"(string list * term option * term) list * term"} = "merge_sltotlt";
$merges{"'a"} = "merge_poly";
$merges{"'a list"} = "merge_poly";
$merges{"term -> term"} = $merges{"ml_rewrite"} = "merge_ttf";
$merges{"object_id"} = $merges{"param list"};

#
# The "splits" table defines a "split" function for each type that can appear
# as an input type for one of the refiner function.
#
# Syntax:
#    $splits{"type"} = "function";
#
# The function should be defined in the "SPLITS" section of refiner_debug.ml
# and should have the type
#    type(Refiner) -> type(Refiner1) * type(Refiner2)
#
# The empty string value is an optimization that stands for "fun v -> v, v"
#

# Non-refiner types
foreach my $ty ("bool", "int", "var", "opname", "out_channel", "formatter", "string", "Lm_num.num", "SymbolSet.t", "'a", "shape", "unit", "strict", "rewrite_args_spec", "addr_item", "(int * int) SymbolTable.t", "(bool * int * int) SymbolTable.t") {
    $splits{$ty} = $splits{"$ty list"} = $splits{"$ty array"} = "";
};
# Fully abstract types
foreach my $ty ("term", "bound_term", "param", "operator", "level_exp", "level_exp_var", "address", "rewrite_rule", "rewrite_redex", "sentinal", "tactic", "msequent", "extract", "refiner", "build", "rw", "cond_rewrite") {
    $splits{$ty} = "identity";
    $splits{"$ty list"} = "split";
    $splits{"$ty option"} = "split_opt";
    $splits{"$ty array"} = "split_array";
};
foreach my $ty ("term", "param", "level_exp", "level_exp_var") {
    my $ty = $ty . "'";
    $splits{$ty} = "split_$ty";
}
foreach my $ty ("term_subst", "meta_term", "ml_rule", "term_extract", "ml_cond_rewrite", "rewrite_args", "ty_param", "ty_bterm", "ty_term") {
    $splits{$ty} = "split_$ty";
    $splits{"$ty list"} = "split_$ty" . "s";
}
$splits{"esequent"} = "split_eseq";
$splits{"bound_term'"} = "split_bterm'";
$splits{"operator'"} = "split_op'";
$splits{"(term option * term) list"} = "split_popl";
$splits{"address rw_args_poly"} = $splits{"rw_args"} = "split_args";
$splits{"term -> term"} = $splits{"ml_rewrite"} = "split_ttf";
$splits{"SymbolSet.t -> term"} = "split_atf";
$splits{"SymbolSet.t -> term -> term"} = "split_attf";
$splits{"unit -> extract"} = "split_utriv";
$splits{"term -> term * 'a"} = "split_ttaf";
$splits{"SymbolSet.t -> term -> term * 'a"} = "split_attaf";
$splits{"term -> unit"} = $splits{"term -> 'a -> bool"} = $splits{"term -> SymbolSet.t -> bool"} = "split_taf";

$splits{"object_id"} = $splits{"param list"};

if ($#ARGV != 0) {
    die "Usage: gen_refiner_debug.pl Modname < interface";
};

$modname = shift;

$line = "";

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
    my $wrap = ($#types>=0)?"wrap" . ($#types + 1) . " ": "";
    my $merge = ($#types>=0)?"merge ": "";
    for (my $i=0; $i<=$#types; $i++) {
	my ($arg1, $arg2) = do_split($types[$i], "p$i");
	$args1 .= " $arg1";
	$args2 .= " $arg2";
    }
    if ($tyres =~ /^\([^()]*\)$/) { $tyres =~ s/^\([[:space:]]*//; $tyres =~ s/[[:space:]]*\)$// };
    if (defined $merges{$tyres}) {
	print "         ", $merge, $merges{$tyres}, " \"$modname.$name\" ($wrap" , $modname, "1.$name$args1) ($wrap", $modname, "2.$name$args2)\n\n";
    } else {
	if ($tyres =~ /^[^(]*\*/) {
	    my @tyress = split(/[[:space:]]*\*[[:space:]]*/,$tyres);
	    print "         let res1 = " , $wrap, $modname, "1.$name$args1 in\n";
	    print "         let res2 = " , $wrap, $modname, "2.$name$args2 in\n";
	    my $let1 = $let2 = "(";
	    for (my $i=0; $i<=$#tyress; $i++) {
		if ($i > 0) {
		    $let1 .= ", ";
		    $let2 .= ", ";
		}
		$let1 .= "res$i" . "_1";
		$let2 .= "res$i" . "_2";
	    }
	    print "         let $let1), $let2) = ", $merge, "merge_triv \"$modname.$name\" res1 res2 in\n";
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


