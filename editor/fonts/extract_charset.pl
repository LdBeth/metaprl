#!/usr/bin/perl -w

open EX, "grep html_uni theories/tactic/nuprl_font.ml|";

LINE: while (<EX>) {
    chomp;
    next LINE unless /html_uni\[[0-9]/;
    s/^.*:: *//;
    s/[[:space:]]*= html_uni\[/=/;
    s/\]//;
    ($mpname,$uni)=split(/=/);
    if ($uni < 65536) {
	$hex=sprintf("%04X",$uni);
    } else {
	$hex=sprintf("%08X",$uni);
    }
    chomp($name=`zgrep \\<U$hex\\> /usr/share/i18n/charmaps/UTF-8.gz`);
    $name=~s/[[:space:]]*\/x.*\/x..[[:space:]]*/=/;
    ($ucode,$uname)=split(/=/,$name);
    chomp($xname=`grep \':: mode.prl. :: $mpname \' theories/tactic/nuprl_font.ml`);
    $xname=~s/^.*= *\`\"\\//;
    $xname=~s/"//;
    if ( ($xname=~/^[0-9]/)&& (! ($xname=~/\\/)) && ($xname > 0)) {
	$xxname=sprintf("/x%02x",$xname);
	print "$ucode\t$xxname\t$uname\n";
    } else {
	print STDERR "Can not locate $mpname ($ucode $uname) \n";
    }
}
close EX;

