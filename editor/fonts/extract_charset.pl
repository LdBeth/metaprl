#!/usr/bin/perl -w

open EX, "theories/tactic/nuprl_font.ml";

LINE: while (<EX>) {
    chomp;
    next LINE unless /mode.prl.*\`\"\\[0-9]/;
    s/^.*:: *//;
    next LINE if (/\\[0-9]*\\[0-9]/);
    s/ *= *\`\"\\/=/;
    s/"$//;
    next LINE if (/\*\)/);
    ($mpname,$xname)=split(/=/);
    # print "mpname=$mpname; xname=$xname;\n";
    $xxname=sprintf("/x%02x",$xname);
    chomp($uni=`grep \':: mode.html. :: $mpname \' theories/tactic/nuprl_font.ml`);
    if ($uni=~/html_uni/) {
	$uni=~s/^.*html_uni\[//;
        $uni=~s/\]//;
	if ($uni < 65536) {
	    $hex=sprintf("%04X",$uni);
        } else {
	    $hex=sprintf("%08X",$uni);
	}
        chomp($name=`zgrep \'<U$hex>\' /usr/share/i18n/charmaps/UTF-8.gz`);
	$name=~s/[[:space:]]*\/x.*\/x..[[:space:]]*/=/;
	($ucode,$uname)=split(/=/,$name);
	# print "$mpname: unicode $uni (0x$hex) ucode: $ucode, uname: $uname;\n";
    } else { $ucode="" };
    if ($ucode=~/U[0-9]/) {
	print "$ucode\t$xxname\t$uname\n";
    } else {
	print "Err:$mpname:$xname\t$xxname\t\n";
    }
}
close EX;

