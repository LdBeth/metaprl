#!/usr/bin/perl
if ($#ARGV > 0) {
    $log = "/home/mojave/public_html/metaprl/logs/incomplete_proofs-rev$ARGV[1].txt";
} else {
    chomp($log=`ls -1 ~mojave/public_html/metaprl/logs/incomplete_proofs-* | tail -1`);
};
$cmdline = 'grep \'incomplete proof\' ' . $ARGV[0] . ' | awk \'{print $2}\' | diff ' . $log . ' - | grep \'^>\' | colrm 1 2';
#print("Executing: $cmdline\n");
open(INC, "-|", $cmdline);
while(<INC>) {
    chomp();
    s/\///;
    s/\//./;
    s/(.)/\U$1\E/;
    $inc{$_}=1;
}
close(INC);
$cmdline = 'MP_DEBUG=load editor/ml/mp -batch 2>&1 < /dev/null';
#print("Executing: $cmdline\n");
open(LOAD, "-|", $cmdline);
while(<LOAD>) {
    chomp();
    if (/^Loading .*\./) {
	s/^Loading //;
	if (defined ($inc{$_})) {
	    s/\./\//;
	    s/(.)/\/\L$1\E/;
	    print("New incomplete: ", $_, "\n");
	}
    }
}
close(LOAD);
