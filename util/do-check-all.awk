BEGIN { ctot=0; crb=0; cprim=0; itot=0; trb=0; iprim=0 }
/is a derived object with a complete grounded proof/ {
   ctot++; crb+=$2; cprim+=$3
}
/is a derived object with an .*incomplete.* proof/ {
   itot++; irb+=$2; iprim+=$3
}
/complete .*ungrounded.* proof .* that depends on an incomplete/ {
   utot++; urb+=$2; uprim+=$3; i=split($NF,fields,/ /); num[fields[i]]++;
}
END {
   printf "\nComplete proofs: %i (%i rule boxes, %i primitive steps)\n",ctot,crb,cprim;
   printf "Ungrounded proofs: %i (%i rule boxes, %i primitive steps)\n",utot,urb,uprim;
   printf "Incomplete proofs: %i (%i rule boxes, %i primitive steps)\n\n",itot,irb,iprim;
   if (utot>0) {
      print "Ungrounded proofs depend on following incomplete ones:\n";
      for (i in num) { ind = sprintf("%8i proofs depend on an incomplete %s",num[i],i); unum[ind]=ind};
      for (i=asort(unum);i>0;i--) print unum[i];
      print ""
   }
}
