BEGIN { ctot=0; crb=0; cprim=0; itot=0; trb=0; iprim=0 }
/is a derived object with a complete proof/ {
   ctot++; crb+=$2; cprim+=$3
}
/is a derived object with an incomplete proof/ {
   itot++; irb+=$2; iprim+=$3
}
END { 
   printf "\nComplete proofs: %i (%i rule boxes, %i primitive steps\n",ctot,crb,cprim;
   printf "Incomplete proofs: %i (%i rule boxes, %i primitive steps\n\n",itot,irb,iprim;
}
