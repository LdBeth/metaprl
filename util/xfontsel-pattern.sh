#!/bin/sh
cd ../theories/tactic
grep 'mode.prl.*::.*`"[^"]' nuprl_font.ml | \
sed -e 's|.*::[[:blank:]]*||' -e 's|[[:blank:]]*=[[:blank:]]*|: |' | \
awk '
BEGIN { COL=1 }
(COL<3) { cols[COL]=$0; COL++; next }
(COL>=3) { print cols[1] "  " cols[2] "  " $0; COL=1; next }
END { if (COL==2) { print cols[1] }; if (COL==3) { print cols[1] "  " cols[2] }}' |
LC_ALL=en_US.UTF-8 column -t


