#!/bin/sh
cd ../theories/tactic
grep 'mode.prl.*::.*`"[^"]' nuprl_font.ml | \
sed -e 's|.*::[[:blank:]]*||' -e 's|[[:blank:]]*=[[:blank:]]*|: |' | \
awk '
BEGIN { COL=1 }
(COL<3) { cols[COL]=$0; COL++; next }
(COL>=3) { printf "%25s %25s %25s\n", cols[1], cols[2], $0; COL=1; next }
END { if (COL==2) { printf "%25s\n", cols[1] }; if (COL==3) { printf "%25s %25s\n", cols[1], cols[2] }}'


