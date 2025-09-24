#!/bin/bash
# 11/03/18 vbc - Added  -Wlinkage to all compiles.
for i in `ls gl*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl -Wlinkage -I ../copybooks -lz -fdump=ws -fmissing-statement=ok; echo "Compiled " $i; done
cobc -x general.cbl dummy-rdbmsMT.cbl -Wlinkage -I ../copybooks -lz -fdump=ws -fmissing-statement=ok; echo "Compiled general"
# Now make general as a module incase user wishes to run via ACAS
cobc -m general.cbl dummy-rdbmsMT.cbl -Wlinkage -I ../copybooks -lz -fdump=ws -fmissing-statement=ok
#
echo "Compiled for No RDBMS"
exit 0
