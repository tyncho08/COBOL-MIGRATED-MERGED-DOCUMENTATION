#!/bin/bash
# 11.03.18 vbc - Added -Wlinkage to all compiles
# 20.03.21 vbc - Added echos for Compiled xxx
#
for i in `ls irs0*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok; echo "Compiled " $i ; done
cobc -x irs.cbl dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok
cobc -m irsubp.cbl dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok
# Now make irs as a module incase user wishes to run via ACAS
cobc -m irs.cbl dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok ; echo "Compiled IRS"
#
echo "Compiled for No RDBMS"
exit 0
