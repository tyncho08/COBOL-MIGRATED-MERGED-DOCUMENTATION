#!/bin/bash
for i in `ls st*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok; echo "compiled " $i; done
cobc -x stock.cbl dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok; echo "compiled Stock"
# Now make stock as a module incase user wishes to run via ACAS
cobc -m stock.cbl dummy-rdbmsMT.cbl -I ../copybooks -Wlinkage -lz -fdump=ws -fmissing-statement=ok
echo "Compiled for No RDBMS"
exit 0
