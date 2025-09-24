#!/bin/bash
# 11/03/18 vbc Added -Wlinkage to all compiles.
for i in `ls pl*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl -Wlinkage -lz; echo "Compiled " $i; done
cobc -x purchase.cbl dummy-rdbmsMT.cbl -Wlinkage -lz
# Now make purchase as a module incase user wishes to run via ACAS
cobc -m purchase.cbl dummy-rdbmsMT.cbl -Wlinkage -lz; echo "Compiled purchase"
echo "Compiled for No RDBMS"
exit 0
