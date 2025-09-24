#!/bin/bash
# 31/01/18 vbc - To include dummy-rdbms in place of *MT routines
#                      that call rdb client libraries.
# 04/02/18 vbc - Cleanup cobc steering so that -I removed and api lib included.
#
# 04/02/18 vbc - This version of script for compiling for NON RDBMS system.
# 11/03/18 vbc - Added  -Wlinkage to all compiles.
# 06/05/18 vbc - Added compile for acasconvert1 IRS default layout change.
# 07/12/22 vbc - Extra text for echo msgs and use of -Wno-goto-section added
#                to remove the silly default warning in latest gnucobol v3.2.
# 21/06/23 vbc - Added program acas-get-params
#

cobc -m dummy-rdbmsMT.cbl;echo "Compiled dummy-rdbmsMT entry points"
cobc -m ACAS-Sysout.cbl; echo "Compiled ACAS-Sysout"
#
cp -vf dummy-rdbmsMT.* ../stock
cp -vf dummy-rdbmsMT.* ../sales
cp -vf dummy-rdbmsMT.* ../purchase
cp -vf dummy-rdbmsMT.* ../irs
cp -vf dummy-rdbmsMT.* ../general
echo "Copied dummy-rdbmsMT.so to irs, general, purchase, sales, stock dirs."
#
for i in `ls maps0*.cbl`; do cobc -m $i; echo "Compiled " $i; done
echo "compiled all maps"

for i in `ls acas0*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl; echo "Compiled " $i; done
for i in `ls acasirsub*.cbl`; do cobc -m $i dummy-rdbmsMT.cbl; echo "Compiled " $i; done
echo "Compiled all acas*"

cobc -m fhlogger.cbl dummy-rdbmsMT.cbl;echo "Compiled fhlogger"

cobc -m xl150.cbl dummy-rdbmsMT.cbl
cobc -m sys002.cbl dummy-rdbmsMT.cbl
echo "compiled xl150 & sys002"
#
cobc -x ACAS.cbl dummy-rdbmsMT.cbl;echo "Compiled ACAS.cbl"
#
#cobc -x acasconvert1.cbl
#echo "Compiled acasconvert1.cbl - IRS Default file layout changes"
for i in `ls *UNL.cbl`; do cobc -x $i; done
echo "Compiled *UNL.cbl"

for i in `ls *RES.cbl`; do cobc -x $i; done
echo "Compiled *RES.cbl"

echo "Compiled for No RDBMS"
exit 0
