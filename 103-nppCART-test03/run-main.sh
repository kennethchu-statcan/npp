#!/bin/bash

currentDIR=`pwd`
   codeDIR=${currentDIR}/code
 outputDIR=${currentDIR//github/gittmp}/output

# parentDIR=`dirname ${currentDIR}`
#   dataDIR=${parentDIR}/000-data

if [ ! -d ${outputDIR} ]; then
	mkdir -p ${outputDIR}
fi

cp -r ${codeDIR} ${outputDIR}
cp    $0         ${outputDIR}/code

##################################################
globalSeed=1234567

### ~~~~~ ###
# myRscript=${codeDIR}/main-00-sanity.R
# stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
# stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
# R --no-save --args ${codeDIR} ${outputDIR} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}

### ~~~~~ ###
# myPyScript=${codeDIR}/main-00-sanity-tree-hierarchy.py
# stdoutFile=${outputDIR}/output-00-sanity-tree-hierarchy/stdout.py.`basename ${myPyScript} .py`
# stderrFile=${outputDIR}/output-00-sanity-tree-hierarchy/stderr.py.`basename ${myPyScript} .py`
# python ${myPyScript} ${codeDIR} ${outputDIR}/output-00-sanity-tree-hierarchy > ${stdoutFile} 2> ${stderrFile}

### ~~~~~ ###
myRscript=${codeDIR}/main-01-simulations.R
stdoutFile=${outputDIR}/stdout.R.`basename ${myRscript} .R`
stderrFile=${outputDIR}/stderr.R.`basename ${myRscript} .R`
R --no-save --args ${codeDIR} ${outputDIR} < ${myRscript} > ${stdoutFile} 2> ${stderrFile}

##################################################
exit
