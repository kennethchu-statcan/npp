#!/bin/bash

currentDIR=`pwd`
#parentDIR="$(dirname "$currentDIR")"
   srcDIR=${currentDIR}/code
   outDIR=${currentDIR//github/gittmp}/output

if [ ! -d ${outDIR} ]; then
	mkdir -p ${outDIR}
fi

# cp -r ${srcDIR} ${outDIR}

##################################################
myPyScript=${srcDIR}/main.py
stdoutFile=stdout.py.`basename ${myPyScript} .py`
stderrFile=stderr.py.`basename ${myPyScript} .py`
python ${myPyScript} ${srcDIR} ${outDIR} > ${outDIR}/${stdoutFile} 2> ${outDIR}/${stderrFile}

##################################################
exit
