#!/usr/bin/env python

import os, getpass, shutil, stat, sys, time, pprint, logging, datetime

thisScript = sys.argv[0]
srcDIR     = sys.argv[1]
outDIR     = sys.argv[2]

# create outDIR if not yet exists
if not os.path.exists(outDIR):
    os.makedirs(outDIR)

# change directory to outDIR
os.chdir(outDIR)

# copy srcDIR to srcCOPY
srcCOPY = os.path.join(outDIR,"code")
shutil.copytree(src = srcDIR, dst = srcCOPY)

# append module path with srcCOPY
sys.path.append(srcCOPY)

# print system time
myTime = "system time: " + datetime.datetime.now().strftime("%c")
print( "\n" + myTime + "\n" )
print("####################")

#################################################
#################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# loading iris data
from getIrisData import getIrisData, examineIrisData
irisData = getIrisData(
    irisFILE = os.path.join(outDIR,"iris.pickle")
    )

examineIrisData(irisData = irisData)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# from irisClassificationTree import irisClassificationTree
# irisClassificationTree(irisData = irisData)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# from irisRegressionTree import irisRegressionTree
# irisRegressionTree(irisData = irisData)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
from irisPrune import irisPrune
irisPrune(irisData = irisData)

#################################################
#################################################
print("\n####################\n")
myTime = "system time: " + datetime.datetime.now().strftime("%c")
print( myTime + "\n" )
