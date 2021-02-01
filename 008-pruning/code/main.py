#!/usr/bin/env python

import os, getpass, shutil, stat, sys, time, pprint, logging, datetime

thisScript = sys.argv[0]
srcDIR     = sys.argv[1]
outDIR     = sys.argv[2]

# change directory to outDIR
os.chdir(outDIR)

# append module path with srcCOPY
srcCOPY = os.path.join(outDIR,"code")
sys.path.append(srcCOPY)

# print system time
myTime = "system time: " + datetime.datetime.now().strftime("%c")
print( "\n" + myTime + "\n" )
print("####################")

#################################################
#################################################
import pandas
import numpy
import matplotlib.pyplot as plt

from sklearn.tree import DecisionTreeClassifier, export_graphviz, export_text, plot_tree

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF_population = pandas.read_csv(filepath_or_buffer = "DF-population.csv")

DF_population['x1'] = DF_population['x1'].str.replace(pat = 'small',  repl = '0.5')
DF_population['x1'] = DF_population['x1'].str.replace(pat = 'medium', repl = '1.5')
DF_population['x1'] = DF_population['x1'].str.replace(pat = 'large',  repl = '2.5')

DF_population['x2'] = DF_population['x2'].str.replace(pat = 'petit', repl = '0.5')
DF_population['x2'] = DF_population['x2'].str.replace(pat = 'moyen', repl = '1.5')
DF_population['x2'] = DF_population['x2'].str.replace(pat = 'grand', repl = '2.5')

DF_population['x1'] = pandas.to_numeric(DF_population['x1'])
DF_population['x2'] = pandas.to_numeric(DF_population['x2'])

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my_TreeClassifier = DecisionTreeClassifier()
my_TreeClassifier.fit(X = DF_population.loc[:,['x1','x2']], y = DF_population.loc[:,['self.selected']])

text_my_TreeClassifier = export_text(my_TreeClassifier, feature_names = ['x1','x2'])
print(text_my_TreeClassifier)

outputFILE = 'plot-my-TreeClassifier.png'
plot_my_TreeClassifier = plot_tree(my_TreeClassifier)
plt.savefig(fname = outputFILE, dpi = 100)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my_path = my_TreeClassifier.cost_complexity_pruning_path(X = DF_population.loc[:,['x1','x2']], y = DF_population.loc[:,['self.selected']])
ccp_alphas, impurities = my_path.ccp_alphas, my_path.impurities

print(ccp_alphas)
print(impurities)

temp = numpy.array([ccp_alphas, impurities])

df2 = pandas.DataFrame(numpy.transpose(numpy.array([ccp_alphas, impurities])), columns = ['ccp_alpha','impurity'])
df2.to_csv(path_or_buf = 'complexity-impurity.csv')
print(df2)

fig, ax = plt.subplots()
ax.plot(ccp_alphas[:-1], impurities[:-1], marker = 'o', drawstyle = "steps-post")
ax.set_xlabel("effective alpha")
ax.set_ylabel("total impurity of leaves")
ax.set_title("Total Impurity vs effective alpha for training set")

outputFILE = 'plot-impurity-vs-complexity.png'
plt.savefig(fname = outputFILE, dpi = 100)

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

#################################################
#################################################
print("\n####################\n")
myTime = "system time: " + datetime.datetime.now().strftime("%c")
print( myTime + "\n" )
