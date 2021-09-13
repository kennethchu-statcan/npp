#!/usr/bin/env python

import os, getpass, shutil, stat, sys, time, pprint, logging, datetime, pandas, numpy
from sklearn.tree import DecisionTreeClassifier, export_graphviz, export_text, plot_tree

def sklearn_impurity_alpha(CSV_population):

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF_population = pandas.read_csv(filepath_or_buffer = CSV_population)

    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'small.1',  repl = '0.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'small.2',  repl = '1.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'small.3',  repl = '2.5')

    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'medium.1', repl = '3.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'medium.2', repl = '4.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'medium.3', repl = '5.5')

    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'large.1',  repl = '6.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'large.2',  repl = '7.5')
    DF_population['x1'] = DF_population['x1'].str.replace(pat = 'large.3',  repl = '8.5')

    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'petit.1',  repl = '0.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'petit.2',  repl = '1.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'petit.3',  repl = '2.5')

    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'moyen.1',  repl = '3.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'moyen.2',  repl = '4.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'moyen.3',  repl = '5.5')

    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'grand.1',  repl = '6.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'grand.2',  repl = '7.5')
    DF_population['x2'] = DF_population['x2'].str.replace(pat = 'grand.3',  repl = '8.5')

    DF_population['x1'] = pandas.to_numeric(DF_population['x1'])
    DF_population['x2'] = pandas.to_numeric(DF_population['x2'])

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my_TreeClassifier = DecisionTreeClassifier()
    my_TreeClassifier.fit(X = DF_population.loc[:,['x1','x2']], y = DF_population.loc[:,['self.selected']])

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my_path = my_TreeClassifier.cost_complexity_pruning_path(X = DF_population.loc[:,['x1','x2']], y = DF_population.loc[:,['self.selected']])
    ccp_alphas, impurities = my_path.ccp_alphas, my_path.impurities

    temp = numpy.array([ccp_alphas, impurities])

    df2 = pandas.DataFrame(numpy.transpose(numpy.array([ccp_alphas, impurities])), columns = ['ccp_alpha','impurity'])
    # df2.to_csv(path_or_buf = "output-DF-sklearn-alpha-impurity.csv")
    # print(df2)

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return df2

##################################################
DF_sia = sklearn_impurity_alpha(CSV_population = "DF-hierarchy-population-with-self-selection.csv")
DF_sia.to_csv(path_or_buf = "output-DF-sklearn-alpha-impurity.csv")
