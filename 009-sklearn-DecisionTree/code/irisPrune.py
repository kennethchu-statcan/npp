
import numpy  as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.tree import DecisionTreeClassifier, export_graphviz, export_text, plot_tree

##############################
def irisPrune( irisData ):

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    iris_TreeClassifier = DecisionTreeClassifier()
    iris_TreeClassifier.fit(X = irisData.data,y = irisData.target)

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    text_iris_TreeClassifier = export_text(iris_TreeClassifier, feature_names = irisData['feature_names'])
    print(text_iris_TreeClassifier)

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    outputFILE = 'plot-iris-TreeClassifier.png'
    plot_iris_TreeClassier = plot_tree(iris_TreeClassifier)
    plt.savefig(fname = outputFILE, dpi = 100)

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my_path = iris_TreeClassifier.cost_complexity_pruning_path(X = irisData.data, y = irisData.target)
    ccp_alphas, impurities = my_path.ccp_alphas, my_path.impurities

    print(ccp_alphas)
    print(impurities)

    temp = np.array([ccp_alphas, impurities])

    df2 = pd.DataFrame(np.transpose(np.array([ccp_alphas, impurities])), columns = ['ccp_alpha','impurity'])
    df2.to_csv(path_or_buf = 'complexity-impurity.csv')
    print(df2)

    fig, ax = plt.subplots()
    ax.plot(ccp_alphas[:-1], impurities[:-1], marker = 'o', drawstyle = "steps-post")
    ax.set_xlabel("effective alpha")
    ax.set_ylabel("total impurity of leaves")
    ax.set_title("Total Impurity vs effective alpha for training set")

    outputFILE = 'plot-impurity-vs-complexity.png'
    plt.savefig(fname = outputFILE, dpi = 100)

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( None )

##############################
