
test.myCART.categorical.predictors <- function(
    ) {

    thisFunctionName <- "test.myCART.categorical.predictors";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(rpart);
    require(titanic);
    require(tree);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.titanic <- test.myCART.categorical.predictors_get.titanic();

    cat("\nstr(DF.titanic)\n");
    print( str(DF.titanic)   );

    cat("\nsummary(DF.titanic)\n");
    print( summary(DF.titanic)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.object <- myCART$new(
        formula = survived ~ pclass + embarked + sibsp,
        # formula = survived ~ .,
        # formula = survived ~ pclass + embarked,
        # formula = survived ~ age + pclass + embarked,
        # formula = survived ~ sex + pclass + embarked,
        data    = DF.titanic
        );

    myCART.object$grow();
    cat("\nmyCART.object$print()\n");
    print( myCART.object$print()   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.tree <- tree(
        formula = survived ~ pclass + embarked + sibsp,
        # formula = survived ~ .,
        # formula = survived ~ pclass + embarked,
        data    = DF.titanic,
        split   = "gini",
        control = tree.control(
            nobs    = nrow(DF.titanic),
            mincut  = 1,
            minsize = 2,
            mindev  = 1e-50
            )
        );

    cat("\nresults.tree\n");
    print( results.tree   );

    cat("\nstr(results.tree)\n");
    print( str(results.tree)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = survived ~ pclass + embarked + sibsp,
        # formula = survived ~ .,
        # formula = survived ~ pclass + embarked,
        data    = DF.titanic,
        method  = "anova", # "class",
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    cat("\nresults.rpart\n");
    print( results.rpart   );

    cat("\nstr(results.rpart)\n");
    print( str(results.rpart)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    data(iris);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.object <- myCART$new(
        formula    = Species ~ .,
        data       = iris
        );

    myCART.object$grow();
    cat("\nmyCART.object$print()\n");
    print( myCART.object$print()   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.subtree.hierarchy <- myCART.object$public_get_subtree_hierarchy();

    temp.alphas.myCART <- as.numeric(sapply(
        X   = myCART.subtree.hierarchy,
        FUN = function(x) { return(x[['alpha']]) }
        ));

    cat("\nstr(myCART.subtree.hierarchy)\n");
    print( str(myCART.subtree.hierarchy)   );

    # cat("\nmyCART.subtree.hierarchy\n");
    # print( myCART.subtree.hierarchy   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( index.subtree in seq(1,length(myCART.subtree.hierarchy)) ) {
        cat("\n")
        cat(paste0("\n### index.subtree: ",index.subtree,"\n"));
        cat("\nmyCART.subtree.hierarchy[[index.subtree]][['alpha']]:\n");
        print( myCART.subtree.hierarchy[[index.subtree]][['alpha']]    );
        cat("\nmyCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]:\n");
        print( myCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]    );
        cat("\nprint_nodes(nodes = myCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']])\n");
        print_nodes(nodes = myCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\ntemp.alphas.myCART.\n");
    print( temp.alphas.myCART   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
test.myCART.categorical.predictors_get.titanic <- function() {

    DF.output <- titanic_train;
    colnames(DF.output) <- tolower(colnames(DF.output));

    retained.colnames <- c('survived','sex','age','pclass','sibsp','embarked');
    DF.output <- DF.output[,retained.colnames];

    temp.survived <- DF.output[,'survived'];
    DF.output[,                  'survived'] <- as.logical(rep(FALSE,nrow(DF.output)));
    DF.output[1 == temp.survived,'survived'] <- TRUE;

    DF.output["" == DF.output[,'embarked'],'embarked'] <- "N";

    DF.output[,'sex'     ] <- as.factor(as.character(DF.output[,'sex'     ]));
    DF.output[,'pclass'  ] <- as.factor(as.character(DF.output[,'pclass'  ]));
    DF.output[,'sibsp'   ] <- as.factor(as.character(DF.output[,'sibsp'   ]));
    DF.output[,'embarked'] <- as.factor(as.character(DF.output[,'embarked']));

    # cat("\nstr(DF.output)\n");
    # print( str(DF.output)   );
    #
    # cat("\nsummary(DF.output)\n");
    # print( summary(DF.output)   );

    return( DF.output );

    }
