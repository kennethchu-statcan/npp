
test.nppCART.iris <- function(seed = 1234567) {

    thisFunctionName <- "test.nppCART.iris";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    data(iris);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    self.selected      <- sample(x = c(TRUE,FALSE), size = nrow(iris), replace = TRUE, prob = c(1,2)/3 );
    DF.non.probability <- iris[self.selected,];

    DF.probability <- iris;
    DF.probability[,"design.weight"] <- 1;

    cat("\nstr(DF.non.probability)\n");
    print( str(DF.non.probability)   );

    cat("\nstr(DF.probability)\n");
    print( str(DF.probability)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.iris <- DF.probability;
    DF.iris[,"self.selected"] <- self.selected;
    write.csv(
        x         = DF.iris,
        file      = "DF-iris.csv",
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = self.selected ~ .,
        data    = DF.iris[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width",'self.selected')],
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    cat("\nresults.rpart\n");
    print( results.rpart   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.object <- myCART$new(
        formula = self.selected ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data    = DF.iris
        );

    myCART.object$grow();

    cat("\nmyCART.object$print()\n");
    print( myCART.object$print()   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART.object <- nppCART(
        np.data       = DF.non.probability,
        p.data        = DF.probability,
        predictors    = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
        weight        = "design.weight",
        min.cell.size = 1,
        min.impurity  = 1e-9,
        max.levels    = 10000
        );

    my.nppCART.object$grow();

    cat("\nmy.nppCART.object$print()\n");
    print( my.nppCART.object$print()   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART.subtree.hierarchy <- my.nppCART.object$public_get_subtree_hierarchy();

    # cat("\nstr(my.nppCART.subtree.hierarchy)\n");
    # print( str(my.nppCART.subtree.hierarchy)   );

    # cat("\nmyCART.subtree.hierarchy\n");
    # print( myCART.subtree.hierarchy   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( index.subtree in seq(1,length(my.nppCART.subtree.hierarchy)) ) {
        cat("\n")
        cat(paste0("\n### index.subtree: ",index.subtree,"\n"));
        cat("\nmy.nppCART.subtree.hierarchy[[index.subtree]][['alpha']]:\n");
        print( my.nppCART.subtree.hierarchy[[index.subtree]][['alpha']]    );
        cat("\nmy.nppCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]:\n");
        print( my.nppCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]    );
        cat("\nprint_nodes(nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']])\n");
        print_nodes(nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }
