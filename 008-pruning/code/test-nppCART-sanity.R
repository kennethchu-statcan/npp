
test.nppCART.sanity <- function(seed = 1234567) {

    thisFunctionName <- "test.nppCART.sanity";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(seed = seed);
    list.samples  <- test.nppCART_get.samples(
        DF.population         = DF.population,
        RData.non.probability = "DF-non-probability.RData",
        RData.probability     = "DF-probability.RData"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.probability <- DF.population[,c("unit.ID","x1","x2")];
    DF.probability[,"design.weight"] <- 1;

    my.nppCART <- nppCART(
        np.data       = list.samples[['DF.non.probability']],
        p.data        = DF.probability,
        predictors    = c("x1","x2"),
        weight        = "design.weight",
        min.cell.size = 1,
        min.impurity  = 1e-9,
        max.levels    = 10000
        );

    my.nppCART$grow();
    # cat("\nmy.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} )\n");
    # my.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} );

    my.nppCART.subtree.hierarchy <- my.nppCART$public_get_subtree_hierarchy();
    # cat("\nstr(my.nppCART.subtree.hierarchy)\n");
    # print( str(my.nppCART.subtree.hierarchy)   );

    temp.alphas.nppCART <- as.numeric(sapply(
        X   = my.nppCART.subtree.hierarchy,
        FUN = function(x) { return(x[['alpha']]) }
        ));

    # cat("\ntemp.alphas.nppCART.\n");
    # print( temp.alphas.nppCART   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    write.csv(
        x         = DF.population,
        file      = "DF-population.csv",
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = self.selected ~ .,
        data    = DF.population[,c('x1','x2','self.selected')],
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    # cat("\nresults.rpart\n");
    # print( results.rpart   );

    png("plot-rpart.png");
    rpart.plot(
        x = results.rpart
        );
    dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.object <- myCART$new(
        formula    = self.selected ~ .,
        data       = DF.population[,c('x1','x2','self.selected')]
        );

    myCART.object$grow();
    # cat("\nmyCART.object$print()\n");
    # print( myCART.object$print()   );

    myCART.subtree.hierarchy <- myCART.object$public_get_subtree_hierarchy();
    # cat("\nstr(myCART.subtree.hierarchy)\n");
    # print( str(myCART.subtree.hierarchy)   );

    temp.alphas.myCART <- as.numeric(sapply(
        X   = myCART.subtree.hierarchy,
        FUN = function(x) { return(x[['alpha']]) }
        ));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\ntemp.alphas.nppCART.\n");
    print( temp.alphas.nppCART   );

    cat("\ntemp.alphas.myCART.\n");
    print( temp.alphas.myCART   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nresults.rpart\n");
    print( results.rpart   );

    cat("\nmy.nppCART$print()\n");
    my.nppCART$print( FUN.format = function(x) {return(round(x,digits=5))} );

    cat("\nmyCART.object$print()\n");
    myCART.object$print( FUN.format = function(x) {return(round(x,digits=5))} );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }
