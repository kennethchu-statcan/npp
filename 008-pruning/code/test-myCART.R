
test.myCART <- function(
    ) {

    thisFunctionName <- "test.myCART";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(rpart);
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
    list.pruning.sequence <- myCART.object$get_pruning_sequence();
    cat("\nstr(list.pruning.sequence)\n");
    print( str(list.pruning.sequence)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = Species ~ .,
        data    = iris,
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

    cat("\nprintcp(results.rpart)\n");
    printcp( results.rpart );

    cat("\nresults.rpart[['cptable']]\n");
    print( results.rpart[['cptable']]   );

    cat("\nresults.rpart[['cptable']][,'CP']\n");
    print( results.rpart[['cptable']][,'CP']   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.temp <- rbind(
        data.frame(
            x = results.rpart[['cptable']][,'CP'],
            y = rep(0,nrow(results.rpart[['cptable']]))
            ),
        data.frame(
            x = as.numeric(lapply( X = list.pruning.sequence, FUN = function(x) {x[['alpha']]} )),
            y = rep(1,length(list.pruning.sequence))
            )
        );

    png('temp-cost-complexity.png');
    plot( x = DF.temp[,'x'] , y = DF.temp[,'y'] );
    dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.subtree.sequence <- myCART.object$public_subtree_sequence();

    cat("\nstr(list.subtree.sequence)\n");
    print( str(list.subtree.sequence)   );

    cat("\nlist.subtree.sequence\n");
    print( list.subtree.sequence   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nodes <- myCART.object$public_nodes_to_table();

    index.subtree <- 1;
    list.pruned.subtrees <- list();
    list.pruned.subtrees[[index.subtree]] <- list(alpha = 0, subtree = DF.nodes);

    DF.temp <- DF.nodes;
    while ( nrow(DF.temp) > 1 ) {
        index.subtree <- index.subtree + 1;
        DF.temp       <- compute.g(DF.input = DF.temp);
        DF.temp       <- prune.g.minimizers( DF.input = DF.temp);
        list.pruned.subtrees[[index.subtree]] <- list(
            alpha   = min(DF.temp[,'myCART.g'], na.rm = TRUE),
            subtree = DF.temp
            );
        }

    cat("\nstr(list.pruned.subtrees)\n");
    print( str(list.pruned.subtrees)   );

    cat("\nlist.pruned.subtrees\n");
    print( list.pruned.subtrees   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
