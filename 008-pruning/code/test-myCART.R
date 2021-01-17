
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
    DF.nodes  <- myCART.object$public_nodes_to_table();
    cat("\nDF.nodes\n");
    print( DF.nodes   );

    DF.temp.1 <- update.branches(DF.input = DF.nodes );
    DF.temp.1 <- prune.branches( DF.input = DF.temp.1);
    cat("\nDF.temp.1\n"); print(DF.temp.1);
    cat("\nmin(DF.temp.1[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.1[,'myCART.g'],na.rm=TRUE)   );

    DF.temp.2 <- update.branches(DF.input = DF.temp.1);
    DF.temp.2 <- prune.branches( DF.input = DF.temp.2);
    cat("\nDF.temp.2\n"); print(DF.temp.2);
    cat("\nmin(DF.temp.2[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.2[,'myCART.g'],na.rm=TRUE)   );

    DF.temp.3 <- update.branches(DF.input = DF.temp.2);
    DF.temp.3 <- prune.branches( DF.input = DF.temp.3);
    cat("\nDF.temp.3\n"); print(DF.temp.3);
    cat("\nmin(DF.temp.3[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.3[,'myCART.g'],na.rm=TRUE)   );

    DF.temp.4 <- update.branches(DF.input = DF.temp.3);
    DF.temp.4 <- prune.branches( DF.input = DF.temp.4);
    cat("\nDF.temp.4\n"); print(DF.temp.4);
    cat("\nmin(DF.temp.4[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.4[,'myCART.g'],na.rm=TRUE)   );

    DF.temp.5 <- update.branches(DF.input = DF.temp.4);
    DF.temp.5 <- prune.branches( DF.input = DF.temp.5);
    cat("\nDF.temp.5\n"); print(DF.temp.5);
    cat("\nmin(DF.temp.5[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.5[,'myCART.g'],na.rm=TRUE)   );

    DF.temp.6 <- update.branches(DF.input = DF.temp.5);
    DF.temp.6 <- prune.branches( DF.input = DF.temp.6);
    cat("\nDF.temp.6\n"); print(DF.temp.6);
    cat("\nmin(DF.temp.6[,'myCART.g'],na.rm=TRUE)\n");
    print( min(DF.temp.6[,'myCART.g'],na.rm=TRUE)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
