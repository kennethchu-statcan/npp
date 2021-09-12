
test.myCART.grow <- function(
    ) {

    thisFunctionName <- "test.myCART.grow";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(rpart);
    data(iris);

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

    # cat("\nstr(results.rpart)\n");
    # print( str(results.rpart)   );
    #
    # cat("\nprintcp(results.rpart)\n");
    # printcp( results.rpart );
    #
    # cat("\nresults.rpart[['cptable']]\n");
    # print( results.rpart[['cptable']]   );
    #
    # cat("\nresults.rpart[['cptable']][,'CP']\n");
    # print( results.rpart[['cptable']][,'CP']   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myCART.object <- myCART$new(
        formula    = Species ~ .,
        data       = iris
        );

    myCART.object$grow();

    cat("\nmyCART.object$print()\n");
    print( myCART.object$print()   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # myCART.subtree.hierarchy <- myCART.object$public_get_subtree_hierarchy();
    #
    # cat("\nstr(myCART.subtree.hierarchy)\n");
    # print( str(myCART.subtree.hierarchy)   );

    # cat("\nmyCART.subtree.hierarchy\n");
    # print( myCART.subtree.hierarchy   );

    # DF.temp <- rbind(
    #     data.frame(
    #         x = results.rpart[['cptable']][,'CP'],
    #         y = rep(0,nrow(results.rpart[['cptable']]))
    #         ),
    #     data.frame(
    #         x = as.numeric(lapply( X = myCART.subtree.hierarchy, FUN = function(x) {x[['alpha']]} )),
    #         y = rep(1,length(myCART.subtree.hierarchy))
    #         )
    #     );
    #
    # png('temp-cost-complexity.png');
    # plot( x = DF.temp[,'x'] , y = DF.temp[,'y'] );
    # dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
