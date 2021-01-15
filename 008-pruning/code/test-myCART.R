
test.myCART <- function(
    ) {

    thisFunctionName <- "test.myCART";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

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

    list.pruning.sequence <- myCART.object$get_pruning_sequence();
    cat("\nstr(list.pruning.sequence)\n");
    print( str(list.pruning.sequence)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
