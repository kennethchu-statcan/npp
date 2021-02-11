
test.myCART.get.pruned.nodes <- function(
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
    myCART.subtree.hierarchy <- myCART.object$public_get_subtree_hierarchy();

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
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
