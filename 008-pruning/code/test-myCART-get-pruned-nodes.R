
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
    list.subtree.sequence <- myCART.object$public_subtree_sequence();

    cat("\nstr(list.subtree.sequence)\n");
    print( str(list.subtree.sequence)   );

    cat("\nlist.subtree.sequence\n");
    print( list.subtree.sequence   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # temp.nodes <- myCART.object$nodes;
    # for ( subtree.index in seq(2,length(list.subtree.sequence)) ) {
    #     temp.nodes <- get_pruned_nodes(
    #         input.nodes  = temp.nodes,
    #         pruning.info = list.subtree.sequence[[subtree.index]]
    #         );
    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( index.subtree in seq(1,length(list.subtree.sequence)) ) {
        cat("\n")
        cat(paste0("\n### index.subtree: ",index.subtree,"\n"));
        cat("\nlist.subtree.sequence[[index.subtree]][['alpha']]:\n");
        print( list.subtree.sequence[[index.subtree]][['alpha']]    );
        cat("\nlist.subtree.sequence[[index.subtree]][['nodes_pruned_at']]:\n");
        print( list.subtree.sequence[[index.subtree]][['nodes_pruned_at']]    );
        cat("\nprint_nodes(nodes = list.subtree.sequence[[index.subtree]][['pruned_nodes']])\n");
        print_nodes(nodes = list.subtree.sequence[[index.subtree]][['pruned_nodes']]);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
