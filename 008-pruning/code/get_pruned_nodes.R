
get_pruned_nodes <- function(
    input.nodes  = NULL,
    pruning.info = NULL
    ) {

    thisFunctionName <- "get_pruned_nodes";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # cat("\nstr(input.nodes)\n");
    # print( str(input.nodes)   );
    #
    # cat("\nstr(pruning.info)\n");
    # print( str(pruning.info)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    nodes_untouched <- pruning.info[['nodes_untouched']];
    nodes.pruned.at <- pruning.info[['nodes_pruned_at']];
    nodes_removed   <- pruning.info[['nodes_removed'  ]];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.nodes <- list();
    for ( i in 1:length(input.nodes) ) {
        temp.node <- input.nodes[[i]];
        if ( temp.node[['nodeID']] %in% nodes_removed ) {
            # do nothing
        } else if ( temp.node[['nodeID']] %in% nodes_untouched ) {
            output.nodes[[1+length(output.nodes)]] <- temp.node;
        } else if ( temp.node[['nodeID']] %in% nodes.pruned.at ) {
            temp.node[['notSatisfiedChildID']]     <- NULL;
            temp.node[[   'satisfiedChildID']]     <- NULL;
            temp.node[[     'splitCriterion']]     <- NULL;
            output.nodes[[1+length(output.nodes)]] <- temp.node;
            }
        }

    # cat("\nstr(output.nodes)\n");
    # print( str(output.nodes)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nnodes.pruned.at:\n");
    print( nodes.pruned.at    );

    cat("\n");

    cat("\nprint_nodes(nodes = input.nodes)\n");
    print_nodes(nodes = input.nodes);

    cat("\n");

    cat("\nprint_nodes(nodes = output.nodes)\n");
    print_nodes(nodes = output.nodes);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( output.nodes );

    }

##################################################
print_nodes <- function(
    nodes      = NULL,
    FUN.format = function(x) { return(x) }
    ) {
    if ( 0 == length(nodes) ) {
        cat("\nlist of nodes is empty.\n")
        }
    else {
        for ( i in seq(1,length(nodes)) ) {
            nodes[[i]]$print_node(FUN.format = FUN.format);
            }
        cat("\n");
        }
    }
