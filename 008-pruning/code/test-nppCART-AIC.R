
test.nppCART.AIC <- function(
    seed           = 1234567,
    prob.selection = 0.1,
    n.replicates   = 500
    ) {

    thisFunctionName <- "test.nppCART.AIC";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(seed = seed);
    list.samples  <- test.nppCART_get.samples(
        DF.population         = DF.population,
        prob.selection        = prob.selection, # 0.1, 1 - 1e-8, # 1.0,
        n.replicates          = n.replicates,
        RData.non.probability = "DF-non-probability.RData",
        RData.probability     = "DF-probability.RData"
        );

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
    cat("\nstr(list.samples[['DF.non.probability']])\n");
    print( str(list.samples[['DF.non.probability']])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # DF.probability <- DF.population[,c("unit.ID","x1","x2")];
    # DF.probability[,"design.weight"] <- 1;

    DF.probability <- list.samples[['DF.probability']];
    cat("\nstr(DF.probability)\n");
    print( str(DF.probability)   )

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART <- nppCART(
        np.data           = list.samples[['DF.non.probability']],
        p.data            = DF.probability,
        predictors        = c("x1","x2"),
        weight            = "design.weight",
        bootstrap.weights = paste0("repweight",seq(1,n.replicates)),
        min.cell.size     = 1,
        min.impurity      = 1e-9,
        max.levels        = 10000
        );

    my.nppCART$grow();
    # cat("\nmy.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} )\n");
    # my.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} );

    cat("\nmy.nppCART$print()\n");
    my.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );

    cat("\nls()\n");
    print( ls()   );

    your.nppCART <- rlang::duplicate(x = my.nppCART, shallow = FALSE);

    cat("\nls()\n");
    print( ls()   );

    cat("\nbase::.Internal(address(my.nppCART))\n");
    print( base::.Internal(address(my.nppCART))   );

    cat("\nbase::.Internal(address(your.nppCART))\n");
    print( base::.Internal(address(your.nppCART))   );

    my.nppCART.subtree.hierarchy <- my.nppCART$public_get_subtree_hierarchy();
    # cat("\nstr(my.nppCART.subtree.hierarchy)\n");
    # print( str(my.nppCART.subtree.hierarchy)   );

    temp.alphas.nppCART <- as.numeric(sapply(
        X   = my.nppCART.subtree.hierarchy,
        FUN = function(x) { return(x[['alpha']]) }
        ));

    cat("\ntemp.alphas.nppCART.\n");
    print( temp.alphas.nppCART   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    # DF.population[                ,'self.selected'] <- FALSE;
    # DF.population[is.self.selected,'self.selected'] <- TRUE;
    #
    # write.csv(
    #     x         = DF.population,
    #     file      = "DF-population.csv",
    #     row.names = FALSE
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nstr(my.nppCART$p.data)\n");
    print( str(my.nppCART$p.data)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # for ( index.subtree in seq(1,length(my.nppCART.subtree.hierarchy)) ) {
    #
    #     cat("\n")
    #     cat(paste0("\n### index.subtree: ",index.subtree,"\n"));
    #
    #     cat("\nmy.nppCART.subtree.hierarchy[[index.subtree]][['alpha']]:\n");
    #     print( my.nppCART.subtree.hierarchy[[index.subtree]][['alpha']]    );
    #     cat("\nmy.nppCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]:\n");
    #     print( my.nppCART.subtree.hierarchy[[index.subtree]][['nodes_pruned_at']]    );
    #
    #     cat("\nprint_nodes(nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']])\n");
    #     print_nodes(nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]);
    #
    #     DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity(
    #         nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
    #         );
    #     cat("\nstr(DF.npdata.with.propensity)\n");
    #     print( str(DF.npdata.with.propensity)   );
    #
    #     DF.pdata.with.nodeID <- my.nppCART$get_pdata_with_nodeID(
    #         nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
    #         );
    #     cat("\nstr(DF.pdata.with.nodeID)\n");
    #     print( str(DF.pdata.with.nodeID)   );
    #
    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.AICs <- data.frame(index.subtree = numeric(), AIC = numeric());
    for ( index.subtree in seq(1,length(my.nppCART.subtree.hierarchy)) ) {

        cat("\n")
        cat(paste0("\n### index.subtree: ",index.subtree,"\n"));

        cat("\nnames(my.nppCART.subtree.hierarchy[[index.subtree]])\n");
        print( names(my.nppCART.subtree.hierarchy[[index.subtree]])   );

        DF.retained <- my.nppCART.subtree.hierarchy[[index.subtree]][['DF_retained']];
        cat("\nDF.retained\n");
        print( DF.retained   );

        # cat("\nmy.nppCART$nodes\n");
        # print( my.nppCART$nodes   );

        DF.nprow.to.leafID <- my.nppCART$public_nprow_to_leafID(
            nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
            );
        cat("\nstr(DF.nprow.to.leafID)\n");
        print( str(DF.nprow.to.leafID)   );

        DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity(
            nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
            );
        cat("\nstr(DF.npdata.with.propensity)\n");
        print( str(DF.npdata.with.propensity)   );

        DF.pdata.with.nodeID <- my.nppCART$get_pdata_with_nodeID(
            nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
            );
        cat("\nstr(DF.pdata.with.nodeID)\n");
        print( str(DF.pdata.with.nodeID)   );

        my.AIC <- compute_AIC(
            DF.retained.nodes         = DF.retained,
            DF.npdata.with.propensity = DF.npdata.with.propensity,
            DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
            sampling.weight.varname   = "design.weight",
            replicate.weight.varnames = paste0("repweight",seq(1,500)),
            combined.weights          = FALSE # TRUE
            );

        print( paste0('\n# index.subtree = ',index.subtree,', my.AIC = ',my.AIC,"\n")   );

        DF.AICs <- rbind(DF.AICs,data.frame(index.subtree = index.subtree, AIC = my.AIC));

        }

    png("plot-AICs.png");
    plot(x = DF.AICs[,'index.subtree'], y = DF.AICs[,'AIC']);
    dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }
