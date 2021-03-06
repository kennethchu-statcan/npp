
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
        sampling.weight   = "design.weight",
        bootstrap.weights = paste0("repweight",seq(1,n.replicates)),
        min.cell.size     = 1,
        min.impurity      = 1e-9,
        max.levels        = 10000
        );

    my.nppCART$grow();
    cat("\nmy.nppCART$print()\n");
    my.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );

    DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
    cat("\nstr(DF.npdata.with.propensity)\n");
    print( str(DF.npdata.with.propensity)   );

    DF.alpha.AIC <- my.nppCART$get_alphas_AICs();
    cat("\nDF.alpha.AIC\n");
    print( DF.alpha.AIC   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.alpha <- initializePlot(
        title    = NULL,
        subtitle = NULL # paste0(jurisdiction,' COVID-19 daily hospital admissions')
        );

    my.ggplot.alpha <- my.ggplot.alpha + geom_step(
        data    = DF.alpha.AIC,
        mapping = aes(x = alpha, y = index.subtree),
        alpha   = 0.9,
        size    = 1.3,
        colour  = "black"
        );

    my.ggplot.alpha <- my.ggplot.alpha + scale_y_continuous(
        limits = NULL,
        breaks = seq(0,100,1)
        );

    # PNG.output  <- paste0("plot-alpha.png");
    # ggsave(
    #     file   = PNG.output,
    #     plot   = my.ggplot.alpha,
    #     dpi    = 300,
    #     height =   5,
    #     width  =  10,
    #     units  = 'in'
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.AIC <- initializePlot(
        title    = NULL,
        subtitle = NULL # paste0(jurisdiction,' COVID-19 daily hospital admissions')
        );

    my.ggplot.AIC <- my.ggplot.AIC + geom_point(
        data    = DF.alpha.AIC,
        mapping = aes(x = AIC, y = index.subtree),
        alpha   = 0.9,
        size    = 2.0,
        colour  = "black"
        );

    my.ggplot.AIC <- my.ggplot.AIC + geom_line(
        data        = DF.alpha.AIC,
        mapping     = aes(x = AIC, y = index.subtree),
        orientation = "y",
        alpha       = 0.5,
        size        = 0.5,
        colour      = "black"
        );

    my.ggplot.AIC <- my.ggplot.AIC + scale_y_continuous(
        limits = NULL,
        breaks = seq(0,100,1)
        );

    # PNG.output  <- paste0("plot-AIC.png");
    # ggsave(
    #     file   = PNG.output,
    #     plot   = my.ggplot.AIC,
    #     dpi    = 300,
    #     height =   5,
    #     width  =  10,
    #     units  = 'in'
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        my.ggplot.alpha,
        my.ggplot.AIC,
        nrow       = 1,
        align      = "h",
        rel_widths = c(2,1)
        );

    PNG.output  <- paste0("plot-alpha-AIC.png");
    cowplot::ggsave2(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 300,
        height =   5,
        width  =  20,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART.subtree.hierarchy <- my.nppCART$get_subtree_hierarchy();
    # cat("\nstr(my.nppCART.subtree.hierarchy)\n");
    # print( str(my.nppCART.subtree.hierarchy)   );

    temp.alphas.nppCART <- as.numeric(sapply(
        X   = my.nppCART.subtree.hierarchy,
        FUN = function(x) { return(x[['alpha']]) }
        ));

    cat("\ntemp.alphas.nppCART\n");
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
    # DF.AICs <- data.frame(index.subtree = numeric(), AIC = numeric());
    # for ( index.subtree in seq(1,length(my.nppCART.subtree.hierarchy)) ) {
    #
    #     cat("\n")
    #     cat(paste0("\n### index.subtree: ",index.subtree,"\n"));
    #
    #     cat("\nnames(my.nppCART.subtree.hierarchy[[index.subtree]])\n");
    #     print( names(my.nppCART.subtree.hierarchy[[index.subtree]])   );
    #
    #     DF.retained <- my.nppCART.subtree.hierarchy[[index.subtree]][['DF_retained']];
    #     cat("\nDF.retained\n");
    #     print( DF.retained   );
    #
    #     # cat("\nmy.nppCART$nodes\n");
    #     # print( my.nppCART$nodes   );
    #
    #     DF.nprow.to.leafID <- my.nppCART$public_nprow_to_leafID(
    #         nodes = my.nppCART.subtree.hierarchy[[index.subtree]][['pruned_nodes']]
    #         );
    #     cat("\nstr(DF.nprow.to.leafID)\n");
    #     print( str(DF.nprow.to.leafID)   );
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
    #     my.AIC <- compute_AIC(
    #         DF.retained.nodes         = DF.retained,
    #         DF.npdata.with.propensity = DF.npdata.with.propensity,
    #         DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
    #         sampling.weight.varname   = "design.weight",
    #         replicate.weight.varnames = paste0("repweight",seq(1,500)),
    #         combined.weights          = FALSE # TRUE
    #         );
    #
    #     my.nppCART.AIC <- my.nppCART.subtree.hierarchy[[index.subtree]][['AIC']];
    #
    #     cat(paste0('\n# index.subtree = ',index.subtree,', my.AIC = ',my.AIC,", my.nppCART.AIC = ",my.nppCART.AIC,"\n"));
    #
    #     DF.AICs <- rbind(
    #         DF.AICs,
    #         data.frame(index.subtree = index.subtree, my.AIC = my.AIC, nppCART.AIC = my.nppCART.AIC)
    #         );
    #
    #     }
    #
    # cat("\nDF.AICs\n");
    # print( DF.AICs   );
    #
    # png("plot-my-AICs.png");
    # plot(x = DF.AICs[,'index.subtree'], y = DF.AICs[,'my.AIC']);
    # dev.off();
    #
    # png("plot-nppCART-AICs.png");
    # plot(x = DF.AICs[,'index.subtree'], y = DF.AICs[,'nppCART.AIC']);
    # dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }
