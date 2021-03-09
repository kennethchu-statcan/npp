
test.nppCART.AIC <- function(
    seed           = 1234567,
    prob.selection = 0.1,
    n.replicates   = 500,
    n.simulations  = 10
    ) {

    thisFunctionName <- "test.nppCART.AIC";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(seed = seed);

    DF.simulations <- test.nppCART.AIC_do.simulations(
        seed           = seed,
        DF.population  = DF.population,
        prob.selection = prob.selection,
        n.replicates   = n.replicates,
        n.simulations  = n.simulations
        );

    test.nppCART.AIC_plot.simulations(
        DF.simulations   = DF.simulations,
        vline.xintercept = sum(DF.population[,'y'])
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

####################
test.nppCART.AIC_plot.simulations <- function(
    DF.simulations   = NULL,
    vline.xintercept = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.histogram.fully.grown <- initializePlot(title = NULL, subtitle = NULL);
    my.histogram.fully.grown <- my.histogram.fully.grown + geom_vline(
        xintercept = vline.xintercept,
        colour     = "orange",
        size       = 1.00
        );
    my.histogram.fully.grown <- my.histogram.fully.grown + geom_histogram(
        data     = DF.simulations,
        mapping  = aes(x = estimate.fully.grown),
        alpha    = 0.5
        # binwidth = 2000,
        # fill     = "black",
        # colour   = NULL
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.histogram.pruned <- initializePlot(title = NULL, subtitle = NULL);
    my.histogram.pruned <- my.histogram.pruned + geom_vline(
        xintercept = vline.xintercept,
        colour     = "orange",
        size       = 1.00
        );
    my.histogram.pruned <- my.histogram.pruned + geom_histogram(
        data     = DF.simulations,
        mapping  = aes(x = estimate.pruned),
        alpha    = 0.5
        # binwidth = 2000,
        # fill     = "black",
        # colour   = NULL
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        my.histogram.fully.grown,
        my.histogram.pruned,
        nrow       = 1,
        align      = "h",
        rel_widths = c(1,1)
        );

    PNG.output  <- paste0("plot-histograms.png");
    cowplot::ggsave2(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 300,
        height =  10,
        width  =  20,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    }

test.nppCART.AIC_do.simulations <- function(
    seed           = NULL,
    DF.population  = NULL,
    prob.selection = NULL,
    n.replicates   = NULL,
    n.simulations  = NULL,
    CSV.output     = "DF-simulations.csv"
    ) {

    thisFunctionName <- "test.nppCART.AIC_do.simulations";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( file.exists(CSV.output) ) {
        DF.output <- read.csv(file = CSV.output);
        return(DF.output);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- data.frame(
        index.simulation     = seq(1,n.simulations),
        estimate.fully.grown = rep(NA,times=n.simulations),
        estimate.pruned      = rep(NA,times=n.simulations)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( index.simulation in seq(1,n.simulations) ) {

        cat(paste0("\n### index.simulation: ",index.simulation,"\n"));

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        list.samples  <- test.nppCART_get.samples(
            DF.population         = DF.population,
            prob.selection        = prob.selection, # 0.1, 1 - 1e-8, # 1.0,
            n.replicates          = n.replicates
            # RData.non.probability = "DF-non-probability.RData",
            # RData.probability     = "DF-probability.RData"
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
            min.impurity      = 1e-50,
            max.levels        = 10000
            );

        my.nppCART$grow();
        cat("\nmy.nppCART$print()\n");
        my.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );

        DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
        cat("\nstr(DF.npdata.with.propensity)\n");
        print( str(DF.npdata.with.propensity)   );

        DF.impurity.alpha.AIC <- my.nppCART$get_impurities_alphas_AICs();
        cat("\nDF.impurity.alpha.AIC\n");
        print( DF.impurity.alpha.AIC   );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.nppCART.subtree.hierarchy <- my.nppCART$get_subtree_hierarchy();

        for ( index.temp in seq(1,length(my.nppCART.subtree.hierarchy)) ) {

            DF.retained <- my.nppCART.subtree.hierarchy[[index.temp]][['DF_retained']];
            DF.temp <- DF.retained[is.na(DF.retained[,'satisfiedChildID']),];

            cat("\nsum(DF.temp[,'p.weight'])\n");
            print( sum(DF.temp[,'p.weight'])   );

            cat("\nDF.retained\n");
            print( DF.retained   );

            }

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.output[index.simulation,'estimate.fully.grown'] <- sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity'       ]);
        DF.output[index.simulation,'estimate.pruned']      <- sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity.pruned']);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot.alpha <- initializePlot(title = NULL, subtitle = NULL);

        my.ggplot.alpha <- my.ggplot.alpha + geom_step(
            data    = DF.impurity.alpha.AIC,
            mapping = aes(x = alpha, y = index.subtree),
            alpha   = 0.9,
            size    = 1.3,
            colour  = "black"
            );

        my.ggplot.alpha <- my.ggplot.alpha + scale_y_continuous(
            limits = NULL,
            breaks = seq(0,100,1)
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot.impurity <- initializePlot(title = NULL, subtitle = NULL);

        my.ggplot.impurity <- my.ggplot.impurity + geom_point(
            data    = DF.impurity.alpha.AIC,
            mapping = aes(x = tree.impurity, y = index.subtree),
            alpha   = 0.9,
            size    = 2.0,
            colour  = "black"
            );

        my.ggplot.impurity <- my.ggplot.impurity + geom_line(
            data        = DF.impurity.alpha.AIC,
            mapping     = aes(x = tree.impurity, y = index.subtree),
            orientation = "y",
            alpha       = 0.5,
            size        = 0.5,
            colour      = "black"
            );

        my.ggplot.impurity <- my.ggplot.impurity + scale_y_continuous(
            limits = NULL,
            breaks = seq(0,100,1)
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot.AIC <- initializePlot(title = NULL, subtitle = NULL);

        my.ggplot.AIC <- my.ggplot.AIC + geom_point(
            data    = DF.impurity.alpha.AIC,
            mapping = aes(x = AIC, y = index.subtree),
            alpha   = 0.9,
            size    = 2.0,
            colour  = "black"
            );

        my.ggplot.AIC <- my.ggplot.AIC + geom_line(
            data        = DF.impurity.alpha.AIC,
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

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.cowplot <- cowplot::plot_grid(
            my.ggplot.impurity,
            my.ggplot.alpha,
            my.ggplot.AIC,
            nrow       = 1,
            align      = "h",
            rel_widths = c(2,3,2)
            );

        index.simulation.string <- stringr::str_pad(
            string = as.character(index.simulation),
            width  = nchar(as.character(n.simulations)),
            side   = "left",
            pad    = "0"
            );

        PNG.output  <- paste0("plot-impurity-alpha-AIC-",index.simulation.string,".png");
        cowplot::ggsave2(
            file   = PNG.output,
            plot   = my.cowplot,
            dpi    = 300,
            height =   5,
            width  =  20,
            units  = 'in'
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        file      = CSV.output,
        x         = DF.output,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( DF.output );

    }
