
test.nppCART.AIC_aggregate <- function(
    simulations.directory = NULL,
    n.simulations         = NULL,
    DF.population         = NULL,
    bin.width             = 3000,
    limits                = c(  0,1e6),
    breaks                = seq(0,1e6,1e5)
    ) {

    thisFunctionName <- "test.nppCART.AIC_aggregate";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(foreach);
    require(parallel);
    require(doParallel);
    require(ggplot2);
    require(RColorBrewer);
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    seed.directories <- list.files(path = simulations.directory, pattern = "seed-");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.simulations  <- length(seed.directories);
    DF.simulations <- data.frame(
        simulation.index         = rep(NA,times=n.simulations),
        seed.directory           = character(n.simulations),
        y51.estimate.current     = rep(NA,times=n.simulations),
        y51.estimate.fully.grown = rep(NA,times=n.simulations),
        y51.estimate.pruned      = rep(NA,times=n.simulations),
        y52.estimate.current     = rep(NA,times=n.simulations),
        y52.estimate.fully.grown = rep(NA,times=n.simulations),
        y52.estimate.pruned      = rep(NA,times=n.simulations)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( simulation.index in seq(1,length(seed.directories)) ) {
        seed.directory <- seed.directories[simulation.index];
        DF.temp <- test.nppCART.AIC_aggregate_inner(
            seed.directory = file.path(normalizePath(simulations.directory),seed.directory)
            );
        if ( !is.null(DF.temp) ) {
            DF.simulations[simulation.index,'simulation.index']         <- simulation.index;
            DF.simulations[simulation.index,'seed.directory']           <- seed.directory;
            DF.simulations[simulation.index,'y51.estimate.current']     <- DF.temp[1,'y51.estimate.current'];
            DF.simulations[simulation.index,'y51.estimate.fully.grown'] <- DF.temp[1,'y51.estimate.fully.grown'];
            DF.simulations[simulation.index,'y51.estimate.pruned']      <- DF.temp[1,'y51.estimate.pruned'];
            DF.simulations[simulation.index,'y52.estimate.current']     <- DF.temp[1,'y52.estimate.current'];
            DF.simulations[simulation.index,'y52.estimate.fully.grown'] <- DF.temp[1,'y52.estimate.fully.grown'];
            DF.simulations[simulation.index,'y52.estimate.pruned']      <- DF.temp[1,'y52.estimate.pruned'];
            }
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.simulations <- DF.simulations[!is.na(DF.simulations[,'simulation.index']),];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        file      = "DF-simulations.csv",
        x         = DF.simulations,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    target.variable <- 'y51';
    test.nppCART.AIC_aggregate_histograms(
        DF.simulations   = DF.simulations,
        target.variable  = target.variable,
        vline.xintercept = sum(DF.population[,target.variable]),
        bin.width        = bin.width,
        limits           = limits,
        breaks           = breaks,
        PNG.output       = paste0("plot-simulation-histograms-",target.variable,".png")
        );

    target.variable <- 'y52';
    test.nppCART.AIC_aggregate_histograms(
        DF.simulations   = DF.simulations,
        target.variable  = target.variable,
        vline.xintercept = sum(DF.population[,target.variable]),
        bin.width        = bin.width,
        limits           = limits,
        breaks           = breaks,
        PNG.output       = paste0("plot-simulation-histograms-",target.variable,".png")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

####################
test.nppCART.AIC_aggregate_inner <- function(
    seed.directory = NULL
    ) {

    original.directory <- normalizePath(getwd());
    setwd(seed.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !file.exists("DF-current-npdata-with-propensity.RData") ) {
        setwd(original.directory);
        return(NULL);
        }

    if ( !file.exists("DF-npdata-with-propensity.RData") ) {
        setwd(original.directory);
        return(NULL);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.current <- readRDS(
        file = "DF-current-npdata-with-propensity.RData"
        );

    DF.npdata.with.propensity <- readRDS(
        file = "DF-npdata-with-propensity.RData"
        );

    DF.temp <- data.frame(

        y51.estimate.current     = sum(               DF.current[,'y51'] /                DF.current[,'propensity'       ]),
        y51.estimate.fully.grown = sum(DF.npdata.with.propensity[,'y51'] / DF.npdata.with.propensity[,'propensity'       ]),
        y51.estimate.pruned      = sum(DF.npdata.with.propensity[,'y51'] / DF.npdata.with.propensity[,'propensity.pruned']),

        y52.estimate.current     = sum(               DF.current[,'y52'] /                DF.current[,'propensity'       ]),
        y52.estimate.fully.grown = sum(DF.npdata.with.propensity[,'y52'] / DF.npdata.with.propensity[,'propensity'       ]),
        y52.estimate.pruned      = sum(DF.npdata.with.propensity[,'y52'] / DF.npdata.with.propensity[,'propensity.pruned'])

        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd(original.directory);
    return( DF.temp );

    }

test.nppCART.AIC_aggregate_histograms <- function(
    DF.simulations   = NULL,
    target.variable  = NULL,
    vline.xintercept = NULL,
    bin.width        = 6000,
    limits           = c(  0,1e6),
    breaks           = seq(0,1e6,1e5),
    PNG.output       = paste0("plot-histograms.png")
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    plot.variables <- c("estimate.current","estimate.fully.grown","estimate.pruned");
    plot.variables <- paste0(target.variable,".",plot.variables);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.plots <- list();
    for ( plot.variable in plot.variables) {

        my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
        my.ggplot <- my.ggplot + geom_vline(
            xintercept = vline.xintercept,
            colour     = "orange",
            size       = 1.00
            );
        colnames(DF.simulations) <- gsub(
            x           = colnames(DF.simulations),
            pattern     = paste0("^",plot.variable,"$"),
            replacement = "plot.variable"
            );
        my.ggplot <- my.ggplot + geom_histogram(
            data     = DF.simulations,
            mapping  = aes(x = plot.variable),
            binwidth = bin.width,
            alpha    = 0.5
            # fill     = "black",
            # colour   = NULL
            );
        colnames(DF.simulations) <- gsub(
            x           = colnames(DF.simulations),
            pattern     = "plot.variable",
            replacement = plot.variable
            );
        my.ggplot <- my.ggplot + xlab(plot.variable);
        my.ggplot <- my.ggplot + scale_x_continuous(
            limits = limits,
            breaks = breaks
            );

        MCRelBias <- NA;
        MCRelBias <- (DF.simulations[,plot.variable] - vline.xintercept) / vline.xintercept;
        MCRelBias <- mean( MCRelBias, na.rm = TRUE );
        MCRelBias <- round(MCRelBias,3);

        MCRelRMSE <- NA;
        temp.vect <- DF.simulations[!is.na(DF.simulations[,plot.variable]),plot.variable];
        MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
        MCRelRMSE <- sqrt(mean( MCRelRMSE ));
        MCRelRMSE <- round(MCRelRMSE,3);

        temp.xmax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['x']]$get_limits());
        temp.ymax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['y']]$get_limits());

        temp.min  <- min(DF.simulations[,plot.variable], na.rm = TRUE);
        temp.min  <- format(temp.min, digits = 3, scientific = TRUE);

        temp.max  <- max(DF.simulations[,plot.variable], na.rm = TRUE);
        temp.max  <- format(temp.max, digits = 3, scientific = TRUE);

        temp.iter <- nrow( DF.simulations );
        temp.NA   <- sum(is.na( DF.simulations[,plot.variable] ));

        my.ggplot <- my.ggplot + annotate(
            geom  = "text",
            label = c(
                paste0("MC Rel.BIAS = ",MCRelBias),
                paste0("MC Rel.RMSE = ",MCRelRMSE),
                paste0("min(Ty_hat) = ",temp.min ),
                paste0("max(Ty_hat) = ",temp.max ),
                paste0("   #(iters) = ",temp.iter),
                paste0("      #(NA) = ",temp.NA  )
                ),
            x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
            y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
            size  = 7.5,
            color = "black"
            );

        list.plots[[ plot.variable ]] <- my.ggplot;

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # plot.variable <- paste0(target.variable,".estimate.current");
    #
    # my.histogram.current <- initializePlot(title = NULL, subtitle = NULL);
    # my.histogram.current <- my.histogram.current + geom_vline(
    #     xintercept = vline.xintercept,
    #     colour     = "orange",
    #     size       = 1.00
    #     );
    # colnames(DF.simulations) <- gsub(
    #     x           = colnames(DF.simulations),
    #     pattern     = plot.variable,
    #     replacement = "plot.variable"
    #     );
    # my.histogram.current <- my.histogram.current + geom_histogram(
    #     data     = DF.simulations,
    #     mapping  = aes(x = plot.variable),
    #     binwidth = bin.width,
    #     alpha    = 0.5
    #     # fill     = "black",
    #     # colour   = NULL
    #     );
    # my.histogram.current <- my.histogram.current + xlab(plot.variable);
    # my.histogram.current <- my.histogram.current + scale_x_continuous(
    #     limits = limits,
    #     breaks = breaks
    #     );
    #
    # MCRelBias <- NA;
    # MCRelBias <- (DF.simulations[,plot.variable] - vline.xintercept) / vline.xintercept;
    # MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    # MCRelBias <- round(MCRelBias,3);
    #
    # MCRelRMSE <- NA;
    # temp.vect <- DF.simulations[!is.na(DF.simulations[,plot.variable]),plot.variable];
    # MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    # MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    # MCRelRMSE <- round(MCRelRMSE,3);
    #
    # temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits());
    # temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits());
    #
    # temp.min  <- min(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.min  <- format(temp.min, digits = 3, scientific = TRUE);
    #
    # temp.max  <- max(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.max  <- format(temp.max, digits = 3, scientific = TRUE);
    #
    # temp.iter <- nrow( DF.simulations );
    # temp.NA   <- sum(is.na( DF.simulations[,plot.variable] ));
    #
    # my.histogram.current <- my.histogram.current + annotate(
    #     geom  = "text",
    #     label = c(
    #         paste0("MC Rel.BIAS = ",MCRelBias),
    #         paste0("MC Rel.RMSE = ",MCRelRMSE),
    #         paste0("min(Ty_hat) = ",temp.min ),
    #         paste0("max(Ty_hat) = ",temp.max ),
    #         paste0("   #(iters) = ",temp.iter),
    #         paste0("      #(NA) = ",temp.NA  )
    #         ),
    #     # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
    #     x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
    #     y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
    #     size  = 7.5,
    #     color = "black"
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # plot.variable <- paste0(target.variable,".estimate.fully.grown");
    #
    # my.histogram.fully.grown <- initializePlot(title = NULL, subtitle = NULL);
    # my.histogram.fully.grown <- my.histogram.fully.grown + geom_vline(
    #     xintercept = vline.xintercept,
    #     colour     = "orange",
    #     size       = 1.00
    #     );
    # my.histogram.fully.grown <- my.histogram.fully.grown + geom_histogram(
    #     data     = DF.simulations,
    #     mapping  = aes(x = estimate.fully.grown),
    #     binwidth = bin.width,
    #     alpha    = 0.5
    #     # fill     = "black",
    #     # colour   = NULL
    #     );
    # my.histogram.fully.grown <- my.histogram.fully.grown + xlab(plot.variable);
    # my.histogram.fully.grown <- my.histogram.fully.grown + scale_x_continuous(
    #     limits = limits,
    #     breaks = breaks
    #     );
    #
    # MCRelBias <- NA;
    # MCRelBias <- (DF.simulations[,plot.variable] - vline.xintercept) / vline.xintercept;
    # MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    # MCRelBias <- round(MCRelBias,3);
    #
    # MCRelRMSE <- NA;
    # temp.vect <- DF.simulations[!is.na(DF.simulations[,plot.variable]),plot.variable];
    # MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    # MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    # MCRelRMSE <- round(MCRelRMSE,3);
    #
    # temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits())
    # temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits())
    #
    # temp.min  <- min(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.min  <- format(temp.min, digits = 3, scientific = TRUE);
    #
    # temp.max  <- max(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.max  <- format(temp.max, digits = 3, scientific = TRUE);
    #
    # temp.iter <- nrow( DF.simulations );
    # temp.NA   <- sum(is.na( DF.simulations[,plot.variable] ));
    #
    # my.histogram.fully.grown <- my.histogram.fully.grown + annotate(
    #     geom  = "text",
    #     label = c(
    #         paste0("MC Rel.BIAS = ",MCRelBias),
    #         paste0("MC Rel.RMSE = ",MCRelRMSE),
    #         paste0("min(Ty_hat) = ",temp.min ),
    #         paste0("max(Ty_hat) = ",temp.max ),
    #         paste0("   #(iters) = ",temp.iter),
    #         paste0("      #(NA) = ",temp.NA  )
    #         ),
    #     # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
    #     x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
    #     y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
    #     size  = 7.5,
    #     color = "black"
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # plot.variable <- paste0(target.variable,".estimate.pruned");
    #
    # my.histogram.pruned <- initializePlot(title = NULL, subtitle = NULL);
    # my.histogram.pruned <- my.histogram.pruned + geom_vline(
    #     xintercept = vline.xintercept,
    #     colour     = "orange",
    #     size       = 1.00
    #     );
    # my.histogram.pruned <- my.histogram.pruned + geom_histogram(
    #     data     = DF.simulations,
    #     mapping  = aes(x = estimate.pruned),
    #     binwidth = bin.width,
    #     alpha    = 0.5
    #     # fill     = "black",
    #     # colour   = NULL
    #     );
    # my.histogram.pruned <- my.histogram.pruned + xlab(plot.variable);
    # my.histogram.pruned <- my.histogram.pruned + scale_x_continuous(
    #     limits = limits,
    #     breaks = breaks
    #     );
    #
    # MCRelBias <- NA;
    # MCRelBias <- (DF.simulations[,plot.variable] - vline.xintercept) / vline.xintercept;
    # MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    # MCRelBias <- round(MCRelBias,3);
    #
    # MCRelRMSE <- NA;
    # temp.vect <- DF.simulations[!is.na(DF.simulations[,plot.variable]),plot.variable];
    # MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    # MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    # MCRelRMSE <- round(MCRelRMSE,3);
    #
    # temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits())
    # temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits())
    #
    # temp.min  <- min(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.min  <- format(temp.min, digits = 3, scientific = TRUE);
    #
    # temp.max  <- max(DF.simulations[,plot.variable], na.rm = TRUE);
    # temp.max  <- format(temp.max, digits = 3, scientific = TRUE);
    #
    # temp.iter <- nrow( DF.simulations );
    # temp.NA   <- sum(is.na( DF.simulations[,plot.variable] ));
    #
    # my.histogram.pruned <- my.histogram.pruned + annotate(
    #     geom  = "text",
    #     label = c(
    #         paste0("MC Rel.BIAS = ",MCRelBias),
    #         paste0("MC Rel.RMSE = ",MCRelRMSE),
    #         paste0("min(Ty_hat) = ",temp.min ),
    #         paste0("max(Ty_hat) = ",temp.max ),
    #         paste0("   #(iters) = ",temp.iter),
    #         paste0("      #(NA) = ",temp.NA  )
    #         ),
    #     # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
    #     x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
    #     y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
    #     size  = 7.5,
    #     color = "black"
    #     );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        list.plots[[ paste0(target.variable,".estimate.current"    ) ]], # my.histogram.current,
        list.plots[[ paste0(target.variable,".estimate.fully.grown") ]], # my.histogram.fully.grown,
        list.plots[[ paste0(target.variable,".estimate.pruned"     ) ]], # my.histogram.pruned,
        nrow       = 1,
        align      = "h",
        rel_widths = c(1,1)
        );

    cowplot::ggsave2(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 300,
        height =   6,
        width  =  20,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    }
