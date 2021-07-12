
test.nppCART.AIC_aggregate <- function(
    simulations.directory = NULL,
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
        simulation.index     = seq(1,n.simulations),
        seed.directory       = character(n.simulations),
        estimate.current     = rep(NA,times=n.simulations),
        estimate.fully.grown = rep(NA,times=n.simulations),
        estimate.pruned      = rep(NA,times=n.simulations)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( simulation.index in seq(1,length(seed.directories)) ) {
        seed.directory <- seed.directories[simulation.index];
        DF.temp <- test.nppCART.AIC_aggregate_inner(
            seed.directory = file.path(normalizePath(simulations.directory),seed.directory)
            );
        DF.simulations[simulation.index,'simulation.index']     <- simulation.index;
        DF.simulations[simulation.index,'seed.directory']       <- seed.directory;
        DF.simulations[simulation.index,'estimate.current']     <- DF.temp[1,'estimate.current'];
        DF.simulations[simulation.index,'estimate.fully.grown'] <- DF.temp[1,'estimate.fully.grown'];
        DF.simulations[simulation.index,'estimate.pruned']      <- DF.temp[1,'estimate.pruned'];
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        file      = "DF-simulations.csv",
        x         = DF.simulations,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    test.nppCART.AIC_aggregate_histograms(
        DF.simulations   = DF.simulations,
        vline.xintercept = sum(DF.population[,'y']),
        bin.width        = bin.width,
        limits           = limits,
        breaks           = breaks,
        PNG.output       = paste0("plot-simulation-histograms.png")
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
    DF.current <- readRDS(
        file = "DF-current-npdata-with-propensity.RData"
        );

    DF.npdata.with.propensity <- readRDS(
        file = "DF-npdata-with-propensity.RData"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.temp <- data.frame(
        estimate.current     = sum(               DF.current[,'y'] /                DF.current[,'propensity'       ]),
        estimate.fully.grown = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity'       ]),
        estimate.pruned      = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity.pruned'])
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd(original.directory);
    return( DF.temp );

    }

test.nppCART.AIC_aggregate_histograms <- function(
    DF.simulations   = NULL,
    vline.xintercept = NULL,
    bin.width        = 6000,
    limits           = c(  0,1e6),
    breaks           = seq(0,1e6,1e5)
    PNG.output       = paste0("plot-histograms.png")
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.histogram.current <- initializePlot(title = NULL, subtitle = NULL);
    my.histogram.current <- my.histogram.current + geom_vline(
        xintercept = vline.xintercept,
        colour     = "orange",
        size       = 1.00
        );
    my.histogram.current <- my.histogram.current + geom_histogram(
        data     = DF.simulations,
        mapping  = aes(x = estimate.current),
        binwidth = bin.width,
        alpha    = 0.5
        # fill     = "black",
        # colour   = NULL
        );
    my.histogram.current <- my.histogram.current + scale_x_continuous(
        limits = limits,
        breaks = breaks
        );

    target.variable <- "estimate.current";

    MCRelBias <- NA;
    MCRelBias <- (DF.simulations[,target.variable] - vline.xintercept) / vline.xintercept;
    MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    MCRelBias <- round(MCRelBias,3);

    MCRelRMSE <- NA;
    temp.vect <- DF.simulations[!is.na(DF.simulations[,target.variable]),target.variable];
    MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    MCRelRMSE <- round(MCRelRMSE,3);

    temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits())
    temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits())

    temp.min  <- min(DF.simulations[,target.variable], na.rm = TRUE);
    temp.min  <- format(temp.min, digits = 3, scientific = TRUE);

    temp.max  <- max(DF.simulations[,target.variable], na.rm = TRUE);
    temp.max  <- format(temp.max, digits = 3, scientific = TRUE);

    temp.iter <- nrow( DF.simulations );
    temp.NA   <- sum(is.na( DF.simulations[,target.variable] ));

    my.histogram.current <- my.histogram.current + annotate(
        geom  = "text",
        label = c(
            paste0("MC Rel.BIAS = ",MCRelBias),
            paste0("MC Rel.RMSE = ",MCRelRMSE),
            paste0("min(Ty_hat) = ",temp.min ),
            paste0("max(Ty_hat) = ",temp.max ),
            paste0("   #(iters) = ",temp.iter),
            paste0("      #(NA) = ",temp.NA  )
            ),
        # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
        x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
        y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
        size  = 7.5,
        color = "black"
        );

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
        binwidth = bin.width,
        alpha    = 0.5
        # fill     = "black",
        # colour   = NULL
        );
    my.histogram.fully.grown <- my.histogram.fully.grown + scale_x_continuous(
        limits = limits,
        breaks = breaks
        );

    target.variable <- "estimate.fully.grown";

    MCRelBias <- NA;
    MCRelBias <- (DF.simulations[,target.variable] - vline.xintercept) / vline.xintercept;
    MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    MCRelBias <- round(MCRelBias,3);

    MCRelRMSE <- NA;
    temp.vect <- DF.simulations[!is.na(DF.simulations[,target.variable]),target.variable];
    MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    MCRelRMSE <- round(MCRelRMSE,3);

    temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits())
    temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits())

    temp.min  <- min(DF.simulations[,target.variable], na.rm = TRUE);
    temp.min  <- format(temp.min, digits = 3, scientific = TRUE);

    temp.max  <- max(DF.simulations[,target.variable], na.rm = TRUE);
    temp.max  <- format(temp.max, digits = 3, scientific = TRUE);

    temp.iter <- nrow( DF.simulations );
    temp.NA   <- sum(is.na( DF.simulations[,target.variable] ));

    my.histogram.fully.grown <- my.histogram.fully.grown + annotate(
        geom  = "text",
        label = c(
            paste0("MC Rel.BIAS = ",MCRelBias),
            paste0("MC Rel.RMSE = ",MCRelRMSE),
            paste0("min(Ty_hat) = ",temp.min ),
            paste0("max(Ty_hat) = ",temp.max ),
            paste0("   #(iters) = ",temp.iter),
            paste0("      #(NA) = ",temp.NA  )
            ),
        # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
        x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
        y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
        size  = 7.5,
        color = "black"
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
        binwidth = bin.width,
        alpha    = 0.5
        # fill     = "black",
        # colour   = NULL
        );
    my.histogram.pruned <- my.histogram.pruned + scale_x_continuous(
        limits = limits,
        breaks = breaks
        );

    target.variable <- "estimate.pruned";

    MCRelBias <- NA;
    MCRelBias <- (DF.simulations[,target.variable] - vline.xintercept) / vline.xintercept;
    MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    MCRelBias <- round(MCRelBias,3);

    MCRelRMSE <- NA;
    temp.vect <- DF.simulations[!is.na(DF.simulations[,target.variable]),target.variable];
    MCRelRMSE <- ((temp.vect - vline.xintercept)^2) / (vline.xintercept^2) ;
    MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    MCRelRMSE <- round(MCRelRMSE,3);

    temp.xmax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['x']]$get_limits())
    temp.ymax <- max(layer_scales(my.histogram.current,i=1L,j=1L)[['y']]$get_limits())

    temp.min  <- min(DF.simulations[,target.variable], na.rm = TRUE);
    temp.min  <- format(temp.min, digits = 3, scientific = TRUE);

    temp.max  <- max(DF.simulations[,target.variable], na.rm = TRUE);
    temp.max  <- format(temp.max, digits = 3, scientific = TRUE);

    temp.iter <- nrow( DF.simulations );
    temp.NA   <- sum(is.na( DF.simulations[,target.variable] ));

    my.histogram.pruned <- my.histogram.pruned + annotate(
        geom  = "text",
        label = c(
            paste0("MC Rel.BIAS = ",MCRelBias),
            paste0("MC Rel.RMSE = ",MCRelRMSE),
            paste0("min(Ty_hat) = ",temp.min ),
            paste0("max(Ty_hat) = ",temp.max ),
            paste0("   #(iters) = ",temp.iter),
            paste0("      #(NA) = ",temp.NA  )
            ),
        # x   = temp.xmax * 0.8 * c(1,1,1,1,1,1),
        x     = temp.xmax * 0.3 * c(1,1,1,1,1,1),
        y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
        size  = 7.5,
        color = "black"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        my.histogram.current,
        my.histogram.fully.grown,
        my.histogram.pruned,
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
