
test.nppCART.AIC_graphics <- function(
    simulations.directory = NULL,
    DF.population         = NULL
    ) {

    thisFunctionName <- "test.nppCART.AIC_graphics";

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
    for ( seed.directory in seed.directories ) {
        test.nppCART.AIC_graphics_inner(
            seed.directory = file.path(normalizePath(simulations.directory),seed.directory),
            DF.population  = DF.population
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # n.cores <- parallel::detectCores();
    # cat("\nn.cores\n");
    # print( n.cores   );
    #
    # doParallel::registerDoParallel(n.cores);
    # seed.directories <- list.files(path = simulations.directory, pattern = "seed-");
    # foreach ( seed.directory.index = seq(1,length(seed.directories)) ) %dopar% {
    #     seed.directory <- seed.directories[seed.directory.index];
    #     test.nppCART.AIC_graphics_inner(
    #         seed.directory = file.path(normalizePath(simulations.directory),seed.directory),
    #         DF.population  = DF.population
    #         );
    #     }
    # stopImplicitCluster();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

####################
test.nppCART.AIC_graphics_inner <- function(
    seed.directory = NULL,
    DF.population  = NULL
    ) {

    original.directory <- normalizePath(getwd());
    setwd(seed.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.npdata.with.propensity <- readRDS(
        file = "DF-npdata-with-propensity.RData"
        );

    DF.impurity.alpha.AIC <- readRDS(
        file = "DF-impurity-alpha-AIC.RData"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.fully.grown <- test.nppCART.AIC_do.one.simulation_hex(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        propensity.variable = "propensity"
        );

    PNG.output <- paste0("plot-propensity-hex-fully-grown.png");
    ggsave(
        filename = PNG.output,
        plot     = my.ggplot.fully.grown,
        dpi      = 300,
        height   =  11,
        width    =  10,
        units    = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.pruned <- test.nppCART.AIC_do.one.simulation_hex(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        propensity.variable = "propensity.pruned"
        );

    PNG.output <- paste0("plot-propensity-hex-pruned.png");
    ggsave(
        filename = PNG.output,
        plot     = my.ggplot.pruned,
        dpi      = 300,
        height   =  11,
        width    =  10,
        units    = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.impurity.alpha.AIC <- test.nppCART.AIC_do.one.simulation_plot.impurity.alpha.AIC(
        DF.input = DF.impurity.alpha.AIC
        );

    PNG.output <- paste0("plot-impurity-alpha-AIC.png");
    ggsave(
        filename = PNG.output,
        plot     = my.ggplot.impurity.alpha.AIC,
        dpi      = 300,
        height   =   5,
        width    =  20,
        units    = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    levels.x3.hidden <- unique(as.character(DF.population[,'x3.hidden']));
    for ( temp.level.x3.hidden in levels.x3.hidden ) {

        DF.temp <- DF.npdata.with.propensity[DF.npdata.with.propensity[,'x3.hidden'] == temp.level.x3.hidden,];

        my.ggplot.fully.grown <- test.nppCART.AIC_do.one.simulation_scatter(
            DF.input            = DF.temp, # DF.npdata.with.propensity,
            DF.population       = DF.population,
            subtitle            = paste0("Simulation (x3.hidden = ",temp.level.x3.hidden,")"),
            propensity.variable = "propensity"
            );

        PNG.output <- paste0("plot-propensity-scatter-fully-grown-x3-",temp.level.x3.hidden,".png");
        ggsave(
            filename = PNG.output,
            plot     = my.ggplot.fully.grown,
            dpi      = 300,
            height   =  11,
            width    =  10,
            units    = 'in'
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        my.ggplot.pruned <- test.nppCART.AIC_do.one.simulation_scatter(
            DF.input            = DF.temp, # DF.npdata.with.propensity,
            DF.population       = DF.population,
            subtitle            = paste0("Simulation (x3.hidden = ",temp.level.x3.hidden,")"),
            propensity.variable = "propensity.pruned"
            );

        PNG.output <- paste0("plot-propensity-scatter-pruned-x3-",temp.level.x3.hidden,".png");
        ggsave(
            filename = PNG.output,
            plot     = my.ggplot.pruned,
            dpi      = 300,
            height   =  11,
            width    =  10,
            units    = 'in'
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd(original.directory);
    return(NULL);

    }
