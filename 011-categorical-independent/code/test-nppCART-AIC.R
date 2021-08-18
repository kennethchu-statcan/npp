
test.nppCART.AIC_do.one.simulation <- function(
    seed           = NULL,
    DF.population  = NULL,
    prob.selection = NULL,
    n.replicates   = NULL,
    save.nppCART   = FALSE
    ) {

    thisFunctionName <- "test.nppCART.AIC_do.one.simulation";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.directory <- normalizePath(getwd());
    temp.directory     <- file.path(original.directory,paste0('seed-',seed));
    Sys.sleep(time = 5); if ( !dir.exists(temp.directory) ) { dir.create(temp.directory); }
    Sys.sleep(time = 5); setwd(temp.directory);
    Sys.sleep(time = 5);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    file.output  <- file(description = "sink-output.txt",  open = "wt");
    file.message <- file(description = "sink-message.txt", open = "wt");

    sink(file = file.output,  type = "output" );
    sink(file = file.message, type = "message");

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(ggplot2);
    require(R6);
    require(RColorBrewer);
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
    start.proc.time <- proc.time();

    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.samples <- test.nppCART_get.samples(
        DF.population  = DF.population,
        prob.selection = prob.selection, # 0.1, 1 - 1e-8, # 1.0,
        n.replicates   = n.replicates
        );

    saveRDS(
        file   = "DF-non-probability.RData",
        object = list.samples[['DF.non.probability']]
        );

    saveRDS(
        file   = "DF-probability.RData",
        object = list.samples[['DF.probability']]
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    cat("\nstr(list.samples[['DF.non.probability']])\n");
    print( str(list.samples[['DF.non.probability']])   );

    # DF.probability <- list.samples[['DF.probability']];
    cat("\nstr(list.samples[['DF.probability']])\n");
    print( str(list.samples[['DF.probability']])   )

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    RData.my.nppCART <- "nppCART-my.RData";
    if ( file.exists(RData.my.nppCART) ) {

        my.nppCART <- readRDS(file = RData.my.nppCART);

    } else {

        my.nppCART <- nppCART(
            np.data                   = list.samples[['DF.non.probability']],
            p.data                    = list.samples[['DF.probability']],
            predictors                = c("x1","x2","x3"),
            sampling.weight           = "design.weight",
            bootstrap.weights         = paste0("repweight",seq(1,n.replicates)),
            min.cell.size             = 1,
            min.impurity              = 1e-50,
            n.levels.approx.threshold = 4
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

        saveRDS(
            file   = "DF-npdata-with-propensity.RData",
            object = DF.npdata.with.propensity
            );

        saveRDS(
            file   = "DF-impurity-alpha-AIC.RData",
            object = DF.impurity.alpha.AIC
            );

        if ( save.nppCART ) {
            saveRDS(
                file   = RData.my.nppCART,
                object = my.nppCART
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    RData.current.nppCART <- "nppCART-current.RData";
    if ( file.exists(RData.current.nppCART) ) {

        current.nppCART <- readRDS(file = RData.current.nppCART);

    } else {

        current.nppCART <- nppR::nppCART(
            np.data       = list.samples[['DF.non.probability']],
            p.data        = list.samples[['DF.probability']],
            predictors    = c("x1","x2","x3"),
            weight        = "design.weight",
            min.cell.size = 1,
            min.impurity  = 1e-50,
            max.levels    = 10000
            );

        current.nppCART$grow();
        cat("\ncurrent.nppCART$print()\n");
        current.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );

        DF.current <- current.nppCART$get_npdata_with_propensity();
        cat("\nstr(DF.current)\n");
        print( str(DF.current)   );

        saveRDS(
            file   = "DF-current-npdata-with-propensity.RData",
            object = DF.current
            );

        if ( save.nppCART ) {
            saveRDS(
                file   = RData.current.nppCART,
                object = current.nppCART
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    # print warning messages to log
    cat("\n##### warnings()\n")
    print(warnings());

    # print session info to log
    cat("\n##### sessionInfo()\n")
    print( sessionInfo() );

    # print system time to log
    cat(paste0("\n##### Sys.time(): ",Sys.time(),"\n"));

    # print elapsed time to log
    stop.proc.time <- proc.time();
    cat("\n##### start.proc.time() - stop.proc.time()\n");
    print( stop.proc.time - start.proc.time );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");

    sink(file = NULL, type = "output" );
    sink(file = NULL, type = "message");
    sink();

    Sys.sleep(time = 5);
    setwd(original.directory);
    Sys.sleep(time = 5);

    return( NULL );

    }

####################
test.nppCART.AIC_do.one.simulation_plot.impurity.alpha.AIC <- function(
    DF.input = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.breaks     <- seq(0,200,20);
    my.size.line  <- 0.5;
    my.size.point <- 1.3;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    index.min.subtree <- DF.input[which( DF.input[,'AIC'] == min(DF.input[,'AIC']) ),'index.subtree'];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.impurity <- initializePlot(title = NULL, subtitle = NULL);

    my.ggplot.impurity <- my.ggplot.impurity + geom_point(
        data    = DF.input,
        mapping = aes(x = tree.impurity, y = index.subtree),
        alpha   = 0.9,
        size    = my.size.point,
        colour  = "black"
        );

    my.ggplot.impurity <- my.ggplot.impurity + geom_line(
        data        = DF.input,
        mapping     = aes(x = tree.impurity, y = index.subtree),
        orientation = "y",
        alpha       = 0.5,
        size        = my.size.line,
        colour      = "black"
        );

    my.ggplot.impurity <- my.ggplot.impurity + geom_hline(
        yintercept = index.min.subtree,
        colour     = "red"
        );

    my.ggplot.impurity <- my.ggplot.impurity + scale_y_continuous(
        limits = NULL,
        breaks = my.breaks
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.alpha <- initializePlot(title = NULL, subtitle = NULL);

    my.ggplot.alpha <- my.ggplot.alpha + geom_step(
        data    = DF.input,
        mapping = aes(x = alpha, y = index.subtree),
        alpha   = 0.9,
        size    = my.size.point,
        colour  = "black"
        );

    my.ggplot.alpha <- my.ggplot.alpha + geom_hline(
        yintercept = index.min.subtree,
        colour     = "red"
        );

    my.ggplot.alpha <- my.ggplot.alpha + scale_y_continuous(
        limits = NULL,
        breaks = my.breaks
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.AIC <- initializePlot(title = NULL, subtitle = NULL);

    my.ggplot.AIC <- my.ggplot.AIC + geom_point(
        data    = DF.input,
        mapping = aes(x = AIC, y = index.subtree),
        alpha   = 0.9,
        size    = my.size.point,
        colour  = "black"
        );

    my.ggplot.AIC <- my.ggplot.AIC + geom_line(
        data        = DF.input,
        mapping     = aes(x = AIC, y = index.subtree),
        orientation = "y",
        alpha       = 0.5,
        size        = my.size.line,
        colour      = "black"
        );

    my.ggplot.AIC <- my.ggplot.AIC + geom_hline(
        yintercept = index.min.subtree,
        colour     = "red"
        );

    my.ggplot.AIC <- my.ggplot.AIC + scale_y_continuous(
        limits = NULL,
        breaks = my.breaks
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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(my.cowplot);

    }
