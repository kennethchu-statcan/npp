
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
    if ( !dir.exists(temp.directory) ) { dir.create(temp.directory); }
    setwd(temp.directory);

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
    sink(file = NULL, type = "output" );
    sink(file = NULL, type = "message");
    sink();
    setwd(original.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
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

test.nppCART.AIC_do.one.simulation_scatter <- function(
    DF.input            = NULL,
    DF.population       = NULL,
    subtitle            = NULL,
    propensity.variable = NULL,
    textsize.axis       = 20
    ) {

    my.ggplot <- initializePlot(title = NULL, subtitle = NULL);
    my.ggplot <- my.ggplot + theme(
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold", angle = 90, vjust = 0.5, hjust = 0.5),
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    my.ggplot <- my.ggplot + geom_hline(yintercept = 0,colour="gray",size=0.75);
    my.ggplot <- my.ggplot + geom_vline(xintercept = 0,colour="gray",size=0.75);

    x1.levels <- levels(DF.input[,'x1']);
    x2.levels <- levels(DF.input[,'x2']);

    my.ggplot <- my.ggplot + scale_x_continuous(
        limits = c(  0,length(x1.levels)+1),
        breaks = seq(0,length(x1.levels)+1,1),
        labels = c("",x1.levels,"")
        );

    my.ggplot <- my.ggplot + scale_y_continuous(
        limits = c(  0,length(x2.levels)+1),
        breaks = seq(0,length(x2.levels)+1,1),
        labels = c("",x2.levels,"")
        );

    my.ggplot <- my.ggplot + scale_colour_gradient(
        limits = c(0,1),
        breaks = c(0,0.25,0.5,0.75,1),
        low    = "black",
        high   = "red"
        );

    colnames(DF.input) <- gsub(
        x           = colnames(DF.input),
        pattern     = propensity.variable,
        replacement = "variable.to.plot"
        );

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.input,
        mapping = aes(x = x1.jitter, y = x2.jitter, colour = variable.to.plot),
        alpha   = 0.2
        );

    my.ggplot <- my.ggplot + labs(
        title    = NULL,
        subtitle = subtitle,
        colour   = ifelse("propensity" == propensity.variable,"estd. propensity (fully grown)   ","estd. propensity (pruned)   ")
        );

    return(my.ggplot);

    }

test.nppCART.AIC_do.one.simulation_hex <- function(
    DF.input            = NULL,
    DF.population       = NULL,
    propensity.variable = NULL,
    textsize.axis       = 20
    ) {

    my.ggplot <- initializePlot(title = NULL, subtitle = "Simulation");
    my.ggplot <- my.ggplot + theme(
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    my.ggplot <- my.ggplot + geom_hline(yintercept = 0,colour="gray",size=0.75);
    my.ggplot <- my.ggplot + geom_vline(xintercept = 0,colour="gray",size=0.75);

    my.ggplot <- my.ggplot + xlab("true.propensity");
    my.ggplot <- my.ggplot + ylab(ifelse(test = ("propensity" == propensity.variable), yes = "propensity.fully.grown", no = propensity.variable));

    my.ggplot <- my.ggplot + scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2));
    my.ggplot <- my.ggplot + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2));

    # if ( population.flag %in% c("02","03") ) {
    #     scale_fill_gradient_limits <- 300 * c(  0,1);
    #     scale_fill_gradient_breaks <- 300 * seq(0,1,0.25);
    # } else {
    #     scale_fill_gradient_limits <- 500 * c(  0,4);
    #     scale_fill_gradient_breaks <- 500 * seq(0,4,1);
    #     }

    scale_fill_gradient_limits <- 500 * c(  0,4);
    scale_fill_gradient_breaks <- 500 * seq(0,4,1);
    my.ggplot <- my.ggplot + scale_fill_gradient(
        limits = scale_fill_gradient_limits,
        breaks = scale_fill_gradient_breaks,
        low    = "lightgrey",
        high   = "red"
        );

    colnames(DF.input) <- gsub(
        x           = colnames(DF.input),
        pattern     = propensity.variable,
        replacement = "variable.to.plot"
        );

    DF.input <- merge(
        x  = DF.input,
        y  = DF.population[,c('unit.ID','true.propensity')],
        by = 'unit.ID'
        );

    my.ggplot <- my.ggplot + geom_hex(
        data     = DF.input,
        mapping  = aes(x = true.propensity, y = variable.to.plot),
        binwidth = c(0.02,0.02)
        );

    my.ggplot <- my.ggplot + geom_abline(
        slope     = 1,
        intercept = 0,
        colour    = "gray",
        size      = 0.75
        );

    return(my.ggplot);

    }

####################
# test.nppCART.AIC_do.many.simulations <- function(
#     seed            = NULL,
#     population.flag = NULL,
#     DF.population   = NULL,
#     prob.selection  = NULL,
#     n.replicates    = NULL,
#     n.simulations   = NULL,
#     CSV.output      = paste0("DF-",population.flag,"-simulations.csv")
#     ) {
#
#     thisFunctionName <- "test.nppCART.AIC_do.many.simulations";
#
#     cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
#     cat(paste0("\n",thisFunctionName,"() starts.\n\n"));
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     if ( file.exists(CSV.output) ) {
#         DF.output <- read.csv(file = CSV.output);
#         return(DF.output);
#         }
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     temp.directory <- "plots-impurity-alpha-AIC";
#     if ( !dir.exists(temp.directory) ) { dir.create(path = temp.directory, recursive = TRUE) }
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     cat(paste0("\n# randomization seed: ",seed,"\n"));
#     set.seed(seed = seed);
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     for ( index.simulation in seq(1,n.simulations) ) {
#
#         cat(paste0("\n### index.simulation: ",index.simulation,"\n"));
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         list.samples  <- test.nppCART_get.samples(
#             DF.population         = DF.population,
#             prob.selection        = prob.selection, # 0.1, 1 - 1e-8, # 1.0,
#             n.replicates          = n.replicates
#             # RData.non.probability = "DF-non-probability.RData",
#             # RData.probability     = "DF-probability.RData"
#             );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
#         DF.population[                ,'self.selected'] <- FALSE;
#         DF.population[is.self.selected,'self.selected'] <- TRUE;
#
#         cat("\nstr(list.samples[['DF.non.probability']])\n");
#         print( str(list.samples[['DF.non.probability']])   );
#
#         DF.probability <- list.samples[['DF.probability']];
#         cat("\nstr(DF.probability)\n");
#         print( str(DF.probability)   )
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         current.nppCART <- nppR::nppCART(
#             np.data       = list.samples[['DF.non.probability']],
#             p.data        = DF.probability,
#             predictors    = c("x1","x2","x3"),
#             weight        = "design.weight",
#             min.cell.size = 1,
#             min.impurity  = 1e-50,
#             max.levels    = 10000
#             );
#
#         current.nppCART$grow();
#         cat("\ncurrent.nppCART$print()\n");
#         current.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );
#
#         DF.current <- current.nppCART$get_npdata_with_propensity();
#         cat("\nstr(DF.current)\n");
#         print( str(DF.current)   );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         my.nppCART <- nppCART(
#             np.data                   = list.samples[['DF.non.probability']],
#             p.data                    = DF.probability,
#             predictors                = c("x1","x2","x3"),
#             sampling.weight           = "design.weight",
#             bootstrap.weights         = paste0("repweight",seq(1,n.replicates)),
#             min.cell.size             = 1,
#             min.impurity              = 1e-50,
#             n.levels.approx.threshold = 4
#             );
#
#         my.nppCART$grow();
#         cat("\nmy.nppCART$print()\n");
#         my.nppCART$print( FUN.format = function(x) { return(format(x = x, digits = 3)) } );
#
#         DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
#         cat("\nstr(DF.npdata.with.propensity)\n");
#         print( str(DF.npdata.with.propensity)   );
#
#         DF.impurity.alpha.AIC <- my.nppCART$get_impurities_alphas_AICs();
#         cat("\nDF.impurity.alpha.AIC\n");
#         print( DF.impurity.alpha.AIC   );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         DF.temp <- data.frame(
#             index.simulation     = index.simulation,
#             estimate.current     = sum(               DF.current[,'y'] /                DF.current[,'propensity'       ]),
#             estimate.fully.grown = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity'       ]),
#             estimate.pruned      = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity.pruned'])
#             );
#
#         write.csv(
#             file      = paste0("tmp-population-",population.flag,"-",index.simulation,".csv"),
#             x         = DF.temp,
#             row.names = FALSE
#             );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         my.ggplot.alpha <- initializePlot(title = NULL, subtitle = NULL);
#
#         my.ggplot.alpha <- my.ggplot.alpha + geom_step(
#             data    = DF.impurity.alpha.AIC,
#             mapping = aes(x = alpha, y = index.subtree),
#             alpha   = 0.9,
#             size    = 1.3,
#             colour  = "black"
#             );
#
#         my.ggplot.alpha <- my.ggplot.alpha + scale_y_continuous(
#             limits = NULL,
#             breaks = seq(0,100,1)
#             );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         my.ggplot.impurity <- initializePlot(title = NULL, subtitle = NULL);
#
#         my.ggplot.impurity <- my.ggplot.impurity + geom_point(
#             data    = DF.impurity.alpha.AIC,
#             mapping = aes(x = tree.impurity, y = index.subtree),
#             alpha   = 0.9,
#             size    = 2.0,
#             colour  = "black"
#             );
#
#         my.ggplot.impurity <- my.ggplot.impurity + geom_line(
#             data        = DF.impurity.alpha.AIC,
#             mapping     = aes(x = tree.impurity, y = index.subtree),
#             orientation = "y",
#             alpha       = 0.5,
#             size        = 0.5,
#             colour      = "black"
#             );
#
#         my.ggplot.impurity <- my.ggplot.impurity + scale_y_continuous(
#             limits = NULL,
#             breaks = seq(0,100,1)
#             );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         my.ggplot.AIC <- initializePlot(title = NULL, subtitle = NULL);
#
#         my.ggplot.AIC <- my.ggplot.AIC + geom_point(
#             data    = DF.impurity.alpha.AIC,
#             mapping = aes(x = AIC, y = index.subtree),
#             alpha   = 0.9,
#             size    = 2.0,
#             colour  = "black"
#             );
#
#         my.ggplot.AIC <- my.ggplot.AIC + geom_line(
#             data        = DF.impurity.alpha.AIC,
#             mapping     = aes(x = AIC, y = index.subtree),
#             orientation = "y",
#             alpha       = 0.5,
#             size        = 0.5,
#             colour      = "black"
#             );
#
#         my.ggplot.AIC <- my.ggplot.AIC + scale_y_continuous(
#             limits = NULL,
#             breaks = seq(0,100,1)
#             );
#
#         ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#         my.cowplot <- cowplot::plot_grid(
#             my.ggplot.impurity,
#             my.ggplot.alpha,
#             my.ggplot.AIC,
#             nrow       = 1,
#             align      = "h",
#             rel_widths = c(2,3,2)
#             );
#
#         index.simulation.string <- stringr::str_pad(
#             string = as.character(index.simulation),
#             width  = nchar(as.character(n.simulations)),
#             side   = "left",
#             pad    = "0"
#             );
#
#         PNG.output <- paste0("plot-",population.flag,"-impurity-alpha-AIC-",index.simulation.string,".png");
#         cowplot::ggsave2(
#             file   = file.path(temp.directory,PNG.output),
#             plot   = my.cowplot,
#             dpi    = 300,
#             height =   5,
#             width  =  20,
#             units  = 'in'
#             );
#
#         }
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     DF.output <- data.frame(
#         index.simulation     = seq(1,n.simulations),
#         estimate.current     = rep(NA,times=n.simulations),
#         estimate.fully.grown = rep(NA,times=n.simulations),
#         estimate.pruned      = rep(NA,times=n.simulations)
#         );
#
#     temp.files <- list.files(pattern = paste0("tmp-population-"));
#     for ( temp.file in temp.files ) {
#         DF.temp <- read.csv(file = temp.file);
#         index.simulation <- DF.temp[1,'index.simulation'];
#         DF.output[index.simulation,'estimate.current']     <- DF.temp[1,'estimate.current'];
#         DF.output[index.simulation,'estimate.fully.grown'] <- DF.temp[1,'estimate.fully.grown'];
#         DF.output[index.simulation,'estimate.pruned']      <- DF.temp[1,'estimate.pruned'];
#         }
#
#     file.remove(temp.files);
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     write.csv(
#         file      = CSV.output,
#         x         = DF.output,
#         row.names = FALSE
#         );
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     cat(paste0("\n# ",thisFunctionName,"() quits."));
#     cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
#     return( DF.output );
#
#     }

# test.nppCART.AIC <- function(
#     seed            = 1234567,
#     population.flag = NULL,
#     population.size = NULL,
#     prob.selection  = 0.1,
#     n.replicates    = 500,
#     n.simulations   = 10
#     ) {
#
#     thisFunctionName <- "test.nppCART.AIC";
#
#     cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
#     cat(paste0("\n",thisFunctionName,"() starts.\n\n"));
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     DF.population <- test.nppCART_get.population(
#         seed            = seed,
#         population.flag = population.flag,
#         population.size = population.size,
#         ordered.x1      = FALSE,
#         ordered.x2      = TRUE
#         );
#
#     cat("\nstr(DF.population)\n");
#     print( str(DF.population)   );
#
#     cat("\ntable(DF.population[,c('x1','x2','x3.hidden')])\n");
#     print( table(DF.population[,c('x1','x2','x3.hidden')])   );
#
#     visualizePopulation(
#         population.flag = population.flag,
#         population      = DF.population,
#         textsize.title  = 30,
#         textsize.axis   = 20,
#         inputIsNumeric  = FALSE
#         );
#
#     ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     test.nppCART.AIC_do.one.simulation(
#         seed            = ceiling(seed/2),
#         population.flag = population.flag,
#         DF.population   = DF.population,
#         prob.selection  = prob.selection,
#         n.replicates    = n.replicates,
#         n.simulations   = n.simulations
#         );
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     cat(paste0("\n# ",thisFunctionName,"() quits."));
#     cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
#     return( NULL );
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     DF.simulations <- test.nppCART.AIC_do.many.simulations(
#         seed            = seed,
#         population.flag = population.flag,
#         DF.population   = DF.population,
#         prob.selection  = prob.selection,
#         n.replicates    = n.replicates,
#         n.simulations   = n.simulations
#         );
#
#     test.nppCART.AIC_plot.simulations(
#         DF.simulations   = DF.simulations,
#         vline.xintercept = sum(DF.population[,'y']),
#         bin.width        = 3000,
#         PNG.output       = paste0("plot-simulation-",population.flag,"-histograms.png")
#         );
#
#     ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#     cat(paste0("\n# ",thisFunctionName,"() quits."));
#     cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
#     return( NULL );
#
#     }