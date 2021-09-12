
test.nppCART.AIC_graphics <- function(
    simulations.directory      = NULL,
    DF.population              = NULL,
    scale_fill_gradient_limits = c(  0,2000),
    scale_fill_gradient_breaks = seq(0,2000,500)
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
            seed.directory             = file.path(normalizePath(simulations.directory),seed.directory),
            DF.population              = DF.population,
            scale_fill_gradient_limits = scale_fill_gradient_limits,
            scale_fill_gradient_breaks = scale_fill_gradient_breaks
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
    #         seed.directory             = file.path(normalizePath(simulations.directory),seed.directory),
    #         DF.population              = DF.population,
    #         scale_fill_gradient_limits = scale_fill_gradient_limits,
    #         scale_fill_gradient_breaks = scale_fill_gradient_breaks
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
    seed.directory             = NULL,
    DF.population              = NULL,
    scale_fill_gradient_limits = c(  0,2000),
    scale_fill_gradient_breaks = seq(0,2000,500)
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
    my.ggplot.fully.grown <- test.nppCART.AIC_graphics_hex(
        DF.input                   = DF.npdata.with.propensity,
        DF.population              = DF.population,
        propensity.variable        = "propensity",
        scale_fill_gradient_limits = scale_fill_gradient_limits,
        scale_fill_gradient_breaks = scale_fill_gradient_breaks
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
    my.ggplot.pruned <- test.nppCART.AIC_graphics_hex(
        DF.input                   = DF.npdata.with.propensity,
        DF.population              = DF.population,
        propensity.variable        = "propensity.pruned",
        scale_fill_gradient_limits = scale_fill_gradient_limits,
        scale_fill_gradient_breaks = scale_fill_gradient_breaks
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

        my.ggplot.fully.grown <- test.nppCART.AIC_graphics_scatter(
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
        my.ggplot.pruned <- test.nppCART.AIC_graphics_scatter(
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

##################################################
test.nppCART.AIC_graphics_hex <- function(
    DF.input                   = NULL,
    DF.population              = NULL,
    propensity.variable        = NULL,
    textsize.axis              = 20,
    scale_fill_gradient_limits = 500 * c(  0,4),
    scale_fill_gradient_breaks = 500 * seq(0,4,1)
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

test.nppCART.AIC_graphics_scatter <- function(
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
