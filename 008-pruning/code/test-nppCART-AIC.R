
test.nppCART.AIC <- function(
    seed            = 1234567,
    population.flag = NULL,
    population.size = NULL,
    prob.selection  = 0.1,
    n.replicates    = 500,
    n.simulations   = 10
    ) {

    thisFunctionName <- "test.nppCART.AIC";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(
        seed            = seed,
        population.flag = population.flag,
        population.size = population.size
        );

    visualizePopulation(
        population.flag = population.flag,
        population      = DF.population,
        textsize.title  = 30,
        textsize.axis   = 20,
        inputIsNumeric  = FALSE
        );

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    test.nppCART.AIC_do.one.simulation(
        seed            = ceiling(seed/2),
        population.flag = population.flag,
        DF.population   = DF.population,
        prob.selection  = prob.selection,
        n.replicates    = n.replicates,
        n.simulations   = n.simulations
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.simulations <- test.nppCART.AIC_do.simulations(
        seed            = seed,
        population.flag = population.flag,
        DF.population   = DF.population,
        prob.selection  = prob.selection,
        n.replicates    = n.replicates,
        n.simulations   = n.simulations
        );

    test.nppCART.AIC_plot.simulations(
        DF.simulations   = DF.simulations,
        vline.xintercept = sum(DF.population[,'y']),
        bin.width        = 3000,
        PNG.output       = paste0("plot-population-",population.flag,"-histograms.png")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

####################
test.nppCART.AIC_do.one.simulation <- function(
    seed            = NULL,
    population.flag = NULL,
    DF.population   = NULL,
    prob.selection  = NULL,
    n.replicates    = NULL,
    n.simulations   = NULL
    # CSV.output      = paste0("DF-",population.flag,"-simulations.csv")
    ) {

    thisFunctionName <- "test.nppCART.AIC_do.one.simulation";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.samples  <- test.nppCART_get.samples(
        DF.population  = DF.population,
        prob.selection = prob.selection, # 0.1, 1 - 1e-8, # 1.0,
        n.replicates   = n.replicates
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    cat("\nstr(list.samples[['DF.non.probability']])\n");
    print( str(list.samples[['DF.non.probability']])   );

    DF.probability <- list.samples[['DF.probability']];
    cat("\nstr(DF.probability)\n");
    print( str(DF.probability)   )

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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.fully.grown <- test.nppCART.AIC_do.one.simulation_scatter(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        population.flag     = population.flag,
        propensity.variable = "propensity"
        );

    my.ggplot.pruned <- test.nppCART.AIC_do.one.simulation_scatter(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        population.flag     = population.flag,
        propensity.variable = "propensity.pruned"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        my.ggplot.fully.grown,
        my.ggplot.pruned,
        nrow       = 1,
        align      = "h",
        rel_widths = c(1,1)
        );

    PNG.output <- paste0("plot-simulation-",population.flag,"-propensity-scatter.png");
    cowplot::ggsave2(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 300,
        height =  11,
        width  =  20,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot.fully.grown <- test.nppCART.AIC_do.one.simulation_hex(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        population.flag     = population.flag,
        propensity.variable = "propensity"
        );

    my.ggplot.pruned <- test.nppCART.AIC_do.one.simulation_hex(
        DF.input            = DF.npdata.with.propensity,
        DF.population       = DF.population,
        population.flag     = population.flag,
        propensity.variable = "propensity.pruned"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.cowplot <- cowplot::plot_grid(
        my.ggplot.fully.grown,
        my.ggplot.pruned,
        nrow       = 1,
        align      = "h",
        rel_widths = c(1,1)
        );

    PNG.output <- paste0("plot-simulation-",population.flag,"-propensity-hex.png");
    cowplot::ggsave2(
        file   = PNG.output,
        plot   = my.cowplot,
        dpi    = 300,
        height =  11,
        width  =  20,
        units  = 'in'
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

test.nppCART.AIC_do.one.simulation_scatter <- function(
    DF.input            = NULL,
    DF.population       = NULL,
    population.flag     = NULL,
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
        labels = c("",x1.levels,"")
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
        subtitle = paste0("Simulation ",population.flag),
        colour   = ifelse("propensity" == propensity.variable,"estd. propensity (fully grown)   ","estd. propensity (pruned)   ")
        );

    return(my.ggplot);

    }

test.nppCART.AIC_do.one.simulation_hex <- function(
    DF.input            = NULL,
    DF.population       = NULL,
    population.flag     = NULL,
    propensity.variable = NULL,
    textsize.axis       = 20
    ) {

    my.ggplot <- initializePlot(title = NULL, subtitle = paste0("Simulation ",population.flag));
    my.ggplot <- my.ggplot + theme(
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    my.ggplot <- my.ggplot + geom_hline(yintercept = 0,colour="gray",size=0.75);
    my.ggplot <- my.ggplot + geom_vline(xintercept = 0,colour="gray",size=0.75);

    my.ggplot <- my.ggplot + xlab("true.propensity");
    my.ggplot <- my.ggplot + ylab(ifelse(test = ("propensity" == propensity.variable),yes = "propensity.fully.grown", no = propensity.variable));

    my.ggplot <- my.ggplot + scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2));
    my.ggplot <- my.ggplot + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2));

    if ( population.flag %in% c("02","03") ) {
        scale_fill_gradient_limits <- 300 * c(  0,1);
        scale_fill_gradient_breaks <- 300 * seq(0,1,0.25);
    } else {
        scale_fill_gradient_limits <- 500 * c(  0,4);
        scale_fill_gradient_breaks <- 500 * seq(0,4,1);
        }

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

    return(my.ggplot);

    }

test.nppCART.AIC_do.simulations <- function(
    seed            = NULL,
    population.flag = NULL,
    DF.population   = NULL,
    prob.selection  = NULL,
    n.replicates    = NULL,
    n.simulations   = NULL,
    CSV.output      = paste0("DF-",population.flag,"-simulations.csv")
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
    temp.directory <- "plots-impurity-alpha-AIC";
    if ( !dir.exists(temp.directory) ) { dir.create(path = temp.directory, recursive = TRUE) }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

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

        cat("\nstr(list.samples[['DF.non.probability']])\n");
        print( str(list.samples[['DF.non.probability']])   );

        DF.probability <- list.samples[['DF.probability']];
        cat("\nstr(DF.probability)\n");
        print( str(DF.probability)   )

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        current.nppCART <- nppR::nppCART(
            np.data       = list.samples[['DF.non.probability']],
            p.data        = DF.probability,
            predictors    = c("x1","x2"),
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
        DF.temp <- data.frame(
            index.simulation     = index.simulation,
            estimate.current     = sum(               DF.current[,'y'] /                DF.current[,'propensity'       ]),
            estimate.fully.grown = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity'       ]),
            estimate.pruned      = sum(DF.npdata.with.propensity[,'y'] / DF.npdata.with.propensity[,'propensity.pruned'])
            );

        write.csv(
            file      = paste0("tmp-population-",population.flag,"-",index.simulation,".csv"),
            x         = DF.temp,
            row.names = FALSE
            );

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

        PNG.output <- paste0("plot-",population.flag,"-impurity-alpha-AIC-",index.simulation.string,".png");
        cowplot::ggsave2(
            file   = file.path(temp.directory,PNG.output),
            plot   = my.cowplot,
            dpi    = 300,
            height =   5,
            width  =  20,
            units  = 'in'
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- data.frame(
        index.simulation     = seq(1,n.simulations),
        estimate.current     = rep(NA,times=n.simulations),
        estimate.fully.grown = rep(NA,times=n.simulations),
        estimate.pruned      = rep(NA,times=n.simulations)
        );

    temp.files <- list.files(pattern = paste0("tmp-population-"));
    for ( temp.file in temp.files ) {
        DF.temp <- read.csv(file = temp.file);
        index.simulation <- DF.temp[1,'index.simulation'];
        DF.output[index.simulation,'estimate.current']     <- DF.temp[1,'estimate.current'];
        DF.output[index.simulation,'estimate.fully.grown'] <- DF.temp[1,'estimate.fully.grown'];
        DF.output[index.simulation,'estimate.pruned']      <- DF.temp[1,'estimate.pruned'];
        }

    file.remove(temp.files);

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

test.nppCART.AIC_plot.simulations <- function(
    DF.simulations   = NULL,
    vline.xintercept = NULL,
    bin.width        = 6000,
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
        limits = c(0,1e6),
        breaks = seq(0,1e6,1e5)
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
        limits = c(0,1e6),
        breaks = seq(0,1e6,1e5)
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
        limits = c(0,1e6),
        breaks = seq(0,1e6,1e5)
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
