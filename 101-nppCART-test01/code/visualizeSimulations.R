
visualizeSimulations <- function(
    population.flag  = NULL,
    FILE.input       = NULL,
    textsize.title   = 20,
    textsize.axis    = 20,
    vline_xintercept = NULL
    ) {

    require(ggplot2);

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    DF.input <- read.csv(file = FILE.input);

    cat("\nstr(DF.output)\n");
    print( str(DF.input)    );

    cat("\nsummary(DF.input)\n");
    print( summary(DF.input)   );

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    temp.limits <- 100000 *   c(0, 2.1     );
    temp.breaks <- 100000 * seq(0, 2.0, 0.4);

    if ( "03" == population.flag ) {
        temp.limits <- 100000 *   c( 0, 6.5   );
        temp.breaks <- 100000 * seq( 0, 6.0, 1);
    } else if ( "02" == population.flag ) {
        temp.limits <- 1000000 *   c( 0, 1.3     );
        temp.breaks <- 1000000 * seq( 0, 1.3, 0.2);
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    plotOneHistogram(
        DF.input         = DF.input,
        target.variable  = 'Y_total_hat_propensity',
        FILE.output      = paste0('histogram-Ty-hat-propensity-',population.flag,'.png'),
        plot.title       = paste0("non-probability sample ",population.flag,",  true propensity"),
        plot.subtitle    = NULL,
        vline_xintercept = vline_xintercept,
        textsize.title   = textsize.title,
        textsize.axis    = textsize.axis,
        limits           = temp.limits,
        breaks           = temp.breaks
        );

    plotOneHistogram(
        DF.input         = DF.input,
        target.variable  = 'Y_total_hat_tree',
        FILE.output      = paste0('histogram-Ty-hat-tree-',population.flag,'.png'),
        plot.title       = paste0("non-probability sample ",population.flag,",  tree-based IPW"),
        plot.subtitle    = NULL,
        vline_xintercept = vline_xintercept,
        textsize.title   = textsize.title,
        textsize.axis    = textsize.axis,
        limits           = temp.limits,
        breaks           = temp.breaks
        );

    plotOneHistogram(
        DF.input         = DF.input,
        target.variable  = 'Y_total_hat_calibration',
        FILE.output      = paste0('histogram-Ty-hat-calibration-',population.flag,'.png'),
        plot.title       = paste0("non-probability sample ",population.flag,",  calibration"),
        plot.subtitle    = NULL,
        vline_xintercept = vline_xintercept,
        textsize.title   = textsize.title,
        textsize.axis    = textsize.axis,
        limits           = temp.limits,
        breaks           = temp.breaks
        );

    plotOneHistogram(
        DF.input         = DF.input,
        target.variable  = 'Y_total_hat_naive',
        FILE.output      = paste0('histogram-Ty-hat-naive-',population.flag,'.png'),
        plot.title       = paste0("non-probability sample ",population.flag,",  naive"),
        plot.subtitle    = NULL,
        vline_xintercept = vline_xintercept,
        textsize.title   = textsize.title,
        textsize.axis    = textsize.axis,
        limits           = temp.limits,
        breaks           = temp.breaks
        );

    plotOneHistogram(
        DF.input         = DF.input,
        target.variable  = 'Y_total_hat_CLW',
        FILE.output      = paste0('histogram-Ty-hat-CLW-',population.flag,'.png'),
        plot.title       = paste0("non-probability sample ",population.flag,",  Chen-Li-Wu"),
        plot.subtitle    = NULL,
        vline_xintercept = vline_xintercept,
        textsize.title   = textsize.title,
        textsize.axis    = textsize.axis,
        limits           = temp.limits,
        breaks           = temp.breaks
        );

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    return(NULL);

    }

##################################################
plotOneHistogram <- function(
    DF.input         = NULL,
    target.variable  = NULL,
    FILE.output      = NULL,
    plot.title       = NULL,
    plot.subtitle    = NULL,
    vline_xintercept = NULL,
    textsize.title   = NULL,
    textsize.axis    = NULL,
    limits           = NULL,
    breaks           = NULL
    ) {

    my.ggplot <- ggplot(data = NULL) + theme_bw();
    my.ggplot <- my.ggplot + theme(
        title            = element_text(size = textsize.title, face = "bold"),
        legend.text      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
        axis.title.x     = element_blank(),
        axis.title.y     = element_blank(),
        panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25),
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(0.75,"in")
        );

    my.ggplot <- my.ggplot + labs(
        title    = plot.title,
        subtitle = plot.subtitle
        );

    my.ggplot <- my.ggplot + geom_vline(xintercept = 0,colour="gray",size=0.75);
    my.ggplot <- my.ggplot + geom_hline(yintercept = 0,colour="gray",size=0.75);

    my.ggplot <- my.ggplot + scale_x_continuous(
        limits = limits,
        breaks = breaks
        );
    #my.ggplot <- my.ggplot + scale_y_continuous(limits=c(0,3),breaks=seq(0,3,0.5));

    DF.temp <- data.frame(x = DF.input[,target.variable]);

    my.ggplot <- my.ggplot + geom_histogram(
        data     = DF.temp,
        mapping  = aes(x = x),
        alpha    = 0.5,
        binwidth = 2000
        );

    my.ggplot <- my.ggplot + geom_vline(xintercept = vline_xintercept,colour="orange",size=1.00);

    MCRelBias <- NA;
    MCRelBias <- (DF.input[,target.variable] - vline_xintercept) / vline_xintercept;
    MCRelBias <- mean( MCRelBias, na.rm = TRUE );
    MCRelBias <- round(MCRelBias,3);

    MCRelRMSE <- NA;
    temp.vect <- DF.input[!is.na(DF.input[,target.variable]),target.variable];
    MCRelRMSE <- ((temp.vect - vline_xintercept)^2) / (vline_xintercept^2) ;
    MCRelRMSE <- sqrt(mean( MCRelRMSE ));
    MCRelRMSE <- round(MCRelRMSE,3);

    temp.xmax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['x']]$get_limits())
    temp.ymax <- max(layer_scales(my.ggplot,i=1L,j=1L)[['y']]$get_limits())

    temp.min  <- min(DF.input[,target.variable], na.rm = TRUE);
    temp.min  <- format(temp.min, digits = 3, scientific = TRUE);

    temp.max  <- max(DF.input[,target.variable], na.rm = TRUE);
    temp.max  <- format(temp.max, digits = 3, scientific = TRUE);

    temp.iter <- nrow( DF.input );
    temp.NA   <- sum(is.na( DF.input[,target.variable] ));

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
        x     = temp.xmax * 0.8 * c(1,1,1,1,1,1),
        y     = temp.ymax * c(0.98,0.91,0.81,0.74,0.64,0.57),
        size  = 10,
        color = "black"
        );

    ggsave(
        file   = FILE.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  12,
        units  = 'in'
        );

    return( NULL );

    }

