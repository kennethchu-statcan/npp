
visualizePopulation <- function(
    population     = NULL,
    textsize.title = 30,
    textsize.axis  = 20,
    addDiagonal    = FALSE,
    inputIsNumeric = FALSE,
    target_density_limits        = 1000 * c(  -4.5,4.5),
    target_density_breaks        = 1000 * seq(-4.0,4.0,2),
    propensity_density_limits    = c(-0.05,1.05),
    propensity_density_breaks    = seq(0,1,0.2),
    scale_colour_gradient_limits = c(0,1),
    scale_colour_gradient_breaks = c(0,0.25,0.5,0.75,1)
    ) {

    require(ggplot2);

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    FILE.output <- paste0('plot-population-y-density.png');

    my.ggplot <- ggplot(data = NULL) + theme_bw();
    my.ggplot <- my.ggplot + theme(
        title            = element_text(size = textsize.title, face = "bold"),
        axis.title.x     = element_text(size = textsize.axis,  face = "bold"),
        axis.title.y     = element_blank(),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
        panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25),
        legend.title     = element_text(size = textsize.axis,  face = "bold")
        );

    my.ggplot <- my.ggplot + labs(
        title    = NULL,
        subtitle = "Population"
        );

    my.ggplot <- my.ggplot + xlab("y (target variable)");

    my.ggplot <- my.ggplot + scale_x_continuous(
        limits = target_density_limits,
        breaks = target_density_breaks
        );

    my.ggplot <- my.ggplot + geom_density(
        data    = population,
        mapping = aes(x = y)
        );

    ggsave(
        file   = FILE.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  12,
        units  = 'in'
        );

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    FILE.output <- paste0('plot-population-propensity-density.png');

    my.ggplot <- ggplot(data = NULL) + theme_bw();
    my.ggplot <- my.ggplot + theme(
        title            = element_text(size = textsize.title, face = "bold"),
        axis.title.x     = element_text(size = textsize.axis,  face = "bold"),
        axis.title.y     = element_blank(),
        axis.text.x      = element_text(size = textsize.axis,  face = "bold"),
        axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
        panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
        panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25),
        legend.title     = element_text(size = textsize.axis,  face = "bold")
        );

    my.ggplot <- my.ggplot + labs(
        title    = NULL,
        subtitle = "Population"
        );

    my.ggplot <- my.ggplot + xlab("true propensity");

    my.ggplot <- my.ggplot + scale_x_continuous(
        limits = propensity_density_limits,
        breaks = propensity_density_breaks
        );

    my.ggplot <- my.ggplot + geom_density(
        data    = population,
        mapping = aes(x = true.propensity)
        );

    ggsave(
        file   = FILE.output,
        plot   = my.ggplot,
        dpi    = 300,
        height =   8,
        width  =  12,
        units  = 'in'
        );

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    levels.x3.hidden <- levels(population[,'x3.hidden']);
    for ( temp.level.x3.hidden in levels.x3.hidden ) {

        FILE.output <- paste0('plot-population-propensity-scatter-x3-',temp.level.x3.hidden,'.png');

        DF.temp <- population[population[,'x3.hidden'] == temp.level.x3.hidden,];

        my.ggplot <- ggplot(data = NULL) + theme_bw();
        my.ggplot <- my.ggplot + theme(
            title            = element_text(size = textsize.title, face = "bold"),
            axis.text.x      = element_text(size = textsize.axis,  face = "bold", angle = 90, vjust = 0.5, hjust = 0.5),
            axis.text.y      = element_text(size = textsize.axis,  face = "bold"),
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            panel.grid.major = element_line(colour="gray", linetype=2, size=0.25),
            panel.grid.minor = element_line(colour="gray", linetype=2, size=0.25),
            legend.title     = element_text(size = textsize.axis,  face = "bold"),
            legend.text      = element_text(size = textsize.axis,  face = "bold"),
            legend.position  = "bottom",
            legend.key.width = ggplot2::unit(0.75,"in")
            );

        my.ggplot <- my.ggplot + labs(
            title    = NULL,
            subtitle = paste0("Population (x3.hidden = ",temp.level.x3.hidden,")"),
            colour   = "true propensity   "
            );

        my.ggplot <- my.ggplot + geom_hline(yintercept = 0,colour="gray",size=0.75);
        my.ggplot <- my.ggplot + geom_vline(xintercept = 0,colour="gray",size=0.75);

        if ( inputIsNumeric ) {

            my.ggplot <- my.ggplot + scale_x_continuous(limits = c(0,3), breaks = seq(0,3,0.5));
            my.ggplot <- my.ggplot + scale_y_continuous(limits = c(0,3), breaks = seq(0,3,0.5));

            my.ggplot <- my.ggplot + scale_colour_gradient(
                limits = scale_colour_gradient_limits,
                breaks = scale_colour_gradient_breaks,
                low    = "black",
                high   = "red"
                );

            my.ggplot <- my.ggplot + geom_point(
                data    = DF.temp,
                mapping = aes(x = x1, y = x2, colour = true.propensity),
                alpha   = 0.2
                );

        } else {

            x1.levels <- levels(population$x1);
            x2.levels <- levels(population$x2);

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
                limits = scale_colour_gradient_limits,
                breaks = scale_colour_gradient_breaks,
                low    = "black",
                high   = "red"
                );

            my.ggplot <- my.ggplot + geom_point(
                data    = DF.temp,
                mapping = aes(x = x1.jitter, y = x2.jitter, colour = true.propensity),
                alpha   = 0.2
                );

            }

        if ( addDiagonal ) {
            my.ggplot <- my.ggplot + geom_abline(
                slope     = 1,
                intercept = 0,
                colour    = "gray",
                size      = 0.75
                );
            }

        ggsave(
            file   = FILE.output,
            plot   = my.ggplot,
            dpi    = 300,
            height =  11,
            width  =  10,
            units  = 'in'
            );

        }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    return(NULL);

    }
