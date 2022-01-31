
plot.impurity.alpha.AIC <- function(
    DF.input = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.breaks     <- seq(0,300,20);
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

    my.ggplot.impurity <- my.ggplot.impurity + scale_x_continuous(
        limits = (1e-3) * c(6.75,8.25),
        breaks = (1e-3) * c(6.50,7.00,7.50,8.00,8.50),
        labels = scales::scientific
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
