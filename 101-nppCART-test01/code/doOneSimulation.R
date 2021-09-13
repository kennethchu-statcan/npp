
doOneSimulation <- function(
    population.flag = "01",
    DF.population   = NULL,
    prob.selection  = 0.1
    ) {

    require(nppR);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.samples <- getSamples(
        DF.population  = DF.population,
        prob.selection = prob.selection
        );

    cat("\nstr(LIST.samples)\n");
    print( str(LIST.samples)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    nppTree <- nppR::nppCART(
        predictors = c("x1","x2"),
        np.data    = LIST.samples[['non.probability.sample']],
        p.data     = LIST.samples[['probability.sample']],
        weight     = "weight"
        );

    nppTree$grow();

    cat("\nstr(nppTree)\n");
    print( str(nppTree)   );

    nppTree$print(
        FUN.format = function(x) {return( round(x,digits=3) )} 
        );

    DF.npdata_with_propensity <- nppTree$get_npdata_with_propensity();

    colnames(DF.npdata_with_propensity) <- gsub(
        x           = colnames(DF.npdata_with_propensity),
        pattern     = "propensity",
        replacement = "p_hat"
        );
    DF.npdata_with_propensity <- merge(
        x  = DF.npdata_with_propensity,
        y  = DF.population[,c("ID","propensity")],
        by = "ID"
        );

    DF.npdata_with_propensity <- DF.npdata_with_propensity[order(DF.npdata_with_propensity[,"ID"]),];

    Y_total_hat_tree <- sum(
        DF.npdata_with_propensity[,"y"] / DF.npdata_with_propensity[,"p_hat"]
        );

    cor_propensity_tree <- cor(
        x = DF.npdata_with_propensity[,"p_hat"],
        y = DF.npdata_with_propensity[,"propensity"]
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualizePropensity(
        population.flag = population.flag,
        DF.input        = DF.npdata_with_propensity
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

