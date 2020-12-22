
doOneSimulation <- function(
    population.flag = "01",
    DF.population   = NULL,
    prob.selection  = 0.1
    ) {

    print("A-01");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.samples <- getSamples(
        DF.population  = DF.population,
        prob.selection = prob.selection
        );

    cat("\nstr(LIST.samples)\n");
    print( str(LIST.samples)   );

    print("A-02");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    nppTree <- nppCART(
        predictors = c("x1","x2"),
        np.data    = LIST.samples[['non.probability.sample']],
        p.data     = LIST.samples[['probability.sample']],
        weight     = "weight"
        );

    print("A-03");

    nppTree$grow();

    print("A-04");

    cat("\nstr(nppTree)\n");
    print( str(nppTree)   );

    nppTree$print(
        FUN.format = function(x) {return( round(x,digits=3) )} 
        );

    print("A-05");

    DF.npdata_with_propensity <- nppTree$get_npdata_with_propensity();

    print("A-06");

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

    print("A-07");

    Y_total_hat_tree <- sum(
        DF.npdata_with_propensity[,"y"] / DF.npdata_with_propensity[,"p_hat"]
        );

    print("A-08");

    cor_propensity_tree <- cor(
        x = DF.npdata_with_propensity[,"p_hat"],
        y = DF.npdata_with_propensity[,"propensity"]
        );

    print("A-09");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    visualizePropensity(
        population.flag = population.flag,
        DF.input        = DF.npdata_with_propensity
        );

    print("A-10");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

