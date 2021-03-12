
test.nppCART.sanity <- function(
    seed            = 1234567,
    population.flag = "sanity",
    population.size = 10000,
    n.replicates    =   500
    ) {

    thisFunctionName <- "test.nppCART.sanity";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(
        seed            = seed,
        population.flag = population.flag,
        population.size = population.size
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.samples  <- test.nppCART_get.samples(
        DF.population         = DF.population,
        prob.selection        = 0.999999999,
        n.replicates          = n.replicates,
        RData.non.probability = paste0("DF-",population.flag,"-non-probability.RData"),
        RData.probability     = paste0("DF-",population.flag,"-probability.RData")
        );

    cat("\nstr(list.samples[['DF.probability']])\n");
    print( str(list.samples[['DF.probability']])   );

    cat("\nsummary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])\n");
    print( summary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART <- nppCART(
        np.data           = list.samples[['DF.non.probability']],
        p.data            = list.samples[['DF.probability'    ]], # DF.probability,
        predictors        = c("x1","x2"),
        sampling.weight   = "design.weight",
        bootstrap.weights = paste0("repweight",seq(1,n.replicates)),
        min.cell.size     = 1,
        min.impurity      = 1e-9,
        max.levels        = 10000
        );

    my.nppCART$grow();
    cat("\nmy.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} )\n");
    my.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} );

    DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
    cat("\nstr(DF.npdata.with.propensity)\n");
    print( str(DF.npdata.with.propensity)   );

    DF.impurity.alpha.AIC <- my.nppCART$get_impurities_alphas_AICs();
    cat("\nDF.impurity.alpha.AIC\n");
    print( DF.impurity.alpha.AIC   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    write.csv(
        x         = DF.population,
        file      = "DF-sanity-population-python.csv",
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = self.selected ~ .,
        data    = DF.population[,c('x1','x2','self.selected')],
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    # cat("\nresults.rpart\n");
    # print( results.rpart   );

    png("plot-rpart.png");
    rpart.plot(x = results.rpart);
    dev.off();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }
