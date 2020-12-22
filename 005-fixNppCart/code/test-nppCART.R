
test.nppCART <- function(
    FILE.results   = "results-simulations.csv",
    n.iterations   = 10,
    DF.population  = NULL,
    prob.selection = 0.1
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    my.seed         <- 7654321;
    population.size <- 10000;
    alpha0          <- 0.25;
    prob.selection  <- 0.1;
    population.flag <- "01"

    my.population <- getPopulation(
        seed            = my.seed,
        N               = population.size,
        alpha0          = alpha0,
        population.flag = population.flag
        );

    my.population <- get.modified.population(my.population)

    print( str(    my.population) );
    print( summary(my.population) );
    print( head(   my.population) );

    doOneSimulation(
        population.flag = population.flag,
        DF.population   = my.population,
        prob.selection  = prob.selection
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    return( NULL );

    }

get.modified.population <- function(my.population = NULL) {

    DF.population <- my.population
    DF.population[,"factor.x1"] <- character(nrow(DF.population))    
    
    x1Quantiles <- quantile(
        x     = DF.population[,"x1"],
        probs = c(1,2,3,4,5,6,7)/7
        );

    is.smallest     <- ifelse(DF.population[,"x1"] <  x1Quantiles[1],TRUE,FALSE);
    is.verysmall    <- ifelse(DF.population[,"x1"] >= x1Quantiles[1] & DF.population[,"x1"] < x1Quantiles[2],TRUE,FALSE);
    is.small        <- ifelse(DF.population[,"x1"] >= x1Quantiles[2] & DF.population[,"x1"] < x1Quantiles[3],TRUE,FALSE);
    is.medium       <- ifelse(DF.population[,"x1"] >= x1Quantiles[3] & DF.population[,"x1"] < x1Quantiles[5],TRUE,FALSE);
    is.large        <- ifelse(DF.population[,"x1"] >= x1Quantiles[5] & DF.population[,"x1"] < x1Quantiles[6],TRUE,FALSE);
    is.verylarge    <- ifelse(DF.population[,"x1"] >= x1Quantiles[6] & DF.population[,"x1"] < x1Quantiles[7],TRUE,FALSE);
    is.largest      <- ifelse(DF.population[,"x1"] >= x1Quantiles[7],TRUE,FALSE);

    DF.population[is.smallest, "factor.x1"]     <- "smallest";
    DF.population[is.verysmall, "factor.x1"]    <- "verysmall";
    DF.population[is.small, "factor.x1"]        <- "small";
    DF.population[is.medium,"factor.x1"]        <- "medium";
    DF.population[is.large,  "factor.x1"]       <- "large";
    DF.population[is.verylarge, "factor.x1"]    <- "verylarge";
    DF.population[is.largest, "factor.x1"]      <- "largest";

    DF.population[,"factor.x1"] <- factor(
        x      = DF.population[,"factor.x1"],
        levels = c("smallest", "verysmall","small","medium","large","verylarge", "largest")
        );

    DF.population <- DF.population[,setdiff(colnames(DF.population),"x1")];
    colnames(DF.population) <- gsub(
        x           = colnames(DF.population),
        pattern     = "factor.x1",
        replacement = "x1"
        );
    DF.population <- DF.population[,colnames(my.population)];

    my.population <- DF.population;
    remove(DF.population);

    return( my.population );

    }

