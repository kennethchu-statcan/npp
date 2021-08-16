
test.nppCART_get.population <- function(
    seed             = 1234567,
    population.flag  = NULL,
    population.size  = NULL,
    ordered.x1       = TRUE,
    ordered.x2       = TRUE,
    RData.population = paste0("DF-",population.flag,"-population.RData")
    ) {
    if ( file.exists(RData.population) ) {
        DF.population <- readRDS(file = RData.population);
        return( DF.population );
        }
    if ( "sanity" == population.flag ) {
        DF.population <- test.nppCART_get.population.mixed(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
    } else if ( "independent" == population.flag ) {
        DF.population <- test.nppCART_get.population.independent(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
    } else if ( "mixed" == population.flag ) {
        DF.population <- test.nppCART_get.population.mixed(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
    } else if ( "02" == population.flag ) {
        DF.population <- test.nppCART_get.population.02(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
    } else {
        DF.population <- test.nppCART_get.population.03(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
        }
    saveRDS(file = RData.population, object = DF.population);
    return( DF.population );
    }

test.nppCART_get.samples <- function(
    DF.population         = NULL,
    prob.selection        = 0.1,
    n.replicates          = 500,
    RData.non.probability = NULL,
    RData.probability     = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected   <- sapply(
        X   = DF.population[,"true.propensity"],
        FUN = function(x) { sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
        );
    DF.non.probability <- DF.population;
    DF.non.probability[,"self.selected"] <- is.self.selected;
    DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],c("unit.ID","y","x1","x2","x3","x1.jitter","x2.jitter","x3.hidden")];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection, 1 - prob.selection)
        );

    DF.probability <- DF.population[is.selected,c("unit.ID","x1","x2","x3")];
    DF.probability[,"sampling.fraction"] <- prob.selection;
    DF.probability[,"design.weight"    ] <- 1 / prob.selection;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.svydesign.object <- survey::svydesign(
        data = DF.probability,
        id   = ~1,
        fpc  = ~sampling.fraction
        );
    cat("\nstr(my.svydesign.object)\n");
    print( str(my.svydesign.object)   );

    my.svrepdesign.object <- survey::as.svrepdesign(
        design     = my.svydesign.object,
        type       = "bootstrap",
        replicates = n.replicates # 500
        );
    cat("\nstr(my.svrepdesign.object)\n");
    print( str(my.svrepdesign.object)   );

    DF.repweights <- my.svrepdesign.object[['repweights']][['weights']];
    DF.repweights <- as.data.frame(DF.repweights);
    colnames(DF.repweights) <- paste0("repweight",seq(1,ncol(DF.repweights)));
    # cat("\nstr(DF.repweights)\n");
    # print( str(DF.repweights)   );

    DF.probability <- cbind(
        my.svrepdesign.object[['variables']],
        DF.repweights
        );
    # cat("\nstr(DF.probability)\n");
    # print( str(DF.probability)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !is.null(RData.non.probability) ) {
        saveRDS(
            object = DF.non.probability,
            file   = RData.non.probability,
            );
        }

    if ( !is.null(RData.probability) ) {
        saveRDS(
            object = DF.probability,
            file   = RData.probability
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(list(
        DF.non.probability = DF.non.probability,
        DF.probability     = DF.probability
        ));

    }

##################################################
test.nppCART_get.population.independent <- function(
    seed            = 1234567,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- c("small","medium","large");
    } else {
        levels.x1.hidden <- c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- c("pomme","orange","banane");
        }

    levels.x3.hidden <- c("A","B","C");

    x1.hidden <- factor(x = sample(x = levels.x1.hidden, size = population.size, replace = TRUE), levels = levels.x1.hidden, ordered = ordered.x1);
    x2.hidden <- factor(x = sample(x = levels.x2.hidden, size = population.size, replace = TRUE), levels = levels.x2.hidden, ordered = ordered.x2);
    x3.hidden <- factor(x = sample(x = levels.x3.hidden, size = population.size, replace = TRUE), levels = levels.x3.hidden, ordered = FALSE     );

    c1 <- as.double(x1.hidden) - 0.5;
    c2 <- as.double(x2.hidden) - 0.5;
    c3 <- as.double(x3.hidden) - 0.5;
    is.off.diagonals <- (c2 - c1 == 1 | c2 - c1 == -1);

    is.high.propensity.A <- ( (x3.hidden == "A") &   is.off.diagonals  );
    is.high.propensity.B <- ( (x3.hidden == "B") & (!is.off.diagonals) );
    is.high.propensity.C <- ( (x3.hidden == "C") &   is.off.diagonals  );
    is.high.propensity   <- ( is.high.propensity.A | is.high.propensity.B | is.high.propensity.C );

    true.propensity                     <- rnorm(n = population.size,         mean = 0.20, sd = 0.015);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.80, sd = 0.015);

    # y0 <- rep(x = 30, times = population.size);
    # y0[is.high.propensity] <- 110;
    #
    # epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    # y <- y0 + epsilon^2;

    ####################
    ###  Now, y is independent of the true self-selection propensity
    ###  as well as the predictor variables.
    y <- rnorm(n = population.size, mean = 8e4, sd = 1.5e4);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.subgroups <- 2;
    levels.x1   <- as.vector(sapply( X = levels.x1.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));
    levels.x2   <- as.vector(sapply( X = levels.x2.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.hidden       = x1.hidden,
        x2.hidden       = x2.hidden,
        subgroup.1      = sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        subgroup.2      = sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        x3              = x3.hidden,
        x3.hidden       = factor(x = as.character(x3.hidden), levels = levels.x3.hidden, ordered = FALSE),
        true.propensity = true.propensity
        );

    DF.population[,'x1'] <- apply(
        X      = DF.population[,c('x1.hidden','subgroup.1')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x2'] <- apply(
        X      = DF.population[,c('x2.hidden','subgroup.2')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x1'] <- factor(x = DF.population[,'x1'], levels = levels.x1, ordered = ordered.x1);
    DF.population[,'x2'] <- factor(x = DF.population[,'x2'], levels = levels.x2, ordered = ordered.x2);

    DF.population[,"x1.jitter"] <- as.double(DF.population[,"x1"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- as.double(DF.population[,"x2"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

  # DF.population[,"x3"] <- as.double(DF.population[,"x3"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x3"] <- as.double(DF.population[,"x3"]) + sample(x = c(-0.1,0.1), size = nrow(DF.population), replace = TRUE);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- DF.population[,c('unit.ID','y','x1','x2','x3','true.propensity','x1.jitter','x2.jitter','x3.hidden')];
    return( DF.population );

    }

test.nppCART_get.population.mixed <- function(
    seed            = 1234567,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- c("small","medium","large");
    } else {
        levels.x1.hidden <- c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- c("pomme","orange","banane");
        }

    levels.x3.hidden <- c("A","B","C");

    x1.hidden <- factor(x = sample(x = levels.x1.hidden, size = population.size, replace = TRUE), levels = levels.x1.hidden, ordered = ordered.x1);
    x2.hidden <- factor(x = sample(x = levels.x2.hidden, size = population.size, replace = TRUE), levels = levels.x2.hidden, ordered = ordered.x2);
    x3.hidden <- factor(x = sample(x = levels.x3.hidden, size = population.size, replace = TRUE), levels = levels.x3.hidden, ordered = FALSE     );

    c1 <- as.double(x1.hidden) - 0.5;
    c2 <- as.double(x2.hidden) - 0.5;
    c3 <- as.double(x3.hidden) - 0.5;
    is.off.diagonals <- (c2 - c1 == 1 | c2 - c1 == -1);

    is.high.propensity.A <- ( (x3.hidden == "A") &   is.off.diagonals  );
    is.high.propensity.B <- ( (x3.hidden == "B") & (!is.off.diagonals) );
    is.high.propensity.C <- ( (x3.hidden == "C") &   is.off.diagonals  );
    is.high.propensity   <- ( is.high.propensity.A | is.high.propensity.B | is.high.propensity.C );

    true.propensity                     <- rnorm(n = population.size,         mean = 0.20, sd = 0.015);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.80, sd = 0.015);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.subgroups <- 2;
    levels.x1   <- as.vector(sapply( X = levels.x1.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));
    levels.x2   <- as.vector(sapply( X = levels.x2.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.hidden       = x1.hidden,
        x2.hidden       = x2.hidden,
        subgroup.1      = sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        subgroup.2      = sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        x3              = x3.hidden,
        x3.hidden       = factor(x = as.character(x3.hidden), levels = levels.x3.hidden, ordered = FALSE),
        true.propensity = true.propensity
        );

    DF.population[,'x1'] <- apply(
        X      = DF.population[,c('x1.hidden','subgroup.1')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x2'] <- apply(
        X      = DF.population[,c('x2.hidden','subgroup.2')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x1'] <- factor(x = DF.population[,'x1'], levels = levels.x1, ordered = ordered.x1);
    DF.population[,'x2'] <- factor(x = DF.population[,'x2'], levels = levels.x2, ordered = ordered.x2);

    DF.population[,"x1.jitter"] <- as.double(DF.population[,"x1"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- as.double(DF.population[,"x2"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

  # DF.population[,"x3"] <- as.double(DF.population[,"x3"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x3"] <- as.double(DF.population[,"x3"]) + sample(x = c(-0.1,0.1), size = nrow(DF.population), replace = TRUE);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- DF.population[,c('unit.ID','y','x1','x2','x3','true.propensity','x1.jitter','x2.jitter','x3.hidden')];
    return( DF.population );

    }

test.nppCART_get.population.02 <- function(
    seed            = 1234567,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- c("small","medium","large");
    } else {
        levels.x1.hidden <- c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- c("pomme","orange","banane");
        }

    x1 <- factor(x = sample(x = levels.x1, size = population.size, replace = TRUE), levels = levels.x1, ordered = ordered.x1);
    x2 <- factor(x = sample(x = levels.x2, size = population.size, replace = TRUE), levels = levels.x2, ordered = ordered.x2);

    c1 <- as.double(x1) - 0.5;
    c2 <- as.double(x2) - 0.5;
    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);

    true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1              = x1,
        x2              = x2,
        true.propensity = true.propensity
        );

    DF.population[,"x1.jitter"] <- as.double(DF.population[,"x1"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- as.double(DF.population[,"x2"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.population );

    }

test.nppCART_get.population.03 <- function(
    seed            = 1234567,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- c("small","medium","large");
    } else {
        levels.x1.hidden <- c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- c("pomme","orange","banane");
        }

    x1.hidden <- factor(x = sample(x = levels.x1.hidden, size = population.size, replace = TRUE), levels = levels.x1.hidden, ordered = ordered.x1);
    x2.hidden <- factor(x = sample(x = levels.x2.hidden, size = population.size, replace = TRUE), levels = levels.x2.hidden, ordered = ordered.x2);

    c1 <- as.double(x1.hidden) - 0.5;
    c2 <- as.double(x2.hidden) - 0.5;
    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);

    true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    levels.x1 <- as.vector(sapply( X = levels.x1.hidden, FUN = function(x) {return(paste(x,1:3,sep="."))} ));
    levels.x2 <- as.vector(sapply( X = levels.x2.hidden, FUN = function(x) {return(paste(x,1:3,sep="."))} ));

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.hidden       = x1.hidden,
        x2.hidden       = x2.hidden,
        subgroup.1      = sample(x = 1:3, size = population.size, replace = TRUE),
        subgroup.2      = sample(x = 1:3, size = population.size, replace = TRUE),
        true.propensity = true.propensity
        );

    DF.population[,'x1'] <- apply(
        X      = DF.population[,c('x1.hidden','subgroup.1')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x2'] <- apply(
        X      = DF.population[,c('x2.hidden','subgroup.2')],
        MARGIN = 1,
        FUN    = function(x) { return(paste(x,collapse=".")) }
        );

    DF.population[,'x1'] <- factor(x = DF.population[,'x1'], levels = levels.x1, ordered = ordered.x1);
    DF.population[,'x2'] <- factor(x = DF.population[,'x2'], levels = levels.x2, ordered = ordered.x2);

    DF.population[,"x1.jitter"] <- as.double(DF.population[,"x1"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- as.double(DF.population[,"x2"]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- DF.population[,setdiff(colnames(DF.population),c('x1.hidden','x2.hidden','subgroup.1','subgroup.2'))];
    return( DF.population );

    }
