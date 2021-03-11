
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
    DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],c("unit.ID","y","x1","x2","x1.jitter","x2.jitter")];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection, 1 - prob.selection)
        );

    DF.probability <- DF.population[is.selected,c("unit.ID","x1","x2")];
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

test.nppCART_get.population <- function(
    seed            = 1234567,
    population.flag = NULL,
    population.size = NULL,
    CSV.population  = paste0("DF-",population.flag,"-population.csv")
    ) {
    if ( file.exists(CSV.population) ) {
        DF.population <- read.csv(file = CSV.population);
        return( DF.population );
        }
    if ( "01" == population.flag ) {
        DF.population <- test.nppCART_get.population.01(
            seed            = seed,
            population.size = population.size
            );
    } else if ( "02" == population.flag ) {
        DF.population <- test.nppCART_get.population.02(
            seed            = seed,
            population.size = population.size
            );
    } else {
        DF.population <- test.nppCART_get.population.03(
            seed            = seed,
            population.size = population.size
            );
        }
    write.csv(
        x         = DF.population,
        file      = CSV.population,
        row.names = FALSE
        );
    return( DF.population );
    }

test.nppCART_get.population.02 <- function(
    seed            = 1234567,
    population.size = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.centres <- c(0.5,1.5,2.5);

    c1 <- sample(x = temp.centres, size = population.size, replace = TRUE);
    c2 <- sample(x = temp.centres, size = population.size, replace = TRUE);

    true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    sigma <- 0.2; # 0.8;
    x1 <- c1 + rnorm(n = population.size, mean = 0, sd = sigma);
    x2 <- c2 + rnorm(n = population.size, mean = 0, sd = sigma);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.numeric      = x1,
        x2.numeric      = x2,
        true.propensity = true.propensity
        );

    for ( colname.numeric in c("x1.numeric","x2.numeric") ) {

        temp.quantiles <- quantile(
            x     = DF.population[,colname.numeric],
            probs = c(1,2,3)/3
            );

        is.small  <- ifelse(DF.population[,colname.numeric] <  temp.quantiles[1],TRUE,FALSE);
        is.medium <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[1] & DF.population[,colname.numeric] < temp.quantiles[2],TRUE,FALSE);
        is.large  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[2],TRUE,FALSE);

        colname.factor <- gsub(x = colname.numeric, pattern = "\\.numeric", replacement = "");
        DF.population[,colname.factor] <- character(nrow(DF.population));

        if ( "x1.numeric" == colname.numeric ) {
            DF.population[is.small, colname.factor] <- "small";
            DF.population[is.medium,colname.factor] <- "medium";
            DF.population[is.large, colname.factor] <- "large";
            temp.levels <- c("small","medium","large");
        } else {
            DF.population[is.small, colname.factor] <- "petit";
            DF.population[is.medium,colname.factor] <- "moyen";
            DF.population[is.large, colname.factor] <- "grand";
            temp.levels <- c("petit","moyen","grand");
            }

        DF.population[,colname.factor] <- factor(
            x       = DF.population[,colname.factor],
            levels  = temp.levels,
            ordered = TRUE
            );

        colname.jitter <- gsub(x = colname.numeric, pattern = "numeric", replacement = "jitter");
        # DF.population[,colname.jitter] <- (-0.5) + as.numeric(DF.population[,colname.factor]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
        DF.population[,colname.jitter] <- as.numeric(DF.population[,colname.factor]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

        DF.population <- DF.population[,setdiff(colnames(DF.population),colname.numeric)];

        }

    return( DF.population );

    }

test.nppCART_get.population.03 <- function(
    seed            = 1234567,
    population.size = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp.centres <- c(0.5,1.5,2.5);

    c1 <- sample(x = temp.centres, size = population.size, replace = TRUE);
    c2 <- sample(x = temp.centres, size = population.size, replace = TRUE);

    true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    sigma <- 0.2; # 0.8;
    x1 <- c1 + rnorm(n = population.size, mean = 0, sd = sigma);
    x2 <- c2 + rnorm(n = population.size, mean = 0, sd = sigma);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.numeric      = x1,
        x2.numeric      = x2,
        true.propensity = true.propensity
        );

    for ( colname.numeric in c("x1.numeric","x2.numeric") ) {

        temp.quantiles <- quantile(
            x     = DF.population[,colname.numeric],
            probs = seq(1,9)/9
            );

        is.small.1  <- ifelse(DF.population[,colname.numeric] <  temp.quantiles[1],TRUE,FALSE);
        is.small.2  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[1] & DF.population[,colname.numeric] < temp.quantiles[2],TRUE,FALSE);
        is.small.3  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[2] & DF.population[,colname.numeric] < temp.quantiles[3],TRUE,FALSE);
        is.medium.1 <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[3] & DF.population[,colname.numeric] < temp.quantiles[4],TRUE,FALSE);
        is.medium.2 <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[4] & DF.population[,colname.numeric] < temp.quantiles[5],TRUE,FALSE);
        is.medium.3 <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[5] & DF.population[,colname.numeric] < temp.quantiles[6],TRUE,FALSE);
        is.large.1  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[6] & DF.population[,colname.numeric] < temp.quantiles[7],TRUE,FALSE);
        is.large.2  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[7] & DF.population[,colname.numeric] < temp.quantiles[8],TRUE,FALSE);
        is.large.3  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[8],TRUE,FALSE);

        colname.factor <- gsub(x = colname.numeric, pattern = "\\.numeric", replacement = "");
        DF.population[,colname.factor] <- character(nrow(DF.population));

        if ( "x1.numeric" == colname.numeric ) {

            DF.population[is.small.1, colname.factor] <- "small.1";
            DF.population[is.small.2, colname.factor] <- "small.2";
            DF.population[is.small.3, colname.factor] <- "small.3";

            DF.population[is.medium.1,colname.factor] <- "medium.1";
            DF.population[is.medium.2,colname.factor] <- "medium.2";
            DF.population[is.medium.3,colname.factor] <- "medium.3";

            DF.population[is.large.1, colname.factor] <- "large.1";
            DF.population[is.large.2, colname.factor] <- "large.2";
            DF.population[is.large.3, colname.factor] <- "large.3";

            temp.levels <- c(
                "small.1", "small.2", "small.3",
               "medium.1","medium.2","medium.3",
                "large.1", "large.2", "large.3"
               );

        } else {

            DF.population[is.small.1, colname.factor] <- "petit.1";
            DF.population[is.small.2, colname.factor] <- "petit.2";
            DF.population[is.small.3, colname.factor] <- "petit.3";

            DF.population[is.medium.1,colname.factor] <- "moyen.1";
            DF.population[is.medium.2,colname.factor] <- "moyen.2";
            DF.population[is.medium.3,colname.factor] <- "moyen.3";

            DF.population[is.large.1, colname.factor] <- "grand.1";
            DF.population[is.large.2, colname.factor] <- "grand.2";
            DF.population[is.large.3, colname.factor] <- "grand.3";

            temp.levels <- c(
               "petit.1","petit.2","petit.3",
               "moyen.1","moyen.2","moyen.3",
               "grand.1","grand.2","grand.3"
               );

            }

        DF.population[,colname.factor] <- factor(
            x       = DF.population[,colname.factor],
            levels  = temp.levels,
            ordered = TRUE
            );

        colname.jitter <- gsub(x = colname.numeric, pattern = "numeric", replacement = "jitter");
        DF.population[,colname.jitter] <- as.numeric(DF.population[,colname.factor]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

        DF.population <- DF.population[,setdiff(colnames(DF.population),colname.numeric)];

        }

    return( DF.population );

    }
