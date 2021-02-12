
test.nppCART_get.samples <- function(
    DF.population         = NULL,
    RData.non.probability = NULL,
    RData.probability     = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected   <- sapply(
        X   = DF.population[,"true.propensity"],
        FUN = function(x) { sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
        );
    DF.non.probability <- DF.population;
    DF.non.probability[,"self.selected"] <- is.self.selected;
    DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],c("unit.ID","y","x1","x2","x1.jitter","x2.jitter")];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    prob.selection <- 0.1;

    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection, 1 - prob.selection)
        );

    DF.probability <- DF.population[is.selected,c("unit.ID","x1","x2")];
    DF.probability[,"design.weight"] <- 1 / prob.selection;

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

test.nppCART_get.population <- function(seed = 1234567) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# randomization seed: ",seed,"\n"));
    set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    population.size <- 10000;

    temp.centres <- c(0.5,1.5,2.5);

    c1 <- sample(x = temp.centres, size = population.size, replace = TRUE);
    c2 <- sample(x = temp.centres, size = population.size, replace = TRUE);

    true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);
    true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    sigma <- 0.8;
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
        DF.population[,colname.jitter] <- (-0.5) + as.numeric(DF.population[,colname.factor]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);

        DF.population <- DF.population[,setdiff(colnames(DF.population),colname.numeric)];

        }

    return( DF.population );

    }
