
getPopulation <- function(
    seed            = 1234567,
    population.size = 10000,
    population.flag = "01"
    ) {

    set.seed(seed);

    DF.output <- data.frame(
        ID              = seq( 1,population.size),
        y               = rep(NA,population.size),
        x1              = rep(NA,population.size),
        x2              = rep(NA,population.size),
        w               = rep(NA,population.size),
        true.propensity = rep(NA,population.size)
        );

    if (        "01" == population.flag) {
        DF.output <- getPopulation.01(population.size = population.size);
    } else  if ("02" == population.flag) {
        DF.output <- getPopulation.02(population.size = population.size);
    } else  if ("03" == population.flag) {
        DF.output <- getPopulation.03(population.size = population.size);
    }

    return(DF.output);

    }

##################################################
my.transform <- function(x) {

    if ( x[1] >= x[2] ) {

        return( x );

    } else {

        theta <- pi / 4;
        R_minus_theta <- matrix(
            data  = c(cos(-theta),-sin(-theta),sin(-theta),cos(-theta)),
            nrow  = 2,
            byrow = TRUE
            );

        M <- matrix(
            data = c(1,0,0,1/8),
            nrow  = 2,
            byrow = TRUE
            );

        R_theta <- matrix(
            data  = c(cos(theta),-sin(theta),sin(theta),cos(theta)),
            nrow  = 2,
            byrow = TRUE
            );

        w    <- x;
        w    <- M %*% R_minus_theta %*% w;
        w[2] <- (0.3) * sqrt(w[2]);
        w    <- R_theta %*% w;

        return( c(w[1],w[2]) );

        }

    }

getPopulation.01 <- function(
    population.size = 10000
    ) {

    require(e1071);

    z1 <- rnorm(n = population.size);
    z2 <- rnorm(n = population.size);

    sigma <- 0.25;
    x1    <- exp(sigma * z1);
    x2    <- exp(sigma * z2);

    X <- rbind(x1,x2);
    X <- apply(X = X, MARGIN = 2, FUN = my.transform);

    x1 <- X[1,];
    x2 <- X[2,];

    b0 <-   11;
    b1 <-   13;
    b2 <- - 17;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 2.0)
    y       <- b0 + b1 * x1 + b2 * x2 + epsilon;

    w <- 10 * (x1 - x2);
    propensity <- e1071::sigmoid(w);

    DF.output <- data.frame(
        ID              = seq(1,population.size),
        y               = y,
        x1              = x1,
        x2              = x2,
        w               = w,
        true.propensity = propensity
        );

    return(DF.output);

    }

getPopulation.02 <- function(
    population.size = 10000
    ) {

    require(e1071);

    temp.centres <- c(0.5,1.5,2.5);

    c1 <- sample(x = temp.centres, size = population.size, replace = TRUE);
    c2 <- sample(x = temp.centres, size = population.size, replace = TRUE);

    is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);

    sigma <- 0.20;
    x1    <- c1 + rnorm(n = population.size, mean = 0, sd = sigma);
    x2    <- c2 + rnorm(n = population.size, mean = 0, sd = sigma);

    propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
    propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);

    y0 <- rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y       <- y0 + epsilon^2;

    DF.output <- data.frame(
        ID              = seq(1,population.size),
        y               = y,
        x1              = x1,
        x2              = x2,
        true.propensity = propensity
        );

    return(DF.output);

    }

getPopulation.03 <- function(
    population.size = 10000
    ) {

    require(e1071);

    z1 <- rnorm(n = population.size);
    z2 <- rnorm(n = population.size);

    sigma <- 0.25;
    x1    <- exp(sigma * z1);
    x2    <- exp(sigma * z2);

    X <- rbind(x1,x2);
    X <- apply(X = X, MARGIN = 2, FUN = my.transform);

    x1 <- X[1,];
    x2 <- X[2,];

    b0 <-   11;
    b1 <-   13;
    b2 <- - 17;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 5.0)
    y       <- b0 + b1 * x1 + b2 * x2 + epsilon^2;

    w <- 10 * (x1 - x2);
    propensity <- e1071::sigmoid(w);

    DF.output <- data.frame(
        ID              = seq(1,population.size),
        y               = y,
        x1              = x1,
        x2              = x2,
        w               = w,
        true.propensity = propensity
        );

    return(DF.output);

    }

get.modified.population <- function(DF.input = NULL) {

    DF.population <- DF.input;
    DF.population[,"factor.x1"] <- character(nrow(DF.population))

    x1Quantiles <- quantile(
        x     = DF.population[,"x1"],
        probs = c(1,2,3)/3
        );

    is.small_x1   <- ifelse(DF.population[,"x1"] <  x1Quantiles[1],TRUE,FALSE);
    is.medium_x1  <- ifelse(DF.population[,"x1"] >= x1Quantiles[1] & DF.population[,"x1"] < x1Quantiles[2],TRUE,FALSE);
    is.large_x1   <- ifelse(DF.population[,"x1"] >= x1Quantiles[2],TRUE,FALSE);

    DF.population[is.small_x1, "factor.x1"] <- "small_x1";
    DF.population[is.medium_x1,"factor.x1"] <- "medium_x1";
    DF.population[is.large_x1, "factor.x1"] <- "large_x1";
    x1.levels <- c("small_x1","medium_x1","large_x1")

    DF.population[,"factor.x1"] <- factor(
        x      = DF.population[,"factor.x1"],
        levels = x1.levels,
        ordered = TRUE
        );

    DF.population <- DF.population[,setdiff(colnames(DF.population),"x1")];
    colnames(DF.population) <- gsub(
        x           = colnames(DF.population),
        pattern     = "factor.x1",
        replacement = "x1"
        );
    DF.population <- DF.population[,colnames(DF.input)];

    ##################################################

    DF.population[,"factor.x2"] <- character(nrow(DF.population))

    x2Quantiles <- quantile(
        x     = DF.population[,"x2"],
        probs = c(1,2,3)/3
        );

    is.small_x2   <- ifelse(DF.population[,"x2"] <  x2Quantiles[1],TRUE,FALSE);
    is.medium_x2  <- ifelse(DF.population[,"x2"] >= x2Quantiles[1] & DF.population[,"x2"] < x2Quantiles[2],TRUE,FALSE);
    is.large_x2   <- ifelse(DF.population[,"x2"] >= x2Quantiles[2],TRUE,FALSE);

    DF.population[is.small_x2, "factor.x2"] <- "small_x2";
    DF.population[is.medium_x2,"factor.x2"] <- "medium_x2";
    DF.population[is.large_x2, "factor.x2"] <- "large_x2";
    x2.levels <- c("small_x2","medium_x2","large_x2")

    DF.population[,"factor.x2"] <- factor(
        x      = DF.population[,"factor.x2"],
        levels = x2.levels,
        ordered = TRUE
        );

    DF.population <- DF.population[,setdiff(colnames(DF.population),"x2")];
    colnames(DF.population) <- gsub(
        x           = colnames(DF.population),
        pattern     = "factor.x2",
        replacement = "x2"
        );
    DF.population <- DF.population[,colnames(DF.input)];

    ##################################################

    x1.numeric  <- unlist(lapply(X = DF.population$x1, FUN = function(x) { return(match(x, x1.levels)) }))
    DF.population[,"x1.numeric"] <- x1.numeric
    DF.population[,"x1.jitter"] <- jitter(x1.numeric, 2)

    x2.numeric  <- unlist(lapply(X = DF.population$x2, FUN = function(x) { return(match(x, x2.levels)) }))
    DF.population[,"x2.numeric"] <- x2.numeric
    DF.population[,"x2.jitter"] <- jitter(x2.numeric, 2)

    ##################################################
    return( DF.population );

    }
