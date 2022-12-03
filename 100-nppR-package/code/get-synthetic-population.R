#' Generate a synthetic population
#'
#' The \code{get.synthetic.population} function generate a synthetic population; it is used in the vignettes.
#'
#' @param population.size numeric vector of lengh 1 indicating desired population size. Must be a positive integer. Default value is 10000.
#' @param seed numeric vector of length 1 containing randomization seed. Must be a positive integer. Default is 1234567.
#'
#' @return a data frame containing data of the synthetic population.
#'
#' @examples
#' DF.population <- nppR::get.synthetic.population(
#'     population.size = 10000
#'     );
#'
#' @importFrom stats formula lm coef nlm cor
#' @export

get.synthetic.population <- function(
    population.size = 10000,
    seed            = 1234567
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::stopifnot(
        !base::is.null(population.size), # must not be NULL
        base::is.numeric(population.size), # must be numeric
        (base::length(population.size) == 1), # must be lf length 1
        (population.size == as.integer(population.size)), # must be an integer
        (population.size > 0) # must be strictly positive
        );

    base::stopifnot(
        !base::is.null(seed), # must not be NULL
        base::is.numeric(seed), # must be numeric
        (base::length(seed) == 1), # must be lf length 1
        (seed == as.integer(seed)), # must be an integer
        (seed > 0) # must be strictly positive
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ordered.x1 <- TRUE;
    ordered.x2 <- TRUE;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- base::c("small","medium","large");
    } else {
        levels.x1.hidden <- base::c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- base::c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- base::c("pomme","orange","banane");
        }

    levels.x3.hidden <- base::c("A","B","C");

    x1.hidden <- base::factor(x = base::sample(x = levels.x1.hidden, size = population.size, replace = TRUE), levels = levels.x1.hidden, ordered = ordered.x1);
    x2.hidden <- base::factor(x = base::sample(x = levels.x2.hidden, size = population.size, replace = TRUE), levels = levels.x2.hidden, ordered = ordered.x2);
    x3.hidden <- base::factor(x = base::sample(x = levels.x3.hidden, size = population.size, replace = TRUE), levels = levels.x3.hidden, ordered = FALSE     );

    c1 <- base::as.double(x1.hidden) - 0.5;
    c2 <- base::as.double(x2.hidden) - 0.5;
    c3 <- base::as.double(x3.hidden) - 0.5;
    is.off.diagonals <- (c2 - c1 == 1 | c2 - c1 == -1);

    is.high.propensity.A <- ( (x3.hidden == "A") &   is.off.diagonals  );
    is.high.propensity.B <- ( (x3.hidden == "B") & (!is.off.diagonals) );
    is.high.propensity.C <- ( (x3.hidden == "C") &   is.off.diagonals  );
    is.high.propensity   <- ( is.high.propensity.A | is.high.propensity.B | is.high.propensity.C );

    true.propensity                     <- stats::rnorm(n = population.size,         mean = 0.20, sd = 0.015);
    true.propensity[is.high.propensity] <- stats::rnorm(n = base::sum(is.high.propensity), mean = 0.80, sd = 0.015);

    y0 <- base::rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    y <- y0 + stats::rnorm(n = population.size, mean = 0, sd = 1.0)^2;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.subgroups <- 2;
    levels.x1   <- base::as.vector(base::sapply( X = levels.x1.hidden, FUN = function(x) {return(base::paste(x,1:n.subgroups,sep="."))} ));
    levels.x2   <- base::as.vector(base::sapply( X = levels.x2.hidden, FUN = function(x) {return(base::paste(x,1:n.subgroups,sep="."))} ));

    DF.population <- base::data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.hidden       = x1.hidden,
        x2.hidden       = x2.hidden,
        subgroup.1      = base::sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        subgroup.2      = base::sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        x3              = x3.hidden,
        x3.hidden       = base::factor(x = base::as.character(x3.hidden), levels = levels.x3.hidden, ordered = FALSE),
        true.propensity = true.propensity
        );

    DF.population[,'x1'] <- base::apply(
        X      = DF.population[,base::c('x1.hidden','subgroup.1')],
        MARGIN = 1,
        FUN    = function(x) { return(base::paste(x,collapse=".")) }
        );

    DF.population[,'x2'] <- base::apply(
        X      = DF.population[,base::c('x2.hidden','subgroup.2')],
        MARGIN = 1,
        FUN    = function(x) { return(base::paste(x,collapse=".")) }
        );

    DF.population[,'x1'] <- base::factor(x = DF.population[,'x1'], levels = levels.x1, ordered = ordered.x1);
    DF.population[,'x2'] <- base::factor(x = DF.population[,'x2'], levels = levels.x2, ordered = ordered.x2);

    DF.population[,"x1.jitter"] <- base::as.double(DF.population[,"x1"]) + stats::runif(n = base::nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- base::as.double(DF.population[,"x2"]) + stats::runif(n = base::nrow(DF.population), min = -0.3, max = 0.3);

  # DF.population[,"x3"] <- base::as.double(DF.population[,"x3"]) + stats::runif(n = base::nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x3"] <- base::as.double(DF.population[,"x3"]) + base::sample(x = c(-0.1,0.1), size = base::nrow(DF.population), replace = TRUE);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
  # DF.population <- DF.population[,base::c('unit.ID','y','x1','x2','x3','true.propensity','x1.jitter','x2.jitter','x3.hidden')];
    DF.population <- DF.population[,base::c('unit.ID','y','x1','x2','x3','true.propensity')];
    return( DF.population );

    }
