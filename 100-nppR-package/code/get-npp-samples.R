#' Take a (non-probability, probability) pair of samples from the given synthetic population
#'
#' The \code{get.npp.samples} function draws a (non-probability, probability) pair of samples from the given synthetic population data frame (generated with \code{nppR::get.sythetic.population( )})
#'
#' @param DF.population data frame containing synthetic population data (generated with \code{nppR::get.synthetic.population( )})
#' @param prob.selection numeric vector of length 1 containing the selection probability of the SRS probability sample. Must be a number strictly between 0 and 1. Default is 0.2.
#' @param n.replicates numeric vector of length 1 containing the number of sets of bootstrap weights. Must be a positive integer. Default is 500.
#' @param seed numeric vector of length 1 containing randomization seed. Must be a positive integer. Default is 7654321.
#'
#' @return list containing the non-probability and probability samples as data frames
#'
#' @examples
#' DF.population <- nppR::get.synthetic.population(
#'     population.size = 10000
#'     );
#'
#' list.samples  <- nppR::get.npp.samples(
#'     DF.population = DF.population,
#'     prob.selection = 0.2,
#'     n.replicates   = 500
#'     );
#'
#' @importFrom stats formula lm coef nlm cor
#' @export

get.npp.samples <- function(
    DF.population  = NULL,
    prob.selection = 0.2,
    n.replicates   = 500,
    seed           = 7654321
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::stopifnot(
        !base::is.null(DF.population), # must not be NULL
        base::is.data.frame(DF.population) | base::is.matrix(DF.population) | tibble::is_tibble(DF.population), # must be matrix-like data type
        base::nrow(DF.population) > 0, # must not be empty
        base::all(base::c("unit.ID","y","x1","x2") %in% base::colnames(DF.population))
        );

    base::stopifnot(
        !base::is.null(prob.selection), # must not be NULL
        base::is.numeric(prob.selection), # must be numeric
        (base::length(prob.selection) == 1), # must be lf length 1
        (prob.selection > 0), # must be strictly positive
        (prob.selection < 1) # must be strictly less than 1
        );

    base::stopifnot(
        !base::is.null(n.replicates), # must not be NULL
        base::is.numeric(n.replicates), # must be numeric
        (base::length(n.replicates) == 1), # must be lf length 1
        (n.replicates == as.integer(n.replicates)), # must be an integer
        (n.replicates > 0) # must be strictly positive
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
  # colnames.np <- base::c("unit.ID","y","x1","x2","x3","x1.jitter","x2.jitter","x3.hidden");
    colnames.np <- base::c("unit.ID","y","x1","x2","x3");
    colnames.p  <- base::c("unit.ID","x1","x2","x3");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected   <- base::sapply(
        X   = DF.population[,"true.propensity"],
        FUN = function(x) { base::sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
        );
    DF.non.probability <- DF.population;
    DF.non.probability[,"self.selected"] <- is.self.selected;
    DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],colnames.np];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.selected <- base::sample(
        x       = base::c(TRUE,FALSE),
        size    = base::nrow(DF.population),
        replace = TRUE,
        prob    = base::c(prob.selection, 1 - prob.selection)
        );

    DF.probability <- DF.population[is.selected,colnames.p];
    DF.probability[,"sampling.fraction"] <- prob.selection;
    DF.probability[,"design.weight"    ] <- 1 / prob.selection;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.svydesign.object <- survey::svydesign(
        data = DF.probability,
        id   = ~1,
        fpc  = ~sampling.fraction
        );

    my.svrepdesign.object <- survey::as.svrepdesign(
        design     = my.svydesign.object,
        type       = "bootstrap",
        replicates = n.replicates # 500
        );

    DF.repweights <- my.svrepdesign.object[['repweights']][['weights']];
    DF.repweights <- base::as.data.frame(DF.repweights);
    base::colnames(DF.repweights) <- base::paste0("repweight",base::seq(1,base::ncol(DF.repweights)));

    DF.probability <- base::cbind(
        my.svrepdesign.object[['variables']],
        DF.repweights
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(list(
        DF.non.probability = DF.non.probability,
        DF.probability     = DF.probability
        ));

    }
