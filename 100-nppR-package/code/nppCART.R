#' nppCART
#'
#' The nppCART algorithm, developed by Kenneth Chu and Jean-François Beaumont,
#' is intended for estimating the self-selection propensity of each unit
#' in a non-probability sample, via a variant of the CART algorithm,
#' by incorporating auxiliary information from an appropriate and compatible
#' probability sample.
#' For more information about the algorithm and underlying methodology,
#' please consult the vignette 'nppCART-article'
#' by executing the command: \code{vignette("nppCART-article")}.
#'
#' @docType class
#'
#' @import R6
#'
#' @return The nppCART function returns an instance of a R6 class instantiated with the input parameters.
#'
#' @format \code{\link{R6Class}} object
#'
#' @examples
#' # See the vignette 'nppCART-usage' for more details, by executing the command: vignette("nppCART-usage")
#'
#' ### Generate data frame for synthetic population
#' population.size <- 10000;
#'
#' temp.centres <- c(0.5,1.5,2.5);
#' c1 <- sample(x = temp.centres, size = population.size, replace = TRUE);
#' c2 <- sample(x = temp.centres, size = population.size, replace = TRUE);
#'
#' true.propensity <- rnorm(n = population.size, mean = 0.25, sd = 0.025);
#' is.high.propensity <- (c2 - c1 == 1 | c2 - c1 == -1);
#' true.propensity[is.high.propensity] <- rnorm(n = sum(is.high.propensity), mean = 0.75, sd = 0.025);
#'
#' sigma <- 0.20;
#' x1 <- c1 + rnorm(n = population.size, mean = 0, sd = sigma);
#' x2 <- c2 + rnorm(n = population.size, mean = 0, sd = sigma);
#'
#' y0 <- rep(x = 30, times = population.size);
#' y0[is.high.propensity] <- 110;
#'
#' epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
#' y <- y0 + epsilon^2;
#'
#' DF.population <- data.frame(
#'     unit.ID         = seq(1,population.size),
#'     y               = y,
#'     x1.numeric      = x1,
#'     x2.numeric      = x2,
#'     true.propensity = true.propensity
#'     );
#'
#' for ( colname.numeric in c("x1.numeric","x2.numeric") ) {
#'
#'     temp.quantiles <- quantile(
#'         x     = DF.population[,colname.numeric],
#'         probs = c(1,2,3)/3
#'         );
#'
#'     is.small  <- ifelse(DF.population[,colname.numeric] <  temp.quantiles[1],TRUE,FALSE);
#'     is.medium <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[1] & DF.population[,colname.numeric] < temp.quantiles[2],TRUE,FALSE);
#'     is.large  <- ifelse(DF.population[,colname.numeric] >= temp.quantiles[2],TRUE,FALSE);
#'
#'     colname.factor <- gsub(x = colname.numeric, pattern = "\\.numeric", replacement = "");
#'     DF.population[,colname.factor] <- character(nrow(DF.population));
#'
#'     if ( "x1.numeric" == colname.numeric ) {
#'         DF.population[is.small, colname.factor] <- "small";
#'         DF.population[is.medium,colname.factor] <- "medium";
#'         DF.population[is.large, colname.factor] <- "large";
#'         temp.levels <- c("small","medium","large");
#'     } else {
#'         DF.population[is.small, colname.factor] <- "petit";
#'         DF.population[is.medium,colname.factor] <- "moyen";
#'         DF.population[is.large, colname.factor] <- "grand";
#'         temp.levels <- c("petit","moyen","grand");
#'         }
#'
#'     DF.population[,colname.factor] <- factor(
#'         x       = DF.population[,colname.factor],
#'         levels  = temp.levels,
#'         ordered = TRUE
#'         );
#'
#'     colname.jitter <- gsub(x = colname.numeric, pattern = "numeric", replacement = "jitter");
#'     DF.population[,colname.jitter] <- (-0.5) + as.numeric(DF.population[,colname.factor]) + runif(n = nrow(DF.population), min = -0.3, max = 0.3);
#'
#'     DF.population <- DF.population[,setdiff(colnames(DF.population),colname.numeric)];
#'
#'     }
#'
#' ### Generate data frame for non-probability sample
#' DF.non.probability <- DF.population;
#' DF.non.probability[,"self.selected"] <- sapply(
#'     X   = DF.non.probability[,"true.propensity"],
#'     FUN = function(x) { sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
#'     );
#' DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],c("unit.ID","y","x1","x2","x1.jitter","x2.jitter")];
#'
#' ### Generate data frame for probability sample
#' prob.selection <- 0.1;
#' is.selected <- sample(
#'     x       = c(TRUE,FALSE),
#'     size    = nrow(DF.population),
#'     replace = TRUE,
#'     prob    = c(prob.selection, 1 - prob.selection)
#'     );
#' DF.probability <- DF.population[is.selected,c("unit.ID","x1","x2")];
#' DF.probability[,"design.weight"] <- 1 / prob.selection;
#'
#' ### Instantiate nppCART object
#' my.nppCART <- nppR::nppCART(
#'     np.data         = DF.non.probability,
#'     p.data          = DF.probability,
#'     predictors      = c("x1","x2"),
#'     sampling.weight = "design.weight"
#'     );
#'
#' ### Grow the classification tree
#' my.nppCART$grow();
#'
#' ### Inspect the fully grown tree
#' my.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} );
#'
#' ### Extract the nppCART-estimated propensities
#' DF.npdata.estimated.propensity <- my.nppCART$get_npdata_with_propensity();
#'
#' @param np.data (required) non-empty data frame containing data of the non-probability sample.
#' @param p.data (required) non-empty data frame containing data of the probability sample.
#' @param sampling.weight (required) character vector of length 1. Column name of 'p.data' corresponding to sampling weights. Must have positive values throughout.
#' @param bootstrap.weights (optional) character vector of length at least 1. Column name(s) in 'p.data' corresponding to the bootstrap weights.
#' @param predictors (optional) character vector of length at least 1. Column name(s) common to both 'np.data' and 'p.data' corresponding to predictor variables. If not specified, all columns of 'p.data' except 'sampling.weight' and 'bootstrap.weights' are taken as predictors.
#' @param impurity (optional) character vector of length 1 specifying which impurity function to use. Must be one of 'gini' or 'entropy'. Default is 'entropy'.
#' @param min.cell.size.np (optional) numeric vector of length 1 specifying the minimum number of non-probability sample units in a node for partitioning to continue for that node. Must be a positive integer. If not specified, default value of 10 is used.
#' @param min.cell.size.p (optional) numeric vector of length 1 specifying the minimum number of probability sample units in a node for partitioning to continue for that node. Must be a positive integer. If not specified, default value of 10 is used.
#' @param min.impurity (optional) numeric vector of length 1 specifying the minimum impurity value below which node partitioning will stop. Must be a positive number. If not specified, default value of 0.095 is used.
#' @param n.levels.approx.threshold (optional) numeric vector of length 1. For each node and for each categorical (i.e. non-ordered factor) predictor variable, if the number of levels of the given categorical predictor variable is less than or equal to n.levels.approx.threshold, then all possible splits by the given categorical variable are considered; otherwse (i.e. if the number of levels of the given categorical predictor variable in the given node strictly exceeds n.levels.approx.threshold), then an approximate procedure is used. The input must be an integer greater than or equal to zero. If not specified, default value of 10 is used.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize(predictors, np.data, p.data, sampling.weight, bootstrap.weights, min.cell.size.np, min.cell.size.p, min.impurity)}}{This method is called when the R6 class is created (i.e. when nppCART is called). The arguments passed into nppCART are passed into initialize. This method contains input integrity checks to ensure that the arguments meet the required specifications. In addition, the method does some preprocessing of the input data.}
#'  \item{\code{get_instantiation_data()}}{This method is used to retrieve the instantiation data.}
#'  \item{\code{grow()}}{This method grows a binary classification tree using the nppCART algorithm based on the input non-probability and probability samples, until stopping criteria are met.}
#'  \item{\code{get_npdata_with_propensity()}}{This method returns a dataframe that contains the input non-probability sample data, with nppCART results augmented on the right. The nppCART-generated columns include: the unique identifier ('nodeID') of the terminal node of the unpruned tree containing the non-probability sample unit of each given row, the self-selection propensity estimate ('propensity') of the given terminal node of the unpruned tree; the number ('np.count') of non-probability sample units belonging to the given terminal node of the unpruned tree, the sum ('p.weight') of the sampling weights of the probability sample units belonging to the given terminal node of the unpruned tree, and the impurity ('impurity') of the given terminal node of the unpruned tree. If 'bootstrap.weights' is supplied, the output dataframe also contains the columns 'nodeID.pruned', 'propensity.pruned', 'np.count.pruned', 'p.weight.pruned', 'impurity.pruned', which contain results corresponding to the optimally pruned treed.}
#'  \item{\code{get_pdata_with_nodeID()}}{This method returns a dataframe that contains the input probability sample data, with nppCART results augmented on the right. The column names and respective contents of the nppCART-generated columns are the same as those of the output dataframe of 'get_npdata_with_propensity()'.}
#'  \item{\code{print()}}{This method is used to print the classification tree in a readable format (each node is on a separate line and indented appropriately). There is one parameter, FUN.format, which is a function that customizes the output format. This method should be used after calling grow.}
#'  \item{\code{get_subtree_hierarchy()}}{If 'bootstrap.weights' is supplied, this method returns a list containing the data of the pruning sub-tree hierarchy.}
#'  \item{\code{get_impurities_alphas_AICs()}}{If 'bootstrap.weights' is supplied, this method returns a data frame containing the alpha and AIC values of the sub-trees in the pruning sub-tree hierarchy.}
#' }
#'
#' @export

nppCART <- function(
    np.data                   = NULL,
    p.data                    = NULL,
    sampling.weight           = NULL,
    bootstrap.weights         = NULL,
    predictors                = base::setdiff(base::colnames(p.data),c(sampling.weight,bootstrap.weights)),
    impurity                  = 'entropy',
    min.cell.size.np          = 10,
    min.cell.size.p           = 10,
    min.impurity              = 0.095,
    n.levels.approx.threshold = 10
    ) {

    return(
        R6_nppCART$new(
            np.data                   = np.data,
            p.data                    = p.data,
            sampling.weight           = sampling.weight,
            bootstrap.weights         = bootstrap.weights,
            predictors                = predictors,
            impurity                  = base::tolower(impurity),
            min.cell.size.np          = min.cell.size.np,
            min.cell.size.p           = min.cell.size.p,
            min.impurity              = min.impurity,
            n.levels.approx.threshold = n.levels.approx.threshold
            )
        );
    }

R6_nppCART <- R6::R6Class(
    classname = "R6_nppCART",

    public = base::list(

        initialize = function(
            np.data                   = NULL,
            p.data                    = NULL,
            sampling.weight           = NULL,
            bootstrap.weights         = NULL,
            predictors                = base::colnames(np.data),
            impurity                  = NULL,
            min.cell.size.np          = 10,
            min.cell.size.p           = 10,
            min.impurity              = 0.095,
            n.levels.approx.threshold = 10
            ) {

            ############################################
            #          Input Integrity Checks          #
            ############################################

            # test np.data
            base::stopifnot(
                !base::is.null(np.data), # must not be NULL
                base::is.data.frame(np.data) | base::is.matrix(np.data) | tibble::is_tibble(np.data), # must be matrix-like data type
                base::nrow(np.data) > 0 # must not be empty
                );

            # test p.data
            base::stopifnot(
                !base::is.null(p.data), # must not be NULL
                base::is.data.frame(p.data) | base::is.matrix(p.data) | tibble::is_tibble(p.data), # must be matrix-like data type
                base::nrow(p.data) > 0 # must not be empty
                );

            # test sampling.weight
            base::stopifnot(
                !base::is.null(sampling.weight), # must not be NULL
                base::is.character(sampling.weight), # must be a character vector
                (base::length(sampling.weight) == 1), # must be of length 1
                (sampling.weight %in% base::colnames(p.data)), # must be a column name of p.data
                base::is.numeric(p.data[,sampling.weight]), # corresponding column of p.data must contain only numeric data types
                base::all(p.data[,sampling.weight] > 0)  # all numbers in corresponding column must be positive
                );

            # test bootstrap weights
            if ( !base::is.null(bootstrap.weights) ) {
                base::stopifnot(
                    base::is.character(bootstrap.weights), # must be a character vector
                    (base::length(bootstrap.weights) > 1), # must be of length >= 2
                    base::all(bootstrap.weights %in% base::colnames(p.data)),  # must be a subset of column names of p.data
                    base::all(base::as.logical(base::lapply( X = p.data[,bootstrap.weights], FUN = function(x) {return(base::is.numeric(x))} ))), # all bootstrap weight columns must be numeric
                    base::all(p.data[,bootstrap.weights] >= 0)  # all numbers in corresponding column must be non-negative
                    );
                }

            # test predictors
            base::stopifnot(
                !base::is.null(predictors), # must not be NULL
                base::is.character(predictors), # must be a string or set of strings
                base::length(base::setdiff(predictors, base::colnames(np.data))) == 0, # must be contained in column names of np.data
                base::length(base::setdiff(predictors, base::colnames( p.data))) == 0  # must be contained in column names of  p.data
                );

            # test impurity
            base::stopifnot(
                !base::is.null(impurity), # must not be NULL
                base::is.character(impurity), # must be a character vector
                (base::length(impurity) == 1), # must be of length 1
                (impurity %in% base::c('gini','entropy')) # must be either 'gini' or 'entropy'
                );

            # test min.cell.size.np
            base::stopifnot(
                !base::is.null(min.cell.size.np), # must not be NULL
                base::is.numeric(min.cell.size.np), # must be numeric
                (base::length(min.cell.size.np) == 1), # must be lf length 1
                (min.cell.size.np == as.integer(min.cell.size.np)), # must be an integer
                (min.cell.size.np > 0) # must be strictly positive
                );

            # test min.cell.size.p
            base::stopifnot(
                !base::is.null(min.cell.size.p), # must not be NULL
                base::is.numeric(min.cell.size.p), # must be numeric
                (base::length(min.cell.size.p) == 1), # must be of length 1
                (min.cell.size.p == as.integer(min.cell.size.p)), # must be an integer
                (min.cell.size.p > 0) # must be strictly positive
                );

            # test min.impurity
            base::stopifnot(
                !base::is.null(min.impurity), # must not be NULL
                base::is.numeric(min.impurity), # must be numeric
                (base::length(min.impurity) == 1), # must be length 1
                min.impurity > 0 # must be positive
                );

            # test n.levels.approx.threshold
            base::stopifnot(
                !base::is.null(n.levels.approx.threshold), # must not be NULL
                base::is.numeric(n.levels.approx.threshold), # must be numeric
                (base::length(n.levels.approx.threshold) == 1), # must be of length 1
                (n.levels.approx.threshold == as.integer(n.levels.approx.threshold)), # must be an integer
                (n.levels.approx.threshold >= 0) # must be greater than or equal to zero
                );

            private$predictors                <- predictors;
            private$np.data                   <- np.data;
            private$p.data                    <-  p.data;
            private$sampling.weight           <- sampling.weight;
            private$bootstrap.weights         <- bootstrap.weights;
            private$impurity.name             <- impurity;
            private$impurity.function         <- base::ifelse(test = 'gini' == impurity, yes = private$gini, no = private$entropy);
            private$min.cell.size.np          <- min.cell.size.np;
            private$min.cell.size.p           <- min.cell.size.p;
            private$min.impurity              <- min.impurity;
            private$n.levels.approx.threshold <- n.levels.approx.threshold;

            # add synthetic row ID:
            private$np.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            private$np.data[,private$np.syntheticID] <- base::seq(1,base::nrow(private$np.data));

            private$p.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            private$p.data[,private$p.syntheticID] <- base::seq(1,base::nrow(private$p.data));

            # make replica of non-probability and probablity data frames (with synthetic ID)
            private$np.data.original <- private$np.data;
            private$p.data.original  <- private$p.data;

            # convert character predictors to factors.
            for (temp.colname in private$predictors) {
                if (base::is.character(private$np.data[,temp.colname])) {
                    private$np.data[,temp.colname] <- base::as.factor(private$np.data[,temp.colname]);
                    }
                if (base::is.character(private$p.data[,temp.colname])) {
                    private$p.data[,temp.colname]  <- base::as.factor(private$p.data[,temp.colname]);
                    }
                }

            # determining the collection of predictor variables which are ordered factors
            private$predictors_ordered_factor <- private$predictors[base::sapply(X = private$np.data[1,private$predictors], FUN = function(x) { return( base::is.factor(x) & base::is.ordered(x) ) } )]

            # convert ordered factors to numeric values (corresponding to their index in the list of levels)
            for (temp.colname in private$predictors_ordered_factor) {
                private$np.data[,temp.colname] <- base::unlist(base::lapply(X = private$np.data[,temp.colname], FUN = function(x) { return(base::match(x, base::levels(private$np.data[,temp.colname]))) }))
                private$p.data[, temp.colname] <- base::unlist(base::lapply(X = private$p.data[, temp.colname], FUN = function(x) { return(base::match(x, base::levels(private$p.data[, temp.colname]))) }))
                }

            private$predictors_factor  <- private$predictors[base::sapply(X = private$np.data[1,private$predictors], FUN = function(x) { return( base::is.factor(x) & !base::is.ordered(x) ) } )]
            private$predictors_numeric <- private$predictors[base::sapply(X = private$np.data[1,private$predictors], FUN = base::is.numeric)]

            # test if the max number of levels has been exceeded
            # if( base::length(private$predictors_factor) > 0 ) {
            #     base::stopifnot( base::max(base::unlist(base::lapply(X = private$np.data[,private$predictors_factor], FUN = function(x) { return( base::length(base::levels(x)) ) }))) <= private$n.levels.approx.threshold ) # testing factors in np.data
            #     base::stopifnot( base::max(base::unlist(base::lapply(X = private$p.data[, private$predictors_factor], FUN = function(x) { return( base::length(base::levels(x)) ) }))) <= private$n.levels.approx.threshold ) # testing factors in p.data
            # }

            private$estimatedPopulationSize <- base::sum(private$p.data[,private$sampling.weight]);

            }, # initialize = function()

        get_instantiation_data = function() {
            return(list(
                predictors                = private$predictors,
                np.data                   = private$np.data,
                p.data                    = private$p.data,
                sampling.weight           = private$sampling.weight,
                bootstrap.weights         = private$bootstrap.weights,
                impurity                  = private$impurity.name,
                min.cell.size.np          = private$min.cell.size.np,
                min.cell.size.p           = private$min.cell.size.p,
                min.impurity              = private$min.impurity,
                n.levels.approx.threshold = private$n.levels.approx.threshold
                ));
            },

        grow = function() {

            private$nodes <- base::list();
            lastNodeID <- 0;

            workQueue <- base::list(
                private$node$new(
                    parentID  = -1,
                    nodeID    = lastNodeID,
                    depth     = 0,
                    np.rowIDs = private$np.data[,private$np.syntheticID],
                     p.rowIDs =  private$p.data[, private$p.syntheticID],
                    impurity  = private$npp_impurity(
                        np.rowIDs = private$np.data[,private$np.syntheticID],
                         p.rowIDs = private$p.data[, private$p.syntheticID ]
                         )
                    )
                );

            while (0 < base::length(workQueue)) {

                currentNode <- private$pop(workQueue, envir = environment());

                currentNodeID          <- currentNode$nodeID;
                currentParentID        <- currentNode$parentID;
                currentDepth           <- currentNode$depth;
                np.currentRowIDs       <- currentNode$np.rowIDs;
                 p.currentRowIDs       <- currentNode$p.rowIDs;
                currentImpurity        <- currentNode$impurity;
                current_birthCriterion <- currentNode$birthCriterion;

                if (private$stoppingCriterionSatisfied(np.rowIDs = np.currentRowIDs,p.rowIDs = p.currentRowIDs)) {

                    #cat("\n# ~~~~~~~~~~ #")
                    #cat("\ncurrentNodeID\n");
                    #print( currentNodeID   );
                    #np.subset <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,];
                    # p.subset <-  private$p.data[ private$p.data[, private$p.syntheticID] %in%  p.currentRowIDs,];
                    #cat("\nnp.subset\n");
                    #print( np.subset   );
                    #cat("\np.subset\n");
                    #print( p.subset   );
                    #cat("\nnpp_impurity\n");
                    #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                    #cat("# ~~~~~~~~~~ #\n")
                    #print(paste0("stoppingCriterionSatisfied: nodeID - ", currentNodeID))
                    private$nodes <- private$push(
                        list = private$nodes,
                        x    = private$node$new(
                            nodeID    = currentNodeID,
                            parentID  = currentParentID,
                            depth     = currentDepth,
                            np.rowIDs = np.currentRowIDs,
                             p.rowIDs =  p.currentRowIDs,
                            impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                            birthCriterion = current_birthCriterion
                            )
                        );

                } else {

                    bestSplit <- private$get_best_split(
                        np.currentRowIDs = np.currentRowIDs,
                         p.currentRowIDs =  p.currentRowIDs,
                         current.impurity = currentImpurity
                        );

                    if ( base::is.null(bestSplit) ) {
                        #print(paste0("null bestSplit: nodeID - ", currentNodeID))
                        private$nodes <- private$push(
                            list = private$nodes,
                            x = private$node$new(
                                nodeID    = currentNodeID,
                                parentID  = currentParentID,
                                depth     = currentDepth,
                                np.rowIDs = np.currentRowIDs,
                                 p.rowIDs =  p.currentRowIDs,
                                impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                                birthCriterion = current_birthCriterion
                                )
                            )
                    } else {

                        #print(paste0("valid bestSplit: nodeID - ", currentNodeID))

                        np.satisfied <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,private$np.syntheticID][
                            bestSplit$comparison(
                                private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                        p.satisfied <- private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,private$p.syntheticID][
                            bestSplit$comparison(
                                private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                        # adding 2 here to make ordering of nodeID agree with the order of appearance in private$nodes
                        lastNodeID          <- lastNodeID + 2;
                        notSatisfiedChildID <- lastNodeID;
                        workQueue           <- private$push(
                            list = workQueue,
                            x    = private$node$new(
                                parentID  = currentNodeID,
                                nodeID    = lastNodeID,
                                depth     = currentDepth + 1,
                                np.rowIDs = np.notSatisfied,
                                 p.rowIDs =  p.notSatisfied,
                                impurity  = private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs = p.notSatisfied),
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% private$predictors_factor,"!=",">=")
                                    )
                                )
                            );

                        # subtracting 1 here to make ordering of nodeID agree with the order of appearance in private$nodes
                        lastNodeID       <- lastNodeID - 1;
                        satisfiedChildID <- lastNodeID;
                        workQueue        <- private$push(
                            list = workQueue,
                            x    = private$node$new(
                                parentID  = currentNodeID,
                                nodeID    = lastNodeID,
                                depth     = currentDepth + 1,
                                np.rowIDs = np.satisfied,
                                 p.rowIDs =  p.satisfied,
                                impurity  = private$npp_impurity(np.rowIDs = np.satisfied, p.rowIDs = p.satisfied),
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% private$predictors_factor,"=","<")
                                    ),
                                )
                            );
                        # adding 1 here to make ordering of nodeID agree with the order of appearance in private$nodes
                        lastNodeID <- lastNodeID + 1;

                        #cat("\n# ~~~~~~~~~~ #")
                        #cat("\ncurrentNodeID\n");
                        #print( currentNodeID   );
                        #np.subset <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,];
                        # p.subset <-  private$p.data[ private$p.data[, private$p.syntheticID] %in%  p.currentRowIDs,];
                        #cat("\nnp.subset\n");
                        #print( np.subset   );
                        #cat("\np.subset\n");
                        #print( p.subset   );
                        #cat("\nnpp_impurity\n");
                        #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                        #cat("# ~~~~~~~~~~ #\n")

                        private$nodes <- private$push(
                            list = private$nodes,
                            x = private$node$new(
                                nodeID    = currentNodeID,
                                parentID  = currentParentID,
                                depth     = currentDepth,
                                np.rowIDs = np.currentRowIDs,
                                 p.rowIDs =  p.currentRowIDs,
                                impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                                splitCriterion = bestSplit,
                                birthCriterion = current_birthCriterion,
                                satisfiedChildID    =    satisfiedChildID,
                                notSatisfiedChildID = notSatisfiedChildID
                                )
                            );

                        }

                    }
                }
            # private$order_nodes();
            # return( NULL );
            }, # grow = function()

        # predict = function() {
        #     base::warning("This function is currently un-implemented.");
        #     return( NULL );
        #     },

        print = function(
            FUN.format = function(x) { return(x) }
            ) {
            if ( 0 == base::length(private$nodes) ) {
                base::cat("\nlist of nodes is empty.\n")
                }
            else {
                for ( i in base::seq(1,base::length(private$nodes)) ) {
                    private$nodes[[i]]$print_node(FUN.format = FUN.format);
                    }
                base::cat("\n");
                }
            },

        get_nodes = function() {
            if ( base::is.null(private$nodes) ) { self$grow() }
            return( private$nodes );
            },

        get_npdata_with_propensity = function() {
            if ( base::is.null(private$nodes) ) { self$grow() }
            DF.output <- private$private_get_npdata_with_propensity(nodes = private$nodes);
            if ( base::nrow(DF.output) > 0 & !base::is.null(private$bootstrap.weights) ) {
                if ( base::is.null(private$subtree.hierarchy) ) {
                    private$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
                    }
                temp.AICs <- base::sapply(X = private$subtree.hierarchy, FUN = function(x) return(x[['AIC']]));
                index.optimal.subtree <- base::which( temp.AICs == base::min(temp.AICs) );
                # DF.output.pruned <- private$subtree.hierarchy[[index.optimal.subtree]][['npdata_with_propensity']];
                DF.output.pruned <- private$private_get_npdata_with_propensity(
                    nodes = private$subtree.hierarchy[[index.optimal.subtree]][['pruned_nodes']]
                    );
                DF.output.pruned <- DF.output.pruned[,base::c(private$np.syntheticID,'nodeID','propensity','np.count','p.weight','impurity')];
                base::colnames(DF.output.pruned) <- base::sapply(
                    X   = base::colnames(DF.output.pruned),
                    FUN = function(x) { base::return(base::ifelse(x == private$np.syntheticID,x,paste0(x,'.pruned'))) }
                    );
                DF.output <- base::merge(
                    x  = DF.output,
                    y  = DF.output.pruned,
                    by = private$np.syntheticID
                    );
                }
            retained.colnames <- base::c(private$np.syntheticID,base::setdiff(base::colnames(DF.output),base::colnames(private$np.data.original)));
            DF.output <- base::merge(
                x  = private$np.data.original,
                y  = DF.output[,retained.colnames],
                by = private$np.syntheticID
                );
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),private$np.syntheticID)];
            return( DF.output );
            },

        get_pdata_with_nodeID = function() {
            if ( base::is.null(private$nodes) ) { self$grow() }
            DF.output <- private$private_get_pdata_with_nodeID(nodes = private$nodes);
            if ( base::nrow(DF.output) > 0 & !base::is.null(private$bootstrap.weights) ) {
                if ( base::is.null(private$subtree.hierarchy) ) {
                    private$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
                    }
                temp.AICs <- base::sapply(X = private$subtree.hierarchy, FUN = function(x) return(x[['AIC']]));
                index.optimal.subtree <- base::which( temp.AICs == base::min(temp.AICs) );
                DF.output.pruned <- private$private_get_pdata_with_nodeID(
                    nodes = private$subtree.hierarchy[[index.optimal.subtree]][['pruned_nodes']]
                    );
                DF.output.pruned <- DF.output.pruned[,base::c(private$p.syntheticID,'nodeID','propensity','np.count','p.weight','impurity')];
                base::colnames(DF.output.pruned) <- base::sapply(
                    X   = base::colnames(DF.output.pruned),
                    FUN = function(x) { base::return(base::ifelse(x == private$p.syntheticID,x,paste0(x,'.pruned'))) }
                    );
                DF.output <- base::merge(
                    x  = DF.output,
                    y  = DF.output.pruned,
                    by = private$p.syntheticID
                    );
                }
            retained.colnames <- base::c(private$p.syntheticID,base::setdiff(base::colnames(DF.output),base::colnames(private$p.data.original)));
            DF.output <- base::merge(
                x  = private$p.data.original,
                y  = DF.output[,retained.colnames],
                by = private$p.syntheticID
                );
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),private$p.syntheticID)];
            return( DF.output );
            },

        # get_pdata_with_nodeID = function(which.tree = c('fully.grown','optimal')) {
        #     if ( base::is.null(private$subtree.hierarchy) ) {
        #         private$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
        #         }
        #     if ( 'fully.grown' == which.tree ) {
        #         return( private$private_get_pdata_with_nodeID(nodes = private$subtree.hierarchy[[1]][['pruned_nodes']]) );
        #     } else if ( 'optimal' == which.tree ) {
        #         temp.AICs <- base::sapply(X = private$subtree.hierarchy, FUN = function(x) return(x[['AIC']]));
        #         index.optimal.subtree <- base::which( temp.AICs == base::min(temp.AICs) );
        #         return( private$private_get_pdata_with_nodeID(nodes = private$subtree.hierarchy[[index.optimal.subtree]][['pruned_nodes']]) );
        #     } else {
        #         cat("\nInadmissible value for the parameter which.tree: ",which.tree,".\n")
        #         return( NULL );
        #         }
        #     },

        get_subtree_hierarchy = function() {
            if ( base::is.null(private$bootstrap.weights) ) {
                base::warning("No bootstrap weight column names were supplied; NULL is returned for subtree hierarchy.");
                return(NULL);
                }
            if ( base::is.null(private$subtree.hierarchy) ) {
                private$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
                }
            return( private$subtree.hierarchy );
            },

        get_impurities_alphas_AICs = function() {
            if ( base::is.null(private$bootstrap.weights) ) {
                base::warning("No bootstrap weight column names were supplied; NULL is returned for data frame of alphas and AICs.");
                return(NULL);
                }
            if ( base::is.null(private$subtree.hierarchy) ) {
                private$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
                }
            DF.output <- base::data.frame(
                index.subtree = base::seq(1,base::length(private$subtree.hierarchy)),
                tree.impurity = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['tree_impurity']]) }
                    )),
                alpha = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['alpha']]) }
                    )),
                AIC = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['AIC']]) }
                    )),
                p.log.like = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['p.log.like']]) }
                    )),
                n.leaves = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['n.leaves']]) }
                    )),
                bsvar = base::as.numeric(base::sapply(
                    X   = private$subtree.hierarchy,
                    FUN = function(x) { return(x[['bsvar']]) }
                    ))
                );
            return( DF.output );
            },

        public_nodes_to_table = function(nodes = private$nodes) {
            return( private$nodes_to_table(nodes = nodes) );
            },

        public_nprow_to_leafID = function(nodes = private$nodes) {
            return( private$nprow_to_leafID(nodes = nodes) );
            }

        ),

    private = base::list(

        # instantiation data
        predictors                = NULL,
        np.data                   = NULL,
        np.data.original          = NULL,
         p.data                   = NULL,
         p.data.original          = NULL,
        sampling.weight           = NULL,
        bootstrap.weights         = NULL,
        min.cell.size.np          = NULL,
        min.cell.size.p           = NULL,
        min.impurity              = NULL,
        n.levels.approx.threshold = NULL,

        # attributes
        predictors_factor         = NULL,
        predictors_ordered_factor = NULL,
        predictors_numeric        = NULL,

        impurity.name     = NULL,
        impurity.function = NULL,

        nodes = NULL,
        subtree.hierarchy = NULL,

        np.syntheticID = NULL,
         p.syntheticID = NULL,

        estimatedPopulationSize = NULL,

        # methods
        pop = function(list, i = base::length(list), envir = NULL) {
            base::stopifnot(base::inherits(list, "list"))
            if (0 == base::length(list)) { return(NULL); }
            result <- list[[i]];
            base::assign(x = base::deparse(base::substitute(list)), value = list[-i], envir = envir);
            return( result );
            },

        push = function(list, x, i = base::length(list)) {
            base::stopifnot(base::inherits(list, "list"));
            return( base::c(list,list(x)) );
            },

        stoppingCriterionSatisfied = function(np.rowIDs = NULL, p.rowIDs = NULL) {

            if ( base::length(np.rowIDs) < private$min.cell.size.np ) {
                #print("base::length(np.rowIDs) < private$min.cell.size.np")
                return(TRUE);
            }

            if ( base::length(p.rowIDs) < private$min.cell.size.p ) {
                return(TRUE);
            }

            estimatedCellPopulationSize <- base::sum(private$p.data[private$p.data[,private$p.syntheticID] %in%  p.rowIDs,private$sampling.weight]);
            if ( estimatedCellPopulationSize < length(np.rowIDs) ) {
                #print("estimatedCellPopulationSize < length(np.rowIDs)")
                return(TRUE);
            }

            impurity = private$npp_impurity(np.rowIDs = np.rowIDs, p.rowIDs = p.rowIDs);
            if ( impurity < private$min.impurity ) {
                #print("impurity < private$min.impurity")
                return(TRUE);
            }

            return( FALSE);

            },

        get_descendants = function(nodeIDs = NULL, nodeID = NULL) {
            nodes <- private$nodes[base::unlist(base::lapply(
                X   = private$nodes,
                FUN = function(x) { return( x$nodeID %in% nodeIDs) }
                ))];
            temp <- base::unlist(base::lapply(X = nodes, FUN = function(x) { return( nodeID == x$nodeID ) }));
            if (1 == base::sum(temp)) {
                targetNode  <- nodes[temp][[1]];
                descendants <- base::c();
                if (!base::is.null(targetNode$satisfiedChildID)) {
                    descendants <- base::c(
                        descendants,
                        targetNode$satisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$satisfiedChildID)
                        );
                    }
                if (!base::is.null(targetNode$notSatisfiedChildID)) {
                    descendants <- base::c(
                        descendants,
                        targetNode$notSatisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$notSatisfiedChildID)
                        );
                    }
                return( base::sort(descendants) );
            } else {
                return( base::c() );
                }
            },

        nprow_to_leafID = function(nodes = private$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nNodes <- base::length(nodes);
            DF.output <- data.frame(x = numeric(0), y = numeric(0));
            base::colnames(DF.output) <- base::c(private$np.syntheticID,"nodeID");
            for ( i in base::seq(1,nNodes) ) {
                if ( base::is.null(nodes[[i]]$satisfiedChildID) ) {
                    DF.temp <- data.frame(
                        x = nodes[[i]]$np.rowIDs,
                        y = base::rep(x = nodes[[i]]$nodeID, times = base::length(nodes[[i]]$np.rowIDs))
                        );
                    base::colnames(DF.temp) <- base::c(private$np.syntheticID,"nodeID");
                    DF.output <- base::rbind(DF.output,DF.temp);
                    }
                }
            return( DF.output );
            },

        prow_to_leafID = function(nodes = private$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nNodes <- base::length(nodes);
            DF.output <- data.frame(x = numeric(0), y = numeric(0));
            base::colnames(DF.output) <- base::c(private$p.syntheticID,"nodeID");
            for ( i in base::seq(1,nNodes) ) {
                if ( base::is.null(nodes[[i]]$satisfiedChildID) ) {
                    DF.temp <- data.frame(
                        x = nodes[[i]]$p.rowIDs,
                        y = base::rep(x = nodes[[i]]$nodeID, times = base::length(nodes[[i]]$p.rowIDs))
                        );
                    base::colnames(DF.temp) <- base::c(private$p.syntheticID,"nodeID");
                    DF.output <- base::rbind(DF.output,DF.temp);
                    }
                }
            return( DF.output );
            },

        nodes_to_table = function(nodes = private$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nrow.output <- base::length(nodes);
            DF.output <- data.frame(
                nodeID     = numeric(length = nrow.output),
                depth      = numeric(length = nrow.output),
                np.count   = numeric(length = nrow.output),
                p.weight   = numeric(length = nrow.output),
                impurity   = numeric(length = nrow.output),
                propensity = numeric(length = nrow.output),
                #riskWgtd   = numeric(length = nrow.output),
                #riskLeaves = numeric(length = nrow.output),
                #nLeaves    = numeric(length = nrow.output),
                parentID   = numeric(length = nrow.output),
                   satisfiedChildID = numeric(length = nrow.output),
                notSatisfiedChildID = numeric(length = nrow.output)
                );
            totalNumRecords <- base::length(nodes[[1]]$np.rowIDs);
            for ( i in base::seq(1,nrow.output) ) {
                DF.output[i,'nodeID'  ]   <- nodes[[i]]$nodeID;
                DF.output[i,'depth'   ]   <- nodes[[i]]$depth;
                DF.output[i,'np.count']   <- base::length(nodes[[i]]$np.rowIDs);
                DF.output[i,'p.weight']   <- base::sum(private$p.data[private$p.data[,private$p.syntheticID] %in% nodes[[i]]$p.rowIDs,private$sampling.weight]);
                DF.output[i,'impurity']   <- nodes[[i]]$impurity;
                DF.output[i,'propensity'] <- DF.output[i,'np.count'] / DF.output[i,'p.weight'];
                DF.output[i,'riskWgtd']   <- DF.output[i,'impurity'] * DF.output[i,'p.weight'] / private$estimatedPopulationSize;
                DF.output[i,'riskLeaves'] <- 0;
                DF.output[i,'nLeaves']    <- 0;
                DF.output[i,'parentID']   <- nodes[[i]]$parentID;
                DF.output[i,'satisfiedChildID'] <- base::ifelse(
                    base::is.null(nodes[[i]]$satisfiedChildID),
                    NA,
                    nodes[[i]]$satisfiedChildID
                    );
                DF.output[i,'notSatisfiedChildID'] <- base::ifelse(
                    base::is.null(nodes[[i]]$notSatisfiedChildID),
                    NA,
                    nodes[[i]]$notSatisfiedChildID
                    );
                }
            return( DF.output );
            },

        get_best_split = function(np.currentRowIDs,p.currentRowIDs,current.impurity) {
            uniqueVarValuePairs_factor  <- base::list();
            uniqueVarValuePairs_numeric <- base::list();
            if (base::length(private$predictors_factor) > 0) {
                uniqueVarValuePairs_factor <- private$get_uniqueVarValuePairs_factor(
                    np.currentRowIDs = np.currentRowIDs,
                     p.currentRowIDs =  p.currentRowIDs
                    );
                }
            if (base::length(private$predictors_numeric) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = private$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = private$predictors_numeric
                    ));
                if (base::length(temp.list) > 0) {
                    uniqueVarValuePairs_numeric <- private$get_var_value_pairs(
                        x = base::lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_less_than
                        );
                    }
                }
            uniqueVarValuePairs <- base::c(uniqueVarValuePairs_factor,uniqueVarValuePairs_numeric);
            impurity.reductions <- base::lapply(
                X   = uniqueVarValuePairs,
                FUN = function(x) {

                    np.satisfied <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,private$np.syntheticID][
                        x$comparison(
                            private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                    p.satisfied <- private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,private$p.syntheticID][
                        x$comparison(
                            private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                    p1 <- sum(private$p.data[private$p.data[,private$p.syntheticID] %in% p.satisfied,   private$sampling.weight]);
                    p2 <- sum(private$p.data[private$p.data[,private$p.syntheticID] %in% p.notSatisfied,private$sampling.weight]);

                    p1.plus.p2 <- base::sum( c(p1,p2) , na.rm = TRUE );
                    p1 <- p1 / p1.plus.p2;
                    p2 <- p2 / p1.plus.p2;

                    g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);

                    impurity.reduction <- current.impurity - p1 * g1 - p2 * g2;
                    if ( is.na(impurity.reduction) | (impurity.reduction < 1e-99) ) { impurity.reduction <- -Inf; }

                    return( impurity.reduction );
                    }
                );

            # checks for first 3 NULL cases (no best split):
            #   -   uniqueVarValuePairs is empty (no available splits)
            #   -   all impurity.reductions are NA/NaN (no meaningful splits)
            #   -   minimum impurity is Inf (no meaningful splits)
            if (
                base::length(uniqueVarValuePairs) < 1 |
                base::length(impurity.reductions[!base::is.na(impurity.reductions)]) == 0 |
                -Inf == base::max(base::unique(base::as.double(impurity.reductions[!base::is.na(impurity.reductions)])))
                ) { return(NULL); }

            # returns split that corresponds to the maximum impurity reduction (exluding NA values)
            output <- uniqueVarValuePairs[[ base::which.max(impurity.reductions[!base::is.na(impurity.reductions)]) ]];

            check.np.satisfied <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,private$np.syntheticID][
                output$comparison(
                    private$np.data[private$np.data[,private$np.syntheticID] %in% np.currentRowIDs,output$varname],
                    output$threshold
                    )
                ];
            check.np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,check.np.satisfied));

            # checks for last NULL case (no best split):
            #   -   all np.data is satisfied or all np.data is not satisfied
            #       (prevents infinite loop that keeps splitting into same "all or nothing" groups)
            if( length(check.np.satisfied) < 1 | length(check.np.notSatisfied) < 1 ) { return(NULL) }

            return( output );
            },

        get_uniqueVarValuePairs_factor = function(np.currentRowIDs = NULL, p.currentRowIDs = NULL) {

            uniqueVarValuePairs_factor <- base::list();

            DF.non.constant <- private$get_non_constant_columns_factor(
                DF.input       = private$np.data,
                currentRowIDs  = np.currentRowIDs,
                input.colnames = private$predictors_factor
                );

            if ( base::ncol(DF.non.constant) > 0 ) {
                for ( temp.colname in base::colnames(DF.non.constant) ) {

                    DF.table.np <- base::table(DF.non.constant[,temp.colname]);
                    if ( sum(as.integer(DF.table.np) > 0) > private$n.levels.approx.threshold ) {
                        # cat("\nget_uniqueVarValuePairs_factor_DEV0(): approximate\n");
                        DF.table.np <- base::as.data.frame(DF.table.np);
                        base::colnames(DF.table.np) <- base::c(temp.colname,"freq");
                        DF.table.p <- stats::aggregate(
                            x    = stats::as.formula(base::paste0(private$sampling.weight," ~ ",temp.colname)),
                            data = private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,base::c(temp.colname,private$sampling.weight)],
                            FUN  = base::sum
                            );
                        DF.table <- base::merge(
                            x     = DF.table.np,
                            y     = DF.table.p,
                            by    = temp.colname,
                            all.x = TRUE,
                            all.y = FALSE
                            );
                        DF.table[,'prob'] <- DF.table[,'freq'] / DF.table[,private$sampling.weight];
                        DF.table          <- DF.table[!base::is.na(DF.table[,'prob']),];
                        DF.table          <- DF.table[DF.table[,'freq'] > 0,];
                        DF.table[DF.table[,'prob'] > 1,'prob'] <- 1;
                        DF.table    <- DF.table[base::order(DF.table[,'prob']),];
                        temp.labels <- DF.table[,temp.colname];
                        if ( length(temp.labels) > 0 ) {
                            for ( temp.length in base::seq(1,base::max(1,base::length(temp.labels)-1)) ) {
                                uniqueVarValuePairs_factor <- private$push(
                                    list = uniqueVarValuePairs_factor,
                                    x    = private$splitCriterion$new(
                                        varname    = temp.colname,
                                        threshold  = temp.labels[base::seq(1,temp.length)],
                                        comparison = private$is_element_of
                                        )
                                    );
                                }
                            }
                    } else { # if ( sum(as.integer(DF.table.np) > 0) > private$n.levels.approx.threshold )
                        # cat("\nget_uniqueVarValuePairs_factor_DEV0(): exact\n");
                        temp.labels <- base::names(DF.table.np)[DF.table.np > 0];
                        temp.list <- base::list();
                        for ( temp.label in temp.labels ) { temp.list[[ temp.label ]] <- c(TRUE,FALSE); }
                        DF.grid <- base::expand.grid(temp.list);
                        for ( temp.row.index in base::seq(2,base::nrow(DF.grid)-1) ) {
                            uniqueVarValuePairs_factor <- private$push(
                                list = uniqueVarValuePairs_factor,
                                x    = private$splitCriterion$new(
                                    varname    = temp.colname,
                                    threshold  = base::colnames(DF.grid)[base::as.logical(DF.grid[temp.row.index,])],
                                    comparison = private$is_element_of
                                    )
                                );
                            }
                        base::remove("DF.grid");
                        } # if ( sum(as.integer(DF.table.np) > 0) > 6 ) {} else {}

                    } # for ( temp.colname in base::colnames(DF.non.constant) )
                } # if ( base::ncol(DF.non.constant) > 0 )
            return( uniqueVarValuePairs_factor );
            },

        get_non_constant_columns_factor = function(DF.input = NULL, currentRowIDs = NULL, input.colnames = NULL) {
            DF.output             <- as.data.frame(DF.input[DF.input[,private$np.syntheticID] %in% currentRowIDs,input.colnames]);
            colnames(DF.output)   <- input.colnames;
            nUniqueValues         <- apply(X = DF.output, MARGIN = 2, FUN = function(x) { return(length(unique(x))) } );
            is.nonconstant.column <- (nUniqueValues > 1);
            DF.output             <- as.data.frame(DF.output[,is.nonconstant.column]);
            colnames(DF.output)   <- input.colnames[is.nonconstant.column];
            return( DF.output );
            },

        get_non_constant_columns = function(DF.input = NULL, currentRowIDs = NULL, input.colnames = NULL) {
            DF.output <- DF.input[DF.input[,private$np.syntheticID] %in% currentRowIDs,input.colnames];

            # If length(input.colnames) == 1, then DF.output will be a vector.
            # In that case, cast DF.output into a data frame.
            DF.output <- base::as.data.frame(DF.output);

            base::colnames(DF.output) <- input.colnames;
            nUniqueValues             <- base::apply(X = DF.output, MARGIN = 2, FUN = function(x) { return(base::length(base::unique(x))) } );
            DF.output                 <- base::as.data.frame(DF.output[,nUniqueValues > 1]);
            base::colnames(DF.output) <- input.colnames[nUniqueValues > 1];

            return( DF.output );
            },

        get_midpoints = function(x) {
            if (base::is.numeric(x)) {
                y <- base::sort(base::unique(x));
                return( base::apply(X=data.frame(c1=y[2:base::length(y)],c2=y[1:(base::length(y)-1)]),MARGIN=1,FUN=base::mean) );
                }
            else if (base::is.factor(x) & !base::is.ordered(x)) {
                return( private$enumerate_set(base::levels(x)) );
                }
            else {
                return( base::sort(base::unique(x)) );
                }
            },

        # @param: set - a vector or list, containing factors
        # @return: combinationsA - a list of vectors containing 1st group of all possible splits
        # Enumerates all possible ways to split a given set of factors into 2 groups
        # (combinationsA and combinationsB are lists corresponding to each group caused by splitting;
        #  we only need to return 1 group, since we are checking if the factor belongs to the group or not)
        enumerate_set = function(set = NULL) {

            combinationsA <- base::list()
            #combinationsB <- base::list()
            indexA <- 1
            #indexB <- 1

            # iterate from 1 to half the length of the set (rounded down if odd),
            # where i corresponds to the group size created by combn
            # (this will cover all possible combinations, since we look at the group
            #  returned by combn and the remaining members of the set not in the group)
            for (i in base::seq(1, base::trunc(base::length(set) / 2))) {

                # combn returns a dataframe containing all possible combinations;
                # ncol corresponds to the number of possible combinations
                size <- base::ncol(utils::combn(set, i))

                # special even case: checks if group size i is equal to half the length of the set
                if ( i == base::length(set) / 2) {
                    matrix <- utils::combn(set, i)

                    # iterate through half of the combinations and store them all
                    # (again, this will cover all possible combinations, since we look at what's
                    #  in the group and what's not in the group)
                    for (j in base::seq(1, size / 2)) {
                        combinationsA[[indexA]] <- matrix[, j]
                        #combinationsB[[indexB]] <- matrix[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
                # standard case
                else {
                    matrixA <- utils::combn(set, i)
                    #matrixB <- combn(set, length(set) - i)

                    # iterate through all the combinations and store them all
                    # (here, we are looking at what's in the group of size i, and what's
                    #  not in the group, which has size = length(set) - i)
                    for (j in base::seq(1, size)) {
                        combinationsA[[indexA]] <- matrixA[, j]
                        #combinationsB[[indexB]] <- matrixB[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
            }

            return (combinationsA);
            },

        get_var_value_pairs = function(x = NULL, comparison = NULL) {
            names_x  <- base::names(x);
            templist <- base::list();
            for (i in base::seq(1,base::length(names_x))) {
                for (j in base::seq(1,base::length(x[[i]]))) {
                    templist <- private$push(
                        list = templist,
                        x    = private$splitCriterion$new(
                            varname    = names_x[i],
                            threshold  = x[[i]][j],
                            comparison = comparison
                            )
                        );
                    }
                }
            return( templist );
            },

        is_element_of = function(x,y) {
            return(base::is.element(x,y))
            },

        is_less_than = function(x,y) {
            return(x < y)
            },

        # @param: x - a singular variable, y - a singular variable or vector
        # @return: bool (TRUE or FALSE)
        # Checks if x is contained in y: useful for checking if factor is in contained in split (combination of factors),
        # but also works for checking numerics and ordered factors (i.e. checking if x == y)
        is_equal_to = function(x,y) {
            ret <- base::lapply(
                X   = x,
                FUN = function(z) {
                    for(element in y) { if(element == z) { return(TRUE) } }
                    return(FALSE);
                    }
                );
            return(base::unlist(ret));
            },

        # impurity = function(x) {
        #     # Gini impurity
        #     p <- base::as.double(base::table(x) / base::length(x));
        #     return( base::sum(p * (1 - p)) );
        #     },

        gini    = function(p = 0.5) { return( 2 * p * (1 - p)                                 ) },
        entropy = function(p = 0.5) { return( - p * base::log(p) - (1 - p) * base::log(1 - p) ) },

        npp_impurity = function(np.rowIDs,p.rowIDs) {
            np.subset <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.rowIDs,];
             p.subset <-  private$p.data[ private$p.data[, private$p.syntheticID] %in%  p.rowIDs,];

            if ( base::nrow(np.subset) < private$min.cell.size.np ) { return(Inf); }
            if ( base::nrow( p.subset) < private$min.cell.size.p  ) { return(Inf); }

            estimatedPopulationSize <- base::sum(p.subset[,private$sampling.weight]);
            if ( 0 == estimatedPopulationSize ) { return( Inf ); }

            p <- base::nrow(np.subset) / estimatedPopulationSize;
            if ( p < 1e-199 ) {
                return(0);
            } else if ( 1 < p ) {
                return(Inf);
                }

            # impurity <- 2 * p * (1 - p);
            impurity <- private$impurity.function(p = p);
            return( impurity );
            },

        splitCriterion = R6::R6Class(
            classname  = "splitCriterion",
            public = base::list(
                varname    = NULL,
                threshold  = NULL,
                comparison = NULL,
                initialize = function(varname = NULL, threshold = NULL, comparison = NULL) {
                    self$varname    = varname;
                    self$threshold  = threshold;
                    self$comparison = comparison;
                    }
                )
            ),

        birthCriterion = R6::R6Class(
            classname  = "birthCriterion",
            public = base::list(
                varname    = NULL,
                threshold  = NULL,
                comparison = NULL,
                initialize = function(varname = NULL, threshold = NULL, comparison = NULL) {
                    self$varname    = varname;
                    self$threshold  = threshold;
                    self$comparison = comparison;
                    }
                )
            ),

        order_nodes = function() {
            if (base::length(private$nodes) > 0) {
                nodeIDs <- base::as.integer(base::lapply(X = private$nodes, FUN = function(x) { return( x$nodeID ); } ))
                private$nodes <- private$nodes[base::order(nodeIDs)];
                return( NULL );
                }
            },

        generate_subtree_hierarchy = function(DF.nodes = private$nodes_to_table()) {
            index.subtree <- 1;
            list.subtrees <- base::list();
            list.subtrees[[index.subtree]] <- base::list(
                alpha           = 0,
                DF_compute_g    = NULL,
                nodes_untouched = DF.nodes[,"nodeID"],
                nodes_pruned_at = c(),
                nodes_removed   = c(),
                DF_pruned_at    = NULL,
                DF_retained     = DF.nodes
                );
            DF.temp <- DF.nodes;
            while ( base::nrow(DF.temp) > 1 ) {
                index.subtree <- index.subtree + 1;
                DF.compute.g  <- private$compute_g(DF.input = DF.temp);
                list.temp     <- private$prune_g_minimizers(DF.input = DF.compute.g);
                DF.temp       <- list.temp[['DF_retained']];
                list.subtrees[[index.subtree]] <- base::list(
                    alpha           = list.temp[['alpha']],
                    DF_compute_g    = DF.compute.g,
                    nodes_untouched = list.temp[['nodes_untouched']],
                    nodes_pruned_at = list.temp[['nodes_pruned_at']],
                    nodes_removed   = list.temp[['nodes_removed']],
                    DF_pruned_at    = list.temp[['DF_pruned_at']],
                    DF_retained     = list.temp[['DF_retained']]
                    );
                }
            base::remove(list = c('DF.temp'));
            base::gc();
            ##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #####
            index.subtree <- 1;
            list.subtrees[[index.subtree]][['pruned_nodes' ]] <- private$duplicate_nodes(input.nodes = private$nodes);
            list.subtrees[[index.subtree]][['tree_impurity']] <- private$compute_tree_impurity(
                DF.retained = list.subtrees[[index.subtree]][['DF_retained']]
                );
            # list.subtrees[[index.subtree]][['npdata_with_propensity']] <- private$private_get_npdata_with_propensity(
            #     nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
            #     );
            DF.npdata.with.propensity <- private$private_get_npdata_with_propensity(
                nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                );
            # list.subtrees[[index.subtree]][['pdata_with_nodeID']] <- private$private_get_pdata_with_nodeID(
            #     nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
            #     );
            DF.pdata.with.nodeID <- private$private_get_pdata_with_nodeID(
                nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                );
            results.compute_AIC <- private$compute_AIC(
                DF.retained.nodes         = list.subtrees[[index.subtree]][['DF_retained']],
                DF.npdata.with.propensity = DF.npdata.with.propensity, # list.subtrees[[index.subtree]][['npdata_with_propensity']],
                DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
                sampling.weight.varname   = private$sampling.weight,
                replicate.weight.varnames = private$bootstrap.weights,
                combined.weights          = FALSE # TRUE
                );
            list.subtrees[[index.subtree]][['AIC'       ]] <- results.compute_AIC[['AIC'       ]];
            list.subtrees[[index.subtree]][['p.log.like']] <- results.compute_AIC[['p.log.like']];
            list.subtrees[[index.subtree]][['n.leaves'  ]] <- results.compute_AIC[['n.leaves'  ]];
            list.subtrees[[index.subtree]][['bsvar'     ]] <- results.compute_AIC[['bsvar'     ]];
            base::remove(list = c('DF.npdata.with.propensity','DF.pdata.with.nodeID','results.compute_AIC'));
            base::gc();
            ##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #####
            for ( index.subtree in seq(2,length(list.subtrees)) ) {
                list.subtrees[[index.subtree]][['pruned_nodes']] <- private$get_pruned_nodes(
                    # input.nodes  = list.subtrees[[index.subtree - 1]][['pruned_nodes']],
                    # pruning.info = list.subtrees[[index.subtree    ]]
                    input.nodes  = private$duplicate_nodes(input.nodes = list.subtrees[[index.subtree - 1]][['pruned_nodes']]),
                    pruning.info = list.subtrees[[index.subtree]]
                    );
                list.subtrees[[index.subtree]][['tree_impurity']] <- private$compute_tree_impurity(
                    DF.retained = list.subtrees[[index.subtree]][['DF_retained']]
                    );
                # list.subtrees[[index.subtree]][['npdata_with_propensity']] <- private$private_get_npdata_with_propensity(
                #     nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                #     );
                DF.npdata.with.propensity <- private$private_get_npdata_with_propensity(
                    nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                    );
                # list.subtrees[[index.subtree]][['pdata_with_nodeID']] <- private$private_get_pdata_with_nodeID(
                #     nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                #     );
                DF.pdata.with.nodeID <- private$private_get_pdata_with_nodeID(
                    nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                    );
                results.compute_AIC <- private$compute_AIC(
                    DF.retained.nodes         = list.subtrees[[index.subtree]][['DF_retained']],
                    DF.npdata.with.propensity = DF.npdata.with.propensity, # list.subtrees[[index.subtree]][['npdata_with_propensity']],
                    DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
                    sampling.weight.varname   = private$sampling.weight,
                    replicate.weight.varnames = private$bootstrap.weights,
                    combined.weights          = FALSE # TRUE
                    );
                list.subtrees[[index.subtree]][['AIC'       ]] <- results.compute_AIC[['AIC'       ]];
                list.subtrees[[index.subtree]][['p.log.like']] <- results.compute_AIC[['p.log.like']];
                list.subtrees[[index.subtree]][['n.leaves'  ]] <- results.compute_AIC[['n.leaves'  ]];
                list.subtrees[[index.subtree]][['bsvar'     ]] <- results.compute_AIC[['bsvar'     ]];
                base::remove(list = c('DF.npdata.with.propensity','DF.pdata.with.nodeID','results.compute_AIC'));
                base::gc();
                }
            return( list.subtrees );
            },

        duplicate_nodes = function(input.nodes = base::list()) {
            output.nodes <- base::list();
            if ( base::length(input.nodes) > 0 ) {
                for ( index.element in base::seq(1,base::length(input.nodes)) ) {
                    output.nodes[[index.element]] <- private$node$new(
                        nodeID    = input.nodes[[index.element]]$nodeID,
                        parentID  = input.nodes[[index.element]]$parentID,
                        depth     = input.nodes[[index.element]]$depth,
                        np.rowIDs = input.nodes[[index.element]]$np.rowIDs,
                         p.rowIDs = input.nodes[[index.element]]$p.rowIDs,
                        impurity  = input.nodes[[index.element]]$impurity,
                        # splitCriterion = input.nodes[[index.element]]$splitCriterion,
                        splitCriterion = private$splitCriterion$new(
                            varname    = input.nodes[[index.element]]$splitCriterion$varname,
                            threshold  = input.nodes[[index.element]]$splitCriterion$threshold,
                            comparison = input.nodes[[index.element]]$splitCriterion$comparison
                            ),
                        # birthCriterion = input.nodes[[index.element]]$birthCriterion,
                        birthCriterion = private$splitCriterion$new(
                            varname    = input.nodes[[index.element]]$birthCriterion$varname,
                            threshold  = input.nodes[[index.element]]$birthCriterion$threshold,
                            comparison = input.nodes[[index.element]]$birthCriterion$comparison
                            ),
                        satisfiedChildID    = input.nodes[[index.element]]$satisfiedChildID,
                        notSatisfiedChildID = input.nodes[[index.element]]$notSatisfiedChildID
                        );
                    }
                }
            return( output.nodes );
            },

        get_pruned_nodes = function(
            input.nodes  = NULL,
            pruning.info = NULL
            ) {
            nodes_untouched <- pruning.info[['nodes_untouched']];
            nodes.pruned.at <- pruning.info[['nodes_pruned_at']];
            nodes_removed   <- pruning.info[['nodes_removed'  ]];
            output.nodes <- base::list();
            for ( i in 1:length(input.nodes) ) {
                temp.node <- input.nodes[[i]];
                if ( temp.node[['nodeID']] %in% nodes_removed ) {
                    # do nothing
                } else if ( temp.node[['nodeID']] %in% nodes_untouched ) {
                    output.nodes[[1+length(output.nodes)]] <- temp.node;
                } else if ( temp.node[['nodeID']] %in% nodes.pruned.at ) {
                    temp.node[['notSatisfiedChildID']]     <- NULL;
                    temp.node[[   'satisfiedChildID']]     <- NULL;
                    temp.node[[     'splitCriterion']]     <- NULL;
                    output.nodes[[1+length(output.nodes)]] <- temp.node;
                    }
                }
            return( output.nodes );
            },

        prune_g_minimizers = function(
            DF.input  = NULL,
            tolerance = 1e-9
            ) {
            DF.output      <- DF.input;
            min.CART.g     <- base::min(DF.output[,'nppCART.g'], na.rm = TRUE);
            is.g.minimizer <- (base::abs(x = DF.output[,'nppCART.g'] - min.CART.g) < tolerance);
            g.minimizers   <- base::setdiff(DF.output[is.g.minimizer,'nodeID'],NA);
            nodes.removed  <- base::c();
            for ( g.minimizer in g.minimizers ) {
                nodes.removed <- base::unique(base::c(nodes.removed,private$get_descendant_nodeIDs(DF.input = DF.input, nodeID = g.minimizer)));
                is.selected <- (DF.output[,'nodeID'] == g.minimizer);
                DF.output[is.selected,'riskLeaves'] <- DF.output[is.selected,'riskWgtd'];
                DF.output[is.selected,'nLeaves']    <- 1;
                DF.output[is.selected,c('satisfiedChildID','notSatisfiedChildID')] <- NA;
                }
            # DF.output <- DF.output[!(DF.output[,'nodeID'] %in% nodes.removed),];
            list.output <- base::list(
                alpha           = min.CART.g,
                nodes_untouched = base::setdiff(DF.input[,'nodeID'],c(g.minimizers,nodes.removed)),
                nodes_pruned_at = g.minimizers,
                nodes_removed   = nodes.removed,
                DF_pruned_at    = DF.input[   DF.input[, 'nodeID'] %in% g.minimizers,  ],
                DF_retained     = DF.output[!(DF.output[,'nodeID'] %in% nodes.removed),]
                );
            return( list.output );
            },

        compute_g = function(
            DF.input = NULL
            ) {
            DF.output <- DF.input;
            DF.output[,'riskLeaves'] <- 0;
            DF.output[,'nLeaves'   ] <- 0;
            is.leaf <- base::is.na(DF.output[,'satisfiedChildID']);
            DF.output[is.leaf,'riskLeaves'] <- DF.output[is.leaf,'riskWgtd'];
            DF.output[is.leaf,'nLeaves']    <- 1;
            for ( temp.depth in base::seq(base::max(DF.output[,'depth']),1) ) {
                DF.temp <- DF.output[DF.output[,'depth'] == temp.depth,];
                for ( i in base::seq(1,base::nrow(DF.temp)) ) {
                    temp.parentID <- DF.temp[i,'parentID'];
                    is.parent <- (DF.output[,'nodeID'] == temp.parentID);
                    DF.output[is.parent,'riskLeaves'] <- DF.output[is.parent,'riskLeaves'] + DF.temp[i,'riskLeaves'];
                    DF.output[is.parent,'nLeaves'   ] <- DF.output[is.parent,'nLeaves'   ] + DF.temp[i,'nLeaves'   ];
                    }
                }
            DF.output[,'nppCART.g'] <- (DF.output[,'riskWgtd'] - DF.output[,'riskLeaves']) / (DF.output[,'nLeaves'] - 1);
            return(DF.output);
            },

        compute_tree_impurity = function(DF.retained = NULL) {
            DF.temp <- DF.retained[base::is.na(DF.retained[,'satisfiedChildID']),];
            tree.impurity <- base::sum(DF.temp[,'p.weight'] * DF.temp[,'impurity']) / base::sum(DF.temp[,'p.weight']);
            return(tree.impurity);
            },

        compute_AIC = function(
            DF.retained.nodes         = NULL,
            DF.npdata.with.propensity = NULL,
            DF.pdata.with.nodeID      = NULL,
            sampling.weight.varname   = NULL,
            replicate.weight.varnames = NULL,
            combined.weights          = FALSE
            ) {

            # thisFunctionName <- "compute_AIC";

            # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
            # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.leaves <- DF.retained.nodes[base::is.na(DF.retained.nodes['satisfiedChildID']),];
            DF.leaves[,'likelihood.summand'] <- base::apply(
                X      = DF.leaves[,base::c('p.weight','propensity')],
                MARGIN = 1,
                FUN    = function(x) { return( x[1] * ( x[2]*base::log(x[2]) + (1-x[2]) * base::log(1-x[2]) ) ) }
                );
            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.var_hat <- stats::aggregate(
                x   = DF.pdata.with.nodeID[,replicate.weight.varnames],
                by  = list(DF.pdata.with.nodeID[,"nodeID"]),
                FUN = sum
                );
            base::colnames(DF.var_hat) <- base::gsub(x = base::colnames(DF.var_hat), pattern = '^Group\\.1$', replacement = "nodeID");

            DF.var_hat[,'var_hat'] <- base::apply(
                X      = DF.var_hat[,replicate.weight.varnames],
                MARGIN = 1,
                FUN    = var
                );

            DF.var_hat <- DF.var_hat[,c("nodeID",'var_hat')];

            # base::cat("\nstr(DF.var_hat)\n");
            # base::print( str(DF.var_hat)   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            DF.leaves <- base::merge(
                x     = DF.leaves,
                # y   = template.results.svyby,
                y     = DF.var_hat,
                by    = "nodeID",
                all.x = TRUE,
                sort  = TRUE
                );

            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            DF.leaves[,'trace.summand'] <- base::apply(
                X      = DF.leaves[,c('p.weight','propensity','var_hat')],
                MARGIN = 1,
                FUN    = function(x) { return( x[2] * x[3] / (x[1]*(1-x[2])) ) }
                );

            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            likelihood.estimate <- base::sum(DF.leaves[,'likelihood.summand']);
            trace.term          <- base::sum(DF.leaves[,'trace.summand']);
            output.AIC          <- 2 * (base::nrow(DF.leaves) + trace.term - likelihood.estimate);

            # base::cat("\nlikelihood.estimate\n");
            # base::print( likelihood.estimate   );

            # base::cat("\ntrace.term\n");
            # base::print( trace.term   );

            # base::cat("\noutput.AIC\n");
            # base::print( output.AIC   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            list.output <- base::list(
                AIC        = output.AIC,
                p.log.like = likelihood.estimate,
                n.leaves   = base::nrow(DF.leaves),
                bsvar      = trace.term
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            base::remove(list = c(
                'DF.leaves','DF.var_hat',
                # 'temp.colnames','bsw.colnames',
                'likelihood.estimate','trace.term','output.AIC'
                ));
            base::gc();
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            # base::cat(base::paste0("\n# ",thisFunctionName,"() quits."));
            # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
            base::return( list.output );

            },

        get_descendant_nodeIDs = function(
            DF.input = NULL,
            nodeID   = NULL
            ) {
            descendant.nodeIDs <- base::numeric();
            offspring.nodeIDs  <- base::setdiff(base::as.numeric(DF.input[DF.input[,'nodeID'] == nodeID,base::c('satisfiedChildID','notSatisfiedChildID')]),NA);
            while ( base::length(offspring.nodeIDs) > 0 ) {
                descendant.nodeIDs <- c(descendant.nodeIDs,offspring.nodeIDs);
                offspring.nodeIDs  <- DF.input[DF.input[,'nodeID'] %in% offspring.nodeIDs,c('satisfiedChildID','notSatisfiedChildID')];
                offspring.nodeIDs  <- base::as.matrix(x = offspring.nodeIDs);
                offspring.nodeIDs  <- base::as.numeric(base::matrix(data = offspring.nodeIDs, nrow = 1));
                offspring.nodeIDs  <- setdiff(offspring.nodeIDs,NA);
                }
            return( descendant.nodeIDs );
            },

        private_get_npdata_with_propensity = function(nodes = private$nodes) {

            DF.leaf_table <- private$nodes_to_table(nodes = nodes);
            DF.leaf_table <- DF.leaf_table[base::is.na(DF.leaf_table[,'satisfiedChildID']),];
            # cat("\nDF.leaf_table\n");
            # print( DF.leaf_table   );

            DF.nprow_to_leaf <- private$nprow_to_leafID(nodes = nodes);
            # cat("\nDF.nprow_to_leaf\n");
            # print( DF.nprow_to_leaf   );

            DF.nprow_to_leaf <- base::merge(
                x  = DF.nprow_to_leaf,
                y  = DF.leaf_table[,base::c("nodeID","propensity","np.count","p.weight","impurity")],
                by = "nodeID"
                );
            # cat("\nDF.nprow_to_leaf\n");
            # print( DF.nprow_to_leaf   );

            DF.output <- base::merge(
                x  = private$np.data,
                y  = DF.nprow_to_leaf,
                by = private$np.syntheticID
                );
            # cat("\nDF.output\n");
            # print( DF.output   );

            return( DF.output );
            },

        private_get_pdata_with_nodeID = function(nodes = private$nodes) {

            DF.leaf_table <- private$nodes_to_table(nodes = nodes);
            DF.leaf_table <- DF.leaf_table[base::is.na(DF.leaf_table[,'satisfiedChildID']),];
            #cat("\nDF.leaf_table\n");
            #print( DF.leaf_table   );

            DF.prow_to_leaf <- private$prow_to_leafID(nodes = nodes);
            #cat("\nDF.prow_to_leaf\n");
            #print( DF.prow_to_leaf   );

            DF.prow_to_leaf <- base::merge(
                x  = DF.prow_to_leaf,
                y  = DF.leaf_table[,base::c("nodeID","propensity","np.count","p.weight","impurity")],
                by = "nodeID"
                );
            #cat("\nDF.prow_to_leaf\n");
            #print( DF.prow_to_leaf   );

            DF.output <- base::merge(
                x  = private$p.data,
                y  = DF.prow_to_leaf,
                by = private$p.syntheticID
                );
            #DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),private$p.syntheticID)];
            #cat("\nDF.output\n");
            #print( DF.output   );

            return( DF.output );
            },

        node = R6::R6Class(
            classname = "node",

            public = base::list(

                nodeID    = NULL,
                parentID  = NULL,
                depth     = NULL,
                np.rowIDs = NULL,
                 p.rowIDs = NULL,
                impurity  = NULL,

                splitCriterion = NULL,
                birthCriterion = NULL,

                satisfiedChildID    = NULL,
                notSatisfiedChildID = NULL,

                initialize = function(
                    nodeID    = NULL,
                    parentID  = NULL,
                    depth     = NULL,
                    np.rowIDs = NULL,
                     p.rowIDs = NULL,
                    impurity  = NULL,

                    splitCriterion = NULL,
                    birthCriterion = NULL,

                    satisfiedChildID    = NULL,
                    notSatisfiedChildID = NULL
                    ) {
                        self$nodeID    <- nodeID;
                        self$parentID  <- parentID;
                        self$depth     <- depth;
                        self$np.rowIDs <- np.rowIDs;
                        self$p.rowIDs  <-  p.rowIDs;
                        self$impurity  <- impurity;

                        self$splitCriterion <- splitCriterion;
                        self$birthCriterion <- birthCriterion;

                        self$satisfiedChildID    <- satisfiedChildID;
                        self$notSatisfiedChildID <- notSatisfiedChildID;
                    },

                print_node = function(
                    indent     = '  ',
                    FUN.format = function(x) { return(x) }
                    ) {
                    base::cat("\n");
                    base::cat(base::paste0(base::rep(indent,self$depth),collapse="") );
                    base::cat(base::paste0("(",self$nodeID,") "));
                    if (0 == self$nodeID) {
                        base::cat("[root]");
                        }
                    else {
                        base::cat(base::paste0("[",
                            self$birthCriterion$varname,   " ",
                            self$birthCriterion$comparison," ",
                            #FUN.format(self$birthCriterion$threshold),
                            self$birthCriterion$threshold,
                            "]"));
                        }
                    base::cat(base::paste0(", impurity = ",FUN.format(self$impurity)));
                    base::cat(base::paste0(", np.count = ",FUN.format(base::length(self$np.rowIDs))));
                    base::cat(base::paste0(", p.count = ", FUN.format(base::length(self$p.rowIDs))));
                    }
                )

            )

        )

    );
