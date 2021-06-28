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
#' @import dplyr
#' @import survey
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
#' @param np.data This parameter corresponds to the non-probability sample. The input must be a nonempty matrix-like data type (i.e. matrix, dataframe or tibble). A value must be specified here for initialization to be successful.
#' @param p.data This parameter corresponds to the probability sample. The input must be a nonempty matrix-like data type (i.e. matrix, dataframe or tibble). A value must be specified here for initialization to be successful.
#' @param sampling.weight This parameter corresponds to the column in the probability sample that contains the sampling weights. The input must be a string corresponding to a column name in p.data, such that there are only positive numbers in that column. A value must be specified here for initialization to be successful.
#' @param bootstrap.weights This parameter corresponds to the columns in the probability sample that contains the bootstrap weights. The input must be a character vector corresponding to a subset of column names in p.data, such that there are only non-negative numbers in these columns.
#' @param predictors This parameter corresponds to the auxillary variables on which the partitioning is performed. The input must be a string or vector of strings that contain column names shared by both np.data and p.data. If no value is specified, predictors will be set to all the column names in np.data.
#' @param min.cell.size This parameter corresponds to the minimum number of rows remaining in the probability sample and non-probabilty sample to continue partitioning. The input must be a positive integer. If no value is specified, min.cell.size will be set to 10.
#' @param min.impurity This parameter corresponds to the minimum impurity calculated in each leaf node to continue partitioning. The input must be a positive number. If no value is specified, min.impurity will be set to 0.095.
#' @param n.levels.approx.threshold For each node and for each categorical (i.e. non-ordered factor) predictor variable, if the number of levels of the given categorical predictor variable is less than or equal to n.levels.approx.threshold, then all possible splits by the given categorical variable are considered; otherwse (i.e. if the number of levels of the given categorical predictor variable in the given node strictly exceeds n.levels.approx.threshold), then an approximate procedure is used. The input must be an integer greater than or equal to zero. If no value is specified, n.levels.approx.threshold is set to 10.
#'
#' @field predictors_factor This field contains the inputted predictors that are unordered factors. It is used for internal calcuations.
#' @field predictors_ordered_factor This field contains the inputted predictors that are ordered factors. It is used for internal calcuations.
#' @field predictors_numeric This field contains the inputted predictors that are numeric. It is used for internal calcuations.
#' @field nodes This field contains the nodes of the tree generated by the grow method. It is used for internal calcuations.
#' @field np.syntheticID This field contains synthetic unique identifiers for the data in the non-probability sample. It is used for internal calcuations.
#' @field p.syntheticID This field contains synthetic unique identifiers for the data in the probability sample. It is used for internal calcuations.
#' @field estimatedPopulationSize This field contains the sum of all weights in the probablity sample. It is used for internal calcuations.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize(predictors, np.data, p.data, sampling.weight, bootstrap.weights, min.cell.size, min.impurity)}}{This method is called when the R6 class is created (i.e. when nppCART is called). The arguments passed into nppCART are passed into initialize. This method contains input integrity checks to ensure that the arguments meet the required specifications. In addition, the method does some preprocessing of the input data.}
#'  \item{\code{get_instantiation_data()}}{This method is used to retrieve the instantiation data.}
#'  \item{\code{grow()}}{This method is used to grow a classification tree through recursive binary partitioning of the predictors. It operates in the R6 class internally, and does not have parameters or a return value. This method should be called after the initialization of the class.}
#'  \item{\code{get_npdata_with_propensity(nodes)}}{This method returns a dataframe that contains the non-probability sample, with the tree-calculated values. The tree-calculated values include: the unique identifier for each node (called nodeID); the self-selection propensity for each member in the non-probability sample (called propensity); the number of members in the non-probability sample, which belong to each node (called np.count); the sum of the members’ sampling weights in the probability sample, which belong to each node (called p.weight); and the tree impurity of each node (called impurity). There is one parameter, nodes, which is passed in a value internally by default, and should not be modified. This method should be used after calling grow.}
#'  \item{\code{print()}}{This method is used to print the classification tree in a readable format (each node is on a separate line and indented appropriately). There is one parameter, FUN.format, which is a function that customizes the output format. This method should be used after calling grow.}
#' }
#'
#' @export

nppCART <- function(
    np.data                   = NULL,
    p.data                    = NULL,
    sampling.weight           = NULL,
    bootstrap.weights         = NULL,
    predictors                = base::setdiff(base::colnames(p.data),c(weight,bootstrap.weights)),
    min.cell.size             = 10,
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
            min.cell.size             = min.cell.size,
            min.impurity              = min.impurity,
            n.levels.approx.threshold = n.levels.approx.threshold
            )
        );
    }

R6_nppCART <- R6::R6Class(
    classname = "R6_nppCART",

    public = list(

        initialize = function(
            np.data                   = NULL,
            p.data                    = NULL,
            sampling.weight           = NULL,
            bootstrap.weights         = NULL,
            predictors                = base::colnames(np.data),
            min.cell.size             = 10,
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
                base::is.character(sampling.weight) & (base::length(sampling.weight) == 1), # must be a single string
                base::length(base::setdiff(sampling.weight,base::colnames(p.data))) == 0,  # must correspond to a column name of p.data
                base::is.numeric(p.data[,sampling.weight]), # corresponding column of p.data must contain only numeric data types
                base::all(p.data[,sampling.weight] > 0)  # all numbers in corresponding column must be positive
                );

            # test bootstrap weights
            if ( !base::is.null(bootstrap.weights) ) {
                base::stopifnot(
                    base::is.character(bootstrap.weights) & (base::length(bootstrap.weights) > 1), # must be a character vector of length >= 2
                    base::length(base::setdiff(bootstrap.weights,base::colnames(p.data))) == 0,  # must be a subset of column names of p.data
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

            # test min.cell.size
            base::stopifnot(
                !base::is.null(min.cell.size), # must not be NULL
                base::is.numeric(min.cell.size) & (base::length(min.cell.size) == 1), # must be single number
                (min.cell.size %% 1 == 0) & (min.cell.size > 0) # must be a positive integer
                );

            # test min.impurity
            base::stopifnot(
                !base::is.null(min.impurity), # must not be NULL
                base::is.numeric(min.impurity) & (base::length(min.impurity) == 1), # must be single number
                min.impurity > 0 # must be positive
                );

            # test n.levels.approx.threshold
            base::stopifnot(
                !base::is.null(n.levels.approx.threshold), # must not be NULL
                base::is.numeric(n.levels.approx.threshold) & (base::length(n.levels.approx.threshold) == 1), # must be single number
                (n.levels.approx.threshold == as.integer(n.levels.approx.threshold)) & (n.levels.approx.threshold > -1) # must be an integer greater than or equal to zero
                );

            private$predictors                <- predictors;
            private$np.data                   <- np.data;
            private$p.data                    <-  p.data;
            private$sampling.weight           <- sampling.weight;
            private$bootstrap.weights         <- bootstrap.weights;
            private$min.cell.size             <- min.cell.size;
            private$min.impurity              <- min.impurity;
            private$n.levels.approx.threshold <- n.levels.approx.threshold;

            # add synthetic row ID:
            private$np.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            private$np.data[,private$np.syntheticID] <- base::seq(1,base::nrow(private$np.data));

            private$p.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            private$p.data[,private$p.syntheticID] <- base::seq(1,base::nrow(private$p.data));

            # make replica of non-probability data frame (with synthetic ID)
            private$np.data.original <- private$np.data;

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
                min.cell.size             = private$min.cell.size,
                min.impurity              = private$min.impurity,
                n.levels.approx.threshold = private$n.levels.approx.threshold
                ));
            },

        grow = function() {

            private$nodes <- list();
            lastNodeID <- 0;

            workQueue <- list(
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
                DF.output.pruned <- private$subtree.hierarchy[[index.optimal.subtree]][['npdata_with_propensity']];
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

    private = list(

        # instantiation data
        predictors                = NULL,
        np.data                   = NULL,
        np.data.original          = NULL,
         p.data                   = NULL,
        sampling.weight           = NULL,
        bootstrap.weights         = NULL,
        min.cell.size             = NULL,
        min.impurity              = NULL,
        n.levels.approx.threshold = NULL,

        # attributes
        predictors_factor         = NULL,
        predictors_ordered_factor = NULL,
        predictors_numeric        = NULL,

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

            if ( base::length(np.rowIDs) < private$min.cell.size ) {
                #print("base::length(np.rowIDs) < private$min.cell.size")
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
            uniqueVarValuePairs_factor  <- list();
            uniqueVarValuePairs_numeric <- list();
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

                    ##########
                    ##########
                    # p1 <- base::length(   np.satisfied) / private$estimatedPopulationSize;
                    # p2 <- base::length(np.notSatisfied) / private$estimatedPopulationSize;
                    # g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    # g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);
                    # #print(paste0("threshold: ", x$threshold, ", p1: ", p1, ", g1: ", g1, ", p2: ", p2, ", g2: ", g2, ", impurity: ", p1 * g1 + p2 * g2))
                    # return( p1 * g1 + p2 * g2 );
                    ##########
                    ##########
                    p1 <- sum(private$p.data[private$p.data[,private$p.syntheticID] %in% p.satisfied,   private$sampling.weight]);
                    p2 <- sum(private$p.data[private$p.data[,private$p.syntheticID] %in% p.notSatisfied,private$sampling.weight]);
                    p1 <- p1 / private$estimatedPopulationSize;
                    p2 <- p2 / private$estimatedPopulationSize;
                    g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);
                    impurity.reduction <- current.impurity - p1 * g1 - p2 * g2;
                    # cat("\ncurrent.impurity\n");   print( current.impurity   );
                    # cat("\np1 * g1 + p2 * g2\n");  print( p1 * g1 + p2 * g2  );
                    # cat("\nimpurity.reduction\n"); print( impurity.reduction );
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
                            formula = stats::as.formula(base::paste0(private$sampling.weight," ~ ",temp.colname)),
                            data    = private$p.data[private$p.data[,private$p.syntheticID] %in% p.currentRowIDs,base::c(temp.colname,private$sampling.weight)],
                            FUN     = base::sum
                            );
                        DF.table <- base::merge(
                            x     = DF.table.np,
                            y     = DF.table.p,
                            by    = temp.colname,
                            all.x = TRUE,
                            all.y = FALSE
                            );
                        DF.table[,'prob'] <- DF.table[,'freq'] / DF.table[,private$sampling.weight];
                        DF.table          <- DF.table[DF.table[,'freq'] > 0,];
                        DF.table[DF.table[,'prob'] > 1,'prob'] <- 1;
                        DF.table    <- DF.table[base::order(DF.table[,'prob']),];
                        temp.labels <- DF.table[,temp.colname];
                        for ( temp.length in base::seq(1,base::length(temp.labels)-1) ) {
                            uniqueVarValuePairs_factor <- private$push(
                                list = uniqueVarValuePairs_factor,
                                x    = private$splitCriterion$new(
                                    varname    = temp.colname,
                                    threshold  = temp.labels[base::seq(1,temp.length)],
                                    comparison = private$is_element_of
                                    )
                                );
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

            combinationsA <- list()
            #combinationsB <- list()
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
            templist <- list();
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

        impurity = function(x) {
            # Gini impurity
            p <- base::as.double(base::table(x) / base::length(x));
            return( base::sum(p * (1 - p)) );
            },

        npp_impurity = function(np.rowIDs,p.rowIDs) {
            np.subset <- private$np.data[private$np.data[,private$np.syntheticID] %in% np.rowIDs,];
             p.subset <-  private$p.data[ private$p.data[, private$p.syntheticID] %in%  p.rowIDs,];

            if ( base::nrow(np.subset) < private$min.cell.size ) { return(Inf); }
            if ( base::nrow( p.subset) < private$min.cell.size ) { return(Inf); }

            estimatedPopulationSize <- base::sum(p.subset[,private$sampling.weight]);
            if ( 0 == estimatedPopulationSize ) { return( Inf ); }

            p <- base::nrow(np.subset) / estimatedPopulationSize;
            if ( 1 < p ) { return(Inf); }

            impurity <- 2 * p * (1 - p);
            return( impurity );
            },

        splitCriterion = R6::R6Class(
            classname  = "splitCriterion",
            public = list(
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
            public = list(
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
            ##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #####
            index.subtree <- 1;
            list.subtrees[[index.subtree]][['pruned_nodes']] <- private$duplicate_nodes(input.nodes = private$nodes);
            list.subtrees[[index.subtree]][['npdata_with_propensity']] <- private$private_get_npdata_with_propensity(
                nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                );
            list.subtrees[[index.subtree]][['tree_impurity']] <- private$compute_tree_impurity(
                DF.retained = list.subtrees[[index.subtree]][['DF_retained']]
                );
            DF.pdata.with.nodeID <- private$private_get_pdata_with_nodeID(
                nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                );
            list.subtrees[[index.subtree]][['AIC']] <- private$compute_AIC(
                DF.retained.nodes         = list.subtrees[[index.subtree]][['DF_retained']],
                DF.npdata.with.propensity = list.subtrees[[index.subtree]][['npdata_with_propensity']],
                DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
                sampling.weight.varname   = private$sampling.weight,
                replicate.weight.varnames = private$bootstrap.weights,
                combined.weights          = FALSE # TRUE
                );
            ##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~ #####
            for ( index.subtree in seq(2,length(list.subtrees)) ) {
                list.subtrees[[index.subtree]][['pruned_nodes']] <- private$get_pruned_nodes(
                    # input.nodes  = list.subtrees[[index.subtree - 1]][['pruned_nodes']],
                    # pruning.info = list.subtrees[[index.subtree    ]]
                    input.nodes  = private$duplicate_nodes(input.nodes = list.subtrees[[index.subtree - 1]][['pruned_nodes']]),
                    pruning.info = list.subtrees[[index.subtree]]
                    );
                list.subtrees[[index.subtree]][['npdata_with_propensity']] <- private$private_get_npdata_with_propensity(
                    nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                    );
                list.subtrees[[index.subtree]][['tree_impurity']] <- private$compute_tree_impurity(
                    DF.retained = list.subtrees[[index.subtree]][['DF_retained']]
                    );
                DF.pdata.with.nodeID <- private$private_get_pdata_with_nodeID(
                    nodes = list.subtrees[[index.subtree]][['pruned_nodes']]
                    );
                list.subtrees[[index.subtree]][['AIC']] <- private$compute_AIC(
                    DF.retained.nodes         = list.subtrees[[index.subtree]][['DF_retained']],
                    DF.npdata.with.propensity = list.subtrees[[index.subtree]][['npdata_with_propensity']],
                    DF.pdata.with.nodeID      = DF.pdata.with.nodeID,
                    sampling.weight.varname   = private$sampling.weight,
                    replicate.weight.varnames = private$bootstrap.weights,
                    combined.weights          = FALSE # TRUE
                    );
                }
            return( list.subtrees );
            },

        duplicate_nodes = function(input.nodes = base::list()) {
            output.nodes <- list();
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
            output.nodes <- list();
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
            #
            # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
            # base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            base::require(survey);

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
            DF.template <- DF.pdata.with.nodeID;

            temp.colnames <- base::colnames(DF.template);
            temp.colnames[temp.colnames %in% replicate.weight.varnames] <- base::paste0("internal.repweight",base::seq(1,base::sum(temp.colnames %in% replicate.weight.varnames)))
            base::colnames(DF.template) <- temp.colnames;

            DF.template[,'dummy.one'] <- 1;

            template.svrepdesign <- survey::svrepdesign(
                data             = DF.template,
                weights          = stats::as.formula(base::paste0("~ ",sampling.weight.varname)),
                type             = "bootstrap",
                repweights       = "internal.repweight[0-9]+",
                combined.weights = combined.weights
                );
            # base::cat("\nstr(template.svrepdesign)\n");
            # base::print( str(template.svrepdesign)   );

            template.results.svyby <- survey::svyby(
                design  = template.svrepdesign,
                formula = as.formula("~ dummy.one"),
                by      = as.formula("~ nodeID"),
                FUN     = svytotal, # svymean # svyvar
                vartype = "var" # c("se","ci","ci","cv","cvpct","var")
                );
            # base::cat("\nstr(template.results.svyby)\n");
            # base::print( str(template.results.svyby)   );

            # base::cat("\ntemplate.results.svyby\n");
            # base::print( template.results.svyby   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            DF.leaves <- base::merge(
                x     = DF.leaves,
                y     = template.results.svyby,
                by    = "nodeID",
                all.x = TRUE,
                sort  = TRUE
                );

            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            DF.leaves[,'trace.summand'] <- base::apply(
                X      = DF.leaves[,c('p.weight','propensity','var')],
                MARGIN = 1,
                FUN    = function(x) { return( x[2] * x[3] / (x[1]*(1-x[2])) ) }
                );

            # base::cat("\nDF.leaves\n");
            # base::print( DF.leaves   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            likelihood.estimate <- base::sum(DF.leaves[,'likelihood.summand']);
            # base::cat("\nlikelihood.estimate\n");
            # base::print( likelihood.estimate   );

            trace.term <- base::sum(DF.leaves[,'trace.summand']);
            # base::cat("\ntrace.term\n");
            # base::print( trace.term   );

            output.AIC <- 2 * (base::nrow(DF.leaves) + trace.term - likelihood.estimate);
            # base::cat("\noutput.AIC\n");
            # base::print( output.AIC   );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            # base::cat(base::paste0("\n# ",thisFunctionName,"() quits."));
            # base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
            base::return( output.AIC );

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
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),private$p.syntheticID)];
            #cat("\nDF.output\n");
            #print( DF.output   );

            return( DF.output );
            },

        node = R6::R6Class(
            classname = "node",

            public = list(

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
