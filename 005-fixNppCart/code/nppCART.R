#' zzz
#' 
#' @docType class
#'
#' @import R6
#' @import dplyr
#' @export
#'
#' @keywords data
#'
#' @return zzz
#'
#' @format \code{\link{R6Class}} object
#'
#' @examples
### zzz ###
#'
#' @field predictors zzz
#' @field np.data zzz
#' @field p.data zzz
#' @field weight zzz
#' @field min.cell.size zzz
#' @field min.impurity zzz
#' @field predictors_factor zzz
#' @field predictors_ordered_factor zzz
#' @field predictors_numeric zzz
#' @field nodes zzz
#' @field np.syntheticID zzz
#' @field p.syntheticID zzz
#' @field estimatedPopulationSize zzz
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{initialize(predictors, np.data, p.data, weight, min.cell.size, min.impurity)}}{zzz}
#'  \item{\code{grow()}}{zzz}
#'  \item{\code{predict()}}{zzz}
#'  \item{\code{get_npdata_with_propensity(nodes)}}{zzz}
#'  \item{\code{print()}}{zzz}
#'  \item{\code{get_pruning_sequence(nodes)}}{zzz}
#' }

nppCART <- function() {
    R6_nppCART$new()
}

base::require(R6);
base::require(dplyr);

R6_nppCART <- R6Class(
    classname = "R6_nppCART",

    public = list(

        predictors    = NULL,
        np.data       = NULL,
         p.data       = NULL,
        weight        = NULL,
        min.cell.size = NULL,
        min.impurity  = NULL,
        max.factors   = NULL,

        predictors_factor  = NULL,
        predictors_ordered_factor  = NULL,
        predictors_numeric = NULL,

        nodes = NULL,

        np.syntheticID = NULL,
         p.syntheticID = NULL,

        estimatedPopulationSize = NULL,

        initialize = function(predictors = base::colnames(np.data), np.data, p.data, weight, min.cell.size = 10, min.impurity = 0.095, max.factors = 10) {
            
            #test np.data
            stopifnot(  !is.null(np.data), 
                        is.data.frame(np.data) | is.matrix(np.data) | tibble::is_tibble(np.data), 
                        nrow(np.data) > 0  ) 
            
            #test p.data
            stopifnot(  !is.null(p.data), 
                        is.data.frame(p.data) | is.matrix(p.data) | tibble::is_tibble(p.data), 
                        nrow(p.data) > 0  )
            
            #test predictors
            stopifnot(  !is.null(predictors), 
                        is.character(predictors), 
                        length(setdiff(predictors, colnames(np.data))) == 0, 
                        length(setdiff(predictors, colnames(p.data))) == 0  )
            
            #test weight
            stopifnot(  !is.null(weight), 
                        is.character(weight) & (length(weight) == 1), 
                        length(setdiff(weight,colnames(p.data))) == 0,  
                        is.numeric(p.data$weight), 
                        all(p.data$weight > 0)  )
            
            #test min.cell.size
            stopifnot(  !is.null(min.cell.size), 
                        is.numeric(min.cell.size) & (length(min.cell.size) == 1), 
                        (min.cell.size %% 1 == 0) & (min.cell.size > 0)  )
            
            #test min.impurity
            stopifnot(  !is.null(min.impurity), 
                        is.numeric(min.impurity) & (length(min.impurity) == 1), 
                        min.impurity > 0  )

            #test max.factors
            stopifnot(  !is.null(max.factors), 
                        is.numeric(max.factors) & (length(max.factors) == 1), 
                        (max.factors %% 1 == 0) & (max.factors >= 0)  )

            self$predictors    <- predictors;
            self$np.data       <- np.data;
            self$p.data        <-  p.data;
            self$weight        <- weight;
            self$min.cell.size <- min.cell.size;
            self$min.impurity  <- min.impurity;
            self$max.factors   <- max.factors

            for (temp.colname in self$predictors) {
                if (base::is.character(self$data[,temp.colname])) {
                    self$data[,temp.colname] <- base::as.factor(self$data[,temp.colname]);
                    }
                }

            self$predictors_factor          <- self$predictors[base::sapply(X = np.data[1,self$predictors], FUN = function(x) { return( base::is.factor(x) & !base::is.ordered(x) ) } )]
            self$predictors_ordered_factor  <- self$predictors[base::sapply(X = np.data[1,self$predictors], FUN = function(x) { return( base::is.factor(x) & base::is.ordered(x) ) } )]
            self$predictors_numeric         <- self$predictors[base::sapply(X = np.data[1,self$predictors], FUN = base::is.numeric)]

            #test length(self$predictors_factor)
            stopifnot(  length(self$predictors_factor) <= self$max.factors  )

            # add custom row ID:
            self$np.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            self$np.data[,self$np.syntheticID] <- base::seq(1,base::nrow(self$np.data));

            self$p.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            self$p.data[,self$p.syntheticID] <- base::seq(1,base::nrow(self$p.data));

            self$estimatedPopulationSize <- base::sum(self$p.data[,self$weight]);

            },

        grow = function() {

            self$nodes <- list();
            lastNodeID <- 0;

            workQueue <- list(
                private$node$new(
                    parentID  = -1,
                    nodeID    = lastNodeID,
                    depth     = 0,
                    np.rowIDs = self$np.data[,self$np.syntheticID],
                     p.rowIDs =  self$p.data[, self$p.syntheticID]
                    )
                );

            while (0 < base::length(workQueue)) {

                currentNode <- private$pop(workQueue, envir = environment());

                currentNodeID          <- currentNode$nodeID;
                currentParentID        <- currentNode$parentID;
                currentDepth           <- currentNode$depth;
                np.currentRowIDs       <- currentNode$np.rowIDs;
                 p.currentRowIDs       <- currentNode$p.rowIDs;
                current_birthCriterion <- currentNode$birthCriterion;

                if (private$stoppingCriterionSatisfied(np.rowIDs = np.currentRowIDs,p.rowIDs = p.currentRowIDs)) {

                    #cat("\n# ~~~~~~~~~~ #")
                    #cat("\ncurrentNodeID\n");
                    #print( currentNodeID   );
                    #np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,];
                    # p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.currentRowIDs,];
                    #cat("\nnp.subset\n");
                    #print( np.subset   );
                    #cat("\np.subset\n");
                    #print( p.subset   );
                    #cat("\nnpp_impurity\n");
                    #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                    #cat("# ~~~~~~~~~~ #\n")
                    print(paste0("stoppingCriterionSatisfied: nodeID - ", currentNodeID))
                    self$nodes <- private$push(
                        list = self$nodes,
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
                    }
                else {

                    bestSplit <- private$get_best_split(
                        np.currentRowIDs = np.currentRowIDs,
                         p.currentRowIDs =  p.currentRowIDs
                        );

                    if ( base::is.null(bestSplit) ) {
                    	print(paste0("null bestSplit: nodeID - ", currentNodeID))
                        self$nodes <- private$push(
                            list = self$nodes,
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
                        }
                    else {

                    	print(paste0("valid bestSplit: nodeID - ", currentNodeID))

                        np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                            bestSplit$comparison(
                                self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                        p.satisfied <- self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,self$p.syntheticID][
                            bestSplit$comparison(
                                self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                        # adding 2 here to make ordering of nodeID agree with the order of appearance in self$nodes
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
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% self$predictors_factor,"!=",">=")
                                    )
                                )
                            );

                        # subtracting 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
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
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% self$predictors_factor,"=","<")
                                    ),
                                )
                            );
                        # adding 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
                        lastNodeID <- lastNodeID + 1;

                        #cat("\n# ~~~~~~~~~~ #")
                        #cat("\ncurrentNodeID\n");
                        #print( currentNodeID   );
                        #np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,];
                        # p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.currentRowIDs,];
                        #cat("\nnp.subset\n");
                        #print( np.subset   );
                        #cat("\np.subset\n");
                        #print( p.subset   );
                        #cat("\nnpp_impurity\n");
                        #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                        #cat("# ~~~~~~~~~~ #\n")

                        self$nodes <- private$push(
                            list = self$nodes,
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
                    print(base::length(workQueue))
                } 
            # private$order_nodes();
            # return( NULL );
            },

        predict = function() {
            return( NULL );
            },

        get_npdata_with_propensity = function(nodes = self$nodes) {

            DF.leaf_table <- private$nodes_to_table(nodes = nodes);
            DF.leaf_table <- DF.leaf_table[base::is.na(DF.leaf_table[,'satisfiedChildID']),];
            #cat("\nDF.leaf_table\n");
            #print( DF.leaf_table   );

            DF.nprow_to_leaf <- private$nprow_to_leafID(nodes = nodes);
            #cat("\nDF.nprow_to_leaf\n");
            #print( DF.nprow_to_leaf   );


            DF.nprow_to_leaf <- base::merge(
                x  = DF.nprow_to_leaf,
                y  = DF.leaf_table[,base::c("nodeID","propensity","np.count","p.weight","impurity")],
                by = "nodeID"
                );
            #cat("\nDF.nprow_to_leaf\n");
            #print( DF.nprow_to_leaf   );

            DF.output <- base::merge(
                x  = self$np.data,
                y  = DF.nprow_to_leaf,
                by = self$np.syntheticID
                );
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),self$np.syntheticID)];
            #cat("\nDF.output\n");
            #print( DF.output   );

            return( DF.output );
            },

        print = function(
            FUN.format = function(x) { return(x) }
            ) {
            if ( 0 == base::length(self$nodes) ) {
                base::cat("\nlist of nodes is empty.\n")
                }
            else {
                for ( i in base::seq(1,base::length(self$nodes)) ) {
                    self$nodes[[i]]$print_node(FUN.format = FUN.format);
                    }
                base::cat("\n");
                }
            },

        get_pruning_sequence = function(nodes = self$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }

            DF.node_table <- private$nodes_to_table(nodes = nodes);

            alpha_subtree <- list(
                alpha                = 0,
                nodes_retained       = DF.node_table[,"nodeID"],
                nodes_pruned         = base::c(),
                nodes_removed        = base::c(),
                nodes_retained_table = DF.node_table
                );

            output_list <- list();
            nAlphas     <- 1;

            output_list[[nAlphas]] <- alpha_subtree;
            while (1 < base::length(alpha_subtree$nodes_retained)) {
                nAlphas       <- 1 + nAlphas;
                alpha_subtree <- private$get_alpha_subtree(DF.input = alpha_subtree$nodes_retained_table);
                output_list[[nAlphas]] <- alpha_subtree;
                }

            return( output_list );
            }
        ),

    private = list(
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

            if ( base::length(np.rowIDs) < self$min.cell.size ) { 
            	print("base::length(np.rowIDs) < self$min.cell.size")
            	return(TRUE); 
            }

            estimatedCellPopulationSize <- base::sum(self$p.data[self$p.data[,self$p.syntheticID] %in%  p.rowIDs,self$weight]);
            if ( estimatedCellPopulationSize < length(np.rowIDs) ) { 
            	print("estimatedCellPopulationSize < length(np.rowIDs)")
            	return(TRUE); 
            }

            impurity = private$npp_impurity(np.rowIDs = np.rowIDs, p.rowIDs = p.rowIDs);
            if ( impurity < self$min.impurity ) {
    	        print("impurity < self$min.impurity") 
            	return(TRUE); 
            }
 
            return( FALSE);

            },
        get_descendants = function(nodeIDs = NULL, nodeID = NULL) {
            nodes <- self$nodes[base::unlist(base::lapply(
                X   = self$nodes,
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
        nprow_to_leafID = function(nodes = self$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nNodes <- base::length(nodes);
            DF.output <- data.frame(x = numeric(0), y = numeric(0));
            base::colnames(DF.output) <- base::c(self$np.syntheticID,"nodeID");
            for ( i in base::seq(1,nNodes) ) {
                if ( base::is.null(nodes[[i]]$satisfiedChildID) ) {
                    DF.temp <- data.frame(
                        x = nodes[[i]]$np.rowIDs,
                        y = base::rep(x = nodes[[i]]$nodeID, times = base::length(nodes[[i]]$np.rowIDs))
                        );
                    base::colnames(DF.temp) <- base::c(self$np.syntheticID,"nodeID");
                    DF.output <- base::rbind(DF.output,DF.temp);
                    }
                }
            return( DF.output );
            },
        nodes_to_table = function(nodes = self$nodes) {
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
                DF.output[i,'p.weight']   <- base::sum(self$p.data[self$p.data[,self$p.syntheticID] %in% nodes[[i]]$p.rowIDs,self$weight]);
                DF.output[i,'impurity']   <- nodes[[i]]$impurity;
                DF.output[i,'propensity'] <- DF.output[i,'np.count'] / DF.output[i,'p.weight'];
                #DF.output[i,'riskWgtd']   <- DF.output[i,'risk'] * DF.output[i,'prop'];
                #DF.output[i,'riskLeaves'] <- 0;
                #DF.output[i,'nLeaves']    <- 0;
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
        get_alpha_subtree = function(DF.input = NULL) {

            hasNoChildren <- base::is.na(DF.input[,'satisfiedChildID']) & base::is.na(DF.input[,'notSatisfiedChildID']);
            DF.leaves <- DF.input[hasNoChildren,];
            DF.leaves <- DF.leaves %>%
                dplyr::rename( riskWgtd_child = riskWgtd ) %>%
                dplyr::rename( nLeaves_child  = nLeaves  ) %>%
                dplyr::select( - base::c( riskLeaves , satisfiedChildID , notSatisfiedChildID ) ) %>%
                dplyr::mutate( nLeaves_child = 1 )
                ;

            retainedColumns <- base::c("nodeID","riskWgtd_child","nLeaves_child");
            DF.internalNodes <- DF.input[!hasNoChildren,] %>%
                dplyr::left_join(DF.leaves[,retainedColumns], by = base::c(   "satisfiedChildID" = "nodeID")) %>%
                dplyr::rename( riskWgtd_s  = riskWgtd_child ) %>%
                dplyr::rename( nLeaves_s   = nLeaves_child  ) %>%

                dplyr::left_join(DF.leaves[,retainedColumns], by = base::c("notSatisfiedChildID" = "nodeID")) %>%
                dplyr::rename( riskWgtd_ns = riskWgtd_child ) %>%
                dplyr::rename( nLeaves_ns  = nLeaves_child  ) %>%

                dplyr::mutate( riskWgtd_s  = base::ifelse(base::is.na(riskWgtd_s ),0,riskWgtd_s ) ) %>%
                dplyr::mutate( riskWgtd_ns = base::ifelse(base::is.na(riskWgtd_ns),0,riskWgtd_ns) ) %>%
                dplyr::mutate( riskLeaves  = riskLeaves + riskWgtd_s + riskWgtd_ns )    %>%

                dplyr::mutate( nLeaves_s   = base::ifelse(base::is.na(nLeaves_s  ),0,nLeaves_s  ) ) %>%
                dplyr::mutate( nLeaves_ns  = base::ifelse(base::is.na(nLeaves_ns ),0,nLeaves_ns ) ) %>%
                dplyr::mutate( nLeaves     = nLeaves + nLeaves_s + nLeaves_ns         ) %>%

                dplyr::select( - base::c(riskWgtd_s,riskWgtd_ns,nLeaves_s,nLeaves_ns) )
                ;

            retainedColumns <- base::c("nodeID","riskLeaves_child","nLeaves_child");
            for (temp.depth in base::seq(base::max(DF.internalNodes[,"depth"]),1,-1)) {
                DF.depth <- DF.internalNodes %>%
                    dplyr::filter( depth == temp.depth ) %>%
                    dplyr::rename( riskLeaves_child = riskLeaves ) %>%
                    dplyr::rename( nLeaves_child    = nLeaves    ) %>%
                    dplyr::select( - base::c( satisfiedChildID , notSatisfiedChildID ) )
                    ;
                DF.internalNodes <- DF.internalNodes %>%
                    dplyr::left_join(DF.depth[,retainedColumns], by = base::c(   "satisfiedChildID" = "nodeID")) %>%
                    dplyr::rename( riskLeaves_s  = riskLeaves_child ) %>%
                    dplyr::rename( nLeaves_s     = nLeaves_child    ) %>%

                    dplyr::left_join(DF.depth[,retainedColumns], by = base::c("notSatisfiedChildID" = "nodeID")) %>%
                    dplyr::rename( riskLeaves_ns = riskLeaves_child ) %>%
                    dplyr::rename( nLeaves_ns    = nLeaves_child    ) %>%

                    dplyr::mutate( riskLeaves_s  = base::ifelse(base::is.na(riskLeaves_s ),0,riskLeaves_s ) ) %>%
                    dplyr::mutate( riskLeaves_ns = base::ifelse(base::is.na(riskLeaves_ns),0,riskLeaves_ns) ) %>%
                    dplyr::mutate( riskLeaves    = riskLeaves + riskLeaves_s + riskLeaves_ns    ) %>%

                    dplyr::mutate( nLeaves_s     = base::ifelse(base::is.na(nLeaves_s    ),0,nLeaves_s    ) ) %>%
                    dplyr::mutate( nLeaves_ns    = base::ifelse(base::is.na(nLeaves_ns   ),0,nLeaves_ns   ) ) %>%
                    dplyr::mutate( nLeaves       = nLeaves + nLeaves_s + nLeaves_ns             ) %>%

                    dplyr::select( - base::c(riskLeaves_s,riskLeaves_ns,nLeaves_s,nLeaves_ns) )
                    ;
                }
            DF.internalNodes <- DF.internalNodes %>%
                dplyr::mutate( alpha = (risk - riskLeaves) / (nLeaves - 1) );

            min.alpha    <- base::min(DF.internalNodes[,'alpha']);
            nodesToPrune <- DF.internalNodes[DF.internalNodes[,'alpha'] == min.alpha,'nodeID'];

            nodesToRemove <- base::c();
            for (tempNodeID in nodesToPrune) {
                nodesToRemove <- base::unique(c(
                    nodesToRemove,
                    private$get_descendants(nodeIDs = DF.input[,'nodeID'], nodeID = tempNodeID)
                    ));
                }
            nodesToRemove <- base::intersect( base::sort(nodesToRemove) , DF.input[,'nodeID'] );

            nodesToRetain <- base::setdiff(DF.input[,'nodeID'],nodesToRemove);

            DF.output <- DF.input[DF.input[,'nodeID'] %in% nodesToRetain,];
            DF.output[DF.input[,'nodeID'] %in% nodesToPrune,   'satisfiedChildID'] <- NA;
            DF.output[DF.input[,'nodeID'] %in% nodesToPrune,'notSatisfiedChildID'] <- NA;

            output_list <- list(
                alpha = min.alpha,
                nodes_retained       = nodesToRetain,
                nodes_pruned         = nodesToPrune,
                nodes_removed        = nodesToRemove,
                nodes_retained_table = DF.output
                );

            return( output_list );
            },
        get_best_split = function(np.currentRowIDs,p.currentRowIDs) {
            uniqueVarValuePairs_factor          <- list();
            uniqueVarValuePairs_ordered_factor  <- list();
            uniqueVarValuePairs_numeric         <- list();
            if (base::length(self$predictors_factor) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = self$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = self$predictors_factor
                    ));
                if (base::length(temp.list) > 0) {
                    uniqueVarValuePairs_factor <- private$get_var_value_pairs(
                        x = base::lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_equal_to
                        );
                    }
                }
            if (base::length(self$predictors_ordered_factor) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = self$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = self$predictors_ordered_factor
                    ));
                if (base::length(temp.list) > 0) {
                    uniqueVarValuePairs_ordered_factor <- private$get_var_value_pairs(
                        x = base::lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_less_than
                        );
                    }
                }
            if (base::length(self$predictors_numeric) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = self$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = self$predictors_numeric
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
            uniqueVarValuePairs <- base::c(uniqueVarValuePairs_factor,uniqueVarValuePairs_ordered_factor,uniqueVarValuePairs_numeric);
            #print(self$np.data)
            #print(self$p.data)
            impurities <- base::lapply(
                X   = uniqueVarValuePairs,
                FUN = function(x) {

                    np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                        x$comparison(
                            self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                    p.satisfied <- self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,self$p.syntheticID][
                        x$comparison(
                            self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                    p1 <- base::length(   np.satisfied) / self$estimatedPopulationSize;
                    p2 <- base::length(np.notSatisfied) / self$estimatedPopulationSize;
                    g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);
                    #print(paste0("threshold: ", x$threshold, ", p1: ", p1, ", g1: ", g1, ", p2: ", p2, ", g2: ", g2, ", impurity: ", p1 * g1 + p2 * g2))
                    return( p1 * g1 + p2 * g2 );
                    }
                );

            if (    length(uniqueVarValuePairs) < 1 | 
                    all(lapply(X = impurities, FUN = function(x) { return( is.nan(x) ) })) | 
                    Inf == base::min(base::unique(base::as.numeric(impurities[!base::is.na(impurities)])))  ) 
                { return(NULL); }
            
            output <- uniqueVarValuePairs[[ base::which.min(impurities[!base::is.na(impurities)]) ]];
            
            check.np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                        output$comparison(
                            self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,output$varname],
                            output$threshold
                            )
                        ];
            check.np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,check.np.satisfied));

            if( length(check.np.satisfied) < 1 |  length(check.np.notSatisfied) < 1 ) { return(NULL) }

            return( output );
            },
        get_non_constant_columns = function(DF.input = NULL, currentRowIDs = NULL, input.colnames = NULL) {
            DF.output <- DF.input[DF.input[,self$np.syntheticID] %in% currentRowIDs,input.colnames];

            # If length(input.colnames) == 1, then DF.output will be a vector.
            # In that case, cast DF.output into a data frame.
            DF.output <- base::as.data.frame(DF.output);
            
            base::colnames(DF.output) <- input.colnames;
            nUniqueValues       <- base::apply(X = DF.output, MARGIN = 2, FUN = function(x) { return(base::length(base::unique(x))) } );
            DF.output           <- base::as.data.frame(DF.output[,nUniqueValues > 1]);
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
                #return( base::sort(base::unique(x)) );
                return( base::levels(x) );
                }
            },
        enumerate_set = function(set = NULL) {
            combinationsA <- list()
            #combinationsB <- list()
            indexA <- 1
            #indexB <- 1

            for (i in seq(1, trunc(length(set) / 2))) {
                size <- ncol(combn(set, i))

                if ( i == length(set) / 2) {
                    matrix <- combn(set, i)

                    for (j in seq(1, size / 2)) {
                        combinationsA[[indexA]] <- matrix[, j]
                        #combinationsB[[indexB]] <- matrix[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
                else {
                    matrixA <- combn(set, i)
                    #matrixB <- combn(set, length(set) - i)

                    for (j in seq(1, size)) {
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
        is_less_than = function(x,y) {
            return(x < y)
            },
        is_equal_to = function(x,y) {
            ret <- lapply(  X = x, 
                            FUN = function(x) {
                                for(element in y) {
                                    if(element == x) {
                                        return(TRUE)
                                    }
                                }
                                return(FALSE)
                                
                                #print(any(ifelse(input == y, TRUE, FALSE)))
                                #return(any(ifelse(input == y, TRUE, FALSE)))
            
                                #print(x %in% y)
                                #return(x %in% y)
                            })
            #print(x %in% y)
            #return(x %in% y)

            return(unlist(ret))
            },
        impurity = function(x) {
            # Gini impurity
            p <- base::as.numeric(base::table(x) / base::length(x));
            return( base::sum(p * (1 - p)) );
            },
        npp_impurity = function(np.rowIDs,p.rowIDs) {
            np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.rowIDs,];
             p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.rowIDs,];
            
            #print(self$min.cell.size)
            #print(base::sum(p.subset[,self$weight]))
            #print(1 < base::nrow(np.subset) / base::sum(p.subset[,self$weight]))

            if ( base::nrow(np.subset) < self$min.cell.size ) { return(Inf); }
            if ( base::nrow( p.subset) < self$min.cell.size ) { return(Inf); }

            estimatedPopulationSize <- base::sum(p.subset[,self$weight]);
            if ( 0 == estimatedPopulationSize ) { return( Inf ); }

            p <- base::nrow(np.subset) / estimatedPopulationSize;
            if ( 1 < p ) { return(Inf); }

            impurity <- 2 * p * (1 - p);
            return( impurity );
            },
        splitCriterion = R6Class(
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
        birthCriterion = R6Class(
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
            if (base::length(self$nodes) > 0) {
                nodeIDs <- base::as.integer(base::lapply(X = self$nodes, FUN = function(x) { return( x$nodeID ); } ))
                self$nodes <- self$nodes[base::order(nodeIDs)];
                return( NULL );
                }
            },

        node = R6Class(
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

