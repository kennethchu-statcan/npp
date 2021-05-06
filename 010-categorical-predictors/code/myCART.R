
require(R6);
require(dplyr);
require(utils);

myCART  <- R6Class(
    classname = "myCART",

    public = list(

        formula = NULL,
        data    = NULL,

        response                  = NULL,
        predictors                = NULL,
        predictors_factor         = NULL,
        predictors_ordered_factor = NULL,
        predictors_numeric        = NULL,

        syntheticID       = NULL,
        nodes             = NULL,
        subtree.hierarchy = NULL,

        initialize = function(formula, data) {

            self$formula <- as.formula(formula);
            self$data    <- data;

            temp <- all.vars(self$formula);
            self$response   <- temp[1];
            self$predictors <- setdiff(temp,self$response);

            if (identical(".",self$predictors)) {
                self$predictors <- base::setdiff(colnames(self$data),c(self$response));
                }

            for (temp.colname in self$predictors) {
                if (is.character(self$data[,temp.colname])) {
                    self$data[,temp.colname] <- as.factor(self$data[,temp.colname]);
                    }
                }

            is.numeric.column        <- sapply(X = self$data[,self$predictors], FUN = function(x) { return(is.numeric(x))                 });
            is.ordered.factor.column <- sapply(X = self$data[,self$predictors], FUN = function(x) { return(is.factor(x) &  is.ordered(x)) });
            is.factor.column         <- sapply(X = self$data[,self$predictors], FUN = function(x) { return(is.factor(x) & !is.ordered(x)) });

            self$predictors_numeric        <- self$predictors[is.numeric.column       ];
            self$predictors_ordered_factor <- self$predictors[is.ordered.factor.column];
            self$predictors_factor         <- self$predictors[is.factor.column        ];

            # add custom row ID:
            self$syntheticID <- paste0(sample(x=letters,size=10,replace=TRUE),collapse="");
            self$data[,self$syntheticID] <- seq(1,nrow(self$data));

            remove(temp);
            },

        grow = function() {

            self$nodes <- list();
            lastNodeID <- 0; # 1; # 0;

            workQueue <- list(
                private$node$new(
                    parentID = -1, # 0, #-1,
                    nodeID   = lastNodeID,
                    depth    = 0,
                    rowIDs   = self$data[,self$syntheticID]
                    )
                );

            while (0 < length(workQueue)) {

                #cat( "\n### ~~~~~~~~~~ ###" );
                #cat( paste0("\nlength(workQueue): ",length(workQueue),"\n") );
                #print( workQueue );

                currentNode <- private$pop(workQueue, envir = environment());

                currentNodeID          <- currentNode$nodeID;
                currentParentID        <- currentNode$parentID;
                currentDepth           <- currentNode$depth;
                currentRowIDs          <- currentNode$rowIDs;
                current_birthCriterion <- currentNode$birthCriterion;

                #cat("\ncurrentNode:");
                #print( currentNode );

                if (private$stoppingCriterionSatisfied(currentRowIDs) ) {
                    self$nodes <- private$push(
                        list = self$nodes,
                        x    = private$node$new(
                            nodeID   = currentNodeID,
                            parentID = currentParentID,
                            depth    = currentDepth,
                            rowIDs   = currentRowIDs,
                            impurity = private$impurity(
                                self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$response]
                                ),
                            risk = private$risk(
                                self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$response]
                                ),
                            birthCriterion = current_birthCriterion
                            )
                        );
                } else {

                    #cat("\ncurrentRowIDs:\n");
                    #print( currentRowIDs    );

                    bestSplit <- private$get_best_split(currentRowIDs = currentRowIDs);
                    #cat("\nbestSplit:\n");
                    #print( bestSplit );

                    satisfied <- self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$syntheticID][
                        bestSplit$comparison(
                            self$data[self$data[,self$syntheticID] %in% currentRowIDs,bestSplit$varname],
                            bestSplit$threshold
                            )
                        ];
                    notSatisfied <- base::sort(base::setdiff(currentRowIDs,satisfied));

                    #cat("\nsatisfied:\n");
                    #print( satisfied    );

                    #cat("\nnotSatisfied:\n");
                    #print( notSatisfied    );
                    # adding 2 here to make ordering of nodeID agree with the order of appearance in self$nodes
                    lastNodeID          <- lastNodeID + 2;
                    notSatisfiedChildID <- lastNodeID;
                    workQueue           <- private$push(
                        list = workQueue,
                        x    = private$node$new(
                            parentID = currentNodeID,
                            nodeID   = lastNodeID,
                            depth    = currentDepth + 1,
                            rowIDs   = notSatisfied,
                            birthCriterion = private$birthCriterion$new(
                                varname    = bestSplit$varname,
                                threshold  = bestSplit$threshold,
                                comparison = ifelse(bestSplit$varname %in% self$predictors_factor,"!=",">=")
                                )
                            )
                        );
                    # subtracting 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
                    lastNodeID       <- lastNodeID - 1;
                    satisfiedChildID <- lastNodeID;
                    workQueue        <- private$push(
                        list = workQueue,
                        x    = private$node$new(
                            parentID = currentNodeID,
                            nodeID   = lastNodeID,
                            depth    = currentDepth + 1,
                            rowIDs   = satisfied,
                            birthCriterion = private$birthCriterion$new(
                                varname    = bestSplit$varname,
                                threshold  = bestSplit$threshold,
                                comparison = ifelse(bestSplit$varname %in% self$predictors_factor,"=","<")
                                ),
                            )
                        );
                    # adding 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
                    lastNodeID <- lastNodeID + 1;
                    self$nodes <- private$push(
                        list = self$nodes,
                        x = private$node$new(
                            nodeID   = currentNodeID,
                            parentID = currentParentID,
                            depth    = currentDepth,
                            rowIDs   = currentRowIDs,
                                impurity = private$impurity(
                                self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$response]
                                ),
                            risk = private$risk(
                                self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$response]
                                ),
                            splitCriterion = bestSplit,
                            birthCriterion = current_birthCriterion,
                            satisfiedChildID    =    satisfiedChildID,
                            notSatisfiedChildID = notSatisfiedChildID
                            )
                        );
                    #cat( paste0("\ncurrentNodeID: ",currentNodeID) );
                    #}
                    }
                }

            # private$order_nodes();
            # return( NULL );

            },

        predict = function() {
            return( NULL );
            },

        print = function(
            FUN.format = function(x) { return(x) }
            ) {
            if ( 0 == length(self$nodes) ) {
                cat("\nlist of nodes is empty.\n")
                }
            else {
                for ( i in seq(1,length(self$nodes)) ) {
                    self$nodes[[i]]$print_node(FUN.format = FUN.format);
                    }
                cat("\n");
                }
            },

        public_nodes_to_table = function(nodes = self$nodes) {
            return( private$nodes_to_table(nodes = nodes) );
            },

        public_get_subtree_hierarchy = function() {
            if ( base::is.null(self$subtree_hierarchy) ) {
                self$subtree.hierarchy <- private$generate_subtree_hierarchy(DF.nodes = private$nodes_to_table());
            }
            return( self$subtree.hierarchy );
            }

        ), # public = list(

    private = list(
        pop = function(list, i = length(list), envir = NULL) {
            stopifnot(inherits(list, "list"))
            if (0 == length(list)) { return(NULL); }
            result <- list[[i]];
            assign(x = deparse(substitute(list)), value = list[-i], envir = envir);
            return( result );
            },
        push = function(list, x, i = length(list)) {
            stopifnot(inherits(list, "list"));
            return( c(list,list(x)) );
            },
        stoppingCriterionSatisfied = function(currentRowIDs = NULL) {
            deduplicatedOutcomes <- unique(self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$response]);
            testSplit <- private$get_best_split(currentRowIDs = currentRowIDs);
            # stop if there is only 1 unique outcome or if there is no best split (returns NULL)
            return( 1 == length(deduplicatedOutcomes) | is.null(testSplit) );
            },
        get_descendants = function(nodeIDs = NULL, nodeID = NULL) {
            nodes <- self$nodes[unlist(lapply(
                X   = self$nodes,
                FUN = function(x) { return( x$nodeID %in% nodeIDs) }
                ))];
            temp <- unlist(lapply(X = nodes, FUN = function(x) { return( nodeID == x$nodeID ) }));
            if (1 == sum(temp)) {
                targetNode  <- nodes[temp][[1]];
                descendants <- c();
                if (!is.null(targetNode$satisfiedChildID)) {
                    descendants <- c(
                        descendants,
                        targetNode$satisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$satisfiedChildID)
                        );
                    }
                if (!is.null(targetNode$notSatisfiedChildID)) {
                    descendants <- c(
                        descendants,
                        targetNode$notSatisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$notSatisfiedChildID)
                        );
                    }
                return( sort(descendants) );
            } else {
                return( c() );
                }
            },
        nodes_to_table = function(nodes = self$nodes) {
            if ( 0 == length(nodes) ) {
                cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nrow.output <- length(nodes);
            DF.output <- data.frame(
                nodeID     = numeric(length = nrow.output),
                depth      = numeric(length = nrow.output),
                nRecords   = numeric(length = nrow.output),
                prop       = numeric(length = nrow.output),
                #impurity  = numeric(length = nrow.output),
                risk       = numeric(length = nrow.output),
                riskWgtd   = numeric(length = nrow.output),
                riskLeaves = numeric(length = nrow.output),
                nLeaves    = numeric(length = nrow.output),
                parentID   = numeric(length = nrow.output),
                   satisfiedChildID = numeric(length = nrow.output),
                notSatisfiedChildID = numeric(length = nrow.output)
                );
            totalNumRecords <- length(nodes[[1]]$rowIDs);
            for ( i in seq(1,nrow.output) ) {
                DF.output[i,'nodeID'  ]   <- nodes[[i]]$nodeID;
                DF.output[i,'depth'   ]   <- nodes[[i]]$depth;
                DF.output[i,'nRecords']   <- length(nodes[[i]]$rowIDs);
                DF.output[i,'prop'    ]   <- DF.output[i,'nRecords'] / totalNumRecords;
                #DF.output[i,'impurity']  <- nodes[[i]]$impurity;
                DF.output[i,'risk']       <- nodes[[i]]$risk;
                DF.output[i,'riskWgtd']   <- DF.output[i,'risk'] * DF.output[i,'prop'];
                DF.output[i,'riskLeaves'] <- 0;
                DF.output[i,'nLeaves']    <- 0;
                DF.output[i,'parentID']   <- nodes[[i]]$parentID;
                DF.output[i,'satisfiedChildID'] <- ifelse(
                    is.null(nodes[[i]]$satisfiedChildID),
                    NA,
                    nodes[[i]]$satisfiedChildID
                    );
                DF.output[i,'notSatisfiedChildID'] <- ifelse(
                    is.null(nodes[[i]]$notSatisfiedChildID),
                    NA,
                    nodes[[i]]$notSatisfiedChildID
                    );
                }
            return( DF.output );
            },
        get_best_split = function(currentRowIDs) {
            uniqueVarValuePairs_factor         <- list();
            uniqueVarValuePairs_ordered_factor <- list();
            uniqueVarValuePairs_numeric        <- list();
            if (length(self$predictors_factor) > 0) {
                temp.list <- as.list(private$get_non_constant_columns(
                    DF.input       = self$data,
                    currentRowIDs  = currentRowIDs,
                    input.colnames = self$predictors_factor
                    ));
                if (length(temp.list) > 0) {
                    for ( temp.name in names(temp.list) ) {
                        # temp.labels <- names(sort(table(temp.list[[temp.name]]),decreasing=TRUE));
                        temp.labels <- names(sort(table(temp.list[[temp.name]])));
                        for ( temp.length in seq(1,length(temp.labels)-1) ) {
                            uniqueVarValuePairs_factor <- private$push(
                                list = uniqueVarValuePairs_factor,
                                x    = private$splitCriterion$new(
                                    varname    = temp.name,
                                    threshold  = temp.labels[seq(1,temp.length)],
                                    comparison = private$is_element_of
                                    )
                                );
                            }
                        }
                    }
                }
            if (length(self$predictors_ordered_factor) > 0) {
                temp.list <- as.list(private$get_non_constant_columns(
                    DF.input       = self$data,
                    currentRowIDs  = currentRowIDs,
                    input.colnames = self$predictors_ordered_factor
                    ));
                if (length(temp.list) > 0) {
                    uniqueVarValuePairs_ordered_factor <- private$get_var_value_pairs(
                        x = lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_less_than
                        );
                    }
                }
            if (length(self$predictors_numeric) > 0) {
                temp.list <- as.list(private$get_non_constant_columns(
                    DF.input       = self$data,
                    currentRowIDs  = currentRowIDs,
                    input.colnames = self$predictors_numeric
                    ));
                if (length(temp.list) > 0) {
                    uniqueVarValuePairs_numeric <- private$get_var_value_pairs(
                        x = lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_less_than
                        );
                    }
                }
            uniqueVarValuePairs <- c(uniqueVarValuePairs_factor,uniqueVarValuePairs_ordered_factor,uniqueVarValuePairs_numeric);

            impurities <- lapply(
                X   = uniqueVarValuePairs,
                FUN = function(x) {

                    #PRINTME <- x$comparison(
                    #        self$data[self$data[,self$syntheticID] %in% currentRowIDs,x$varname],
                    #        x$threshold
                    #        )
                    #print(paste0("Comparison: ", PRINTME, ", Threshold: ", x$threshold))

                    satisfied <- self$data[self$data[,self$syntheticID] %in% currentRowIDs,self$syntheticID][
                        x$comparison(
                            self$data[self$data[,self$syntheticID] %in% currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    notSatisfied <- base::sort(base::setdiff(currentRowIDs,satisfied));

                    p1 <- length(   satisfied) / length(currentRowIDs);
                    p2 <- length(notSatisfied) / length(currentRowIDs);
                    g1 <- private$impurity(self$data[self$data[,self$syntheticID] %in%    satisfied,self$response]);
                    g2 <- private$impurity(self$data[self$data[,self$syntheticID] %in% notSatisfied,self$response]);

                    #print(paste0(x$threshold, ", ", p1, ", ", g1, ", ", p2, ", ", g2, ", ", p1 * g1 + p2 * g2))

                    return( p1 * g1 + p2 * g2 );
                    }
                );
            #print(impurities)

            # checks for NULL cases (no best split):
            #   -   uniqueVarValuePairs is empty (no available splits)
            #   -   all impurities are NaN (no meaningful splits)
            if(length(uniqueVarValuePairs) < 1 | all(lapply(X = impurities, FUN = function(x) { return( is.nan(x) ) }))) {
                output <- NULL
            } else {
                output <- uniqueVarValuePairs[[ which.min(impurities) ]];
            }

            return( output );
            },
        get_non_constant_columns = function(DF.input = NULL, currentRowIDs = NULL, input.colnames = NULL) {
            DF.output <- DF.input[DF.input[,self$syntheticID] %in% currentRowIDs,input.colnames];

            # If length(input.colnames) == 1, then DF.output will be a vector.
            # In that case, cast DF.output into a data frame.
            DF.output <- as.data.frame(DF.output);

            colnames(DF.output) <- input.colnames;
            nUniqueValues       <- apply(X = DF.output, MARGIN = 2, FUN = function(x) { return(length(unique(x))) } );
            DF.output           <- as.data.frame(DF.output[,nUniqueValues > 1]);
            colnames(DF.output) <- input.colnames[nUniqueValues > 1];

            return( DF.output );
            },
        get_midpoints = function(x) {
            if (is.numeric(x)) {
                y <- sort(unique(x));
                return( apply(X=data.frame(c1=y[2:length(y)],c2=y[1:(length(y)-1)]),MARGIN=1,FUN=mean) );
                }
            else if (is.factor(x) & !is.ordered(x)) {
                return( private$enumerate_set(levels(x)) );
                }
            else {
                return( sort(unique(x)) );
                }
            },
        get_var_value_pairs = function(x = NULL, comparison = NULL) {
            names_x  <- names(x);
            templist <- list();
            for (i in seq(1,length(names_x))) {
                for (j in seq(1,length(x[[i]]))) {
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
        get_var_value_pairs_factor = function(x = NULL, comparison = NULL) {
            cat("\nget_var_value_pairs_factor(): x\n")
            print( x )
            return( NULL );
            # names_x  <- names(x);
            # templist <- list();
            # for (i in seq(1,length(names_x))) {
            #     for (j in seq(1,length(x[[i]]))) {
            #         templist <- private$push(
            #             list = templist,
            #             x    = private$splitCriterion$new(
            #                 varname    = names_x[i],
            #                 threshold  = x[[i]][j],
            #                 comparison = comparison
            #                 )
            #             );
            #         }
            #     }
            # return( templist );
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
            for (i in seq(1, trunc(length(set) / 2))) {

                # combn returns a dataframe containing all possible combinations;
                # ncol corresponds to the number of possible combinations
                size <- ncol(combn(set, i))

                # special even case: checks if group size i is equal to half the length of the set
                if ( i == length(set) / 2) {
                    matrix <- combn(set, i)

                    # iterate through half of the combinations and store them all
                    # (again, this will cover all possible combinations, since we look at what's
                    #  in the group and what's not in the group)
                    for (j in seq(1, size / 2)) {
                        combinationsA[[indexA]] <- matrix[, j]
                        #combinationsB[[indexB]] <- matrix[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
                # standard case
                else {
                    matrixA <- combn(set, i)
                    #matrixB <- combn(set, length(set) - i)

                    # iterate through all the combinations and store them all
                    # (here, we are looking at what's in the group of size i, and what's
                    #  not in the group, which has size = length(set) - i)
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
            tmp <- lapply(
                X   = x,
                FUN = function(input) {
                    for(element in y) {
                        if(element == input) return(TRUE)
                        }
                    return(FALSE)
                    }
                );
            return(unlist(tmp));
            },
        impurity = function(x){
            # Gini impurity
            p <- as.numeric(table(x) / length(x));
            return( sum(p * (1 - p)) );
            },
        risk = function(x){
            # Gini impurity
            p <- as.numeric(table(x) / length(x));
            return( sum(p * (1 - p)) );
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
            if (length(self$nodes) > 0) {
                nodeIDs <- as.integer(lapply(X = self$nodes, FUN = function(x) { return( x$nodeID ); } ))
                self$nodes <- self$nodes[order(nodeIDs)];
                return( NULL );
                }
            },
        generate_subtree_hierarchy = function(DF.nodes = private$nodes_to_table()) {
            index.subtree <- 1;
            list.subtrees <- base::list();
            list.subtrees[[index.subtree]] <- base::list(
                alpha           = 0,
                nodes_untouched = DF.nodes[,"nodeID"],
                nodes_pruned_at = c(),
                nodes_removed   = c(),
                subtree         = DF.nodes
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
            list.subtrees[[1]][['pruned_nodes']] <- self$nodes;
            for ( index.subtree in seq(2,length(list.subtrees)) ) {
                list.subtrees[[index.subtree]][['pruned_nodes']] <- private$get_pruned_nodes(
                    input.nodes  = list.subtrees[[index.subtree - 1]][['pruned_nodes']],
                    pruning.info = list.subtrees[[index.subtree    ]]
                    );
                }
            return( list.subtrees );
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
            min.CART.g     <- base::min(DF.output[,'myCART.g'], na.rm = TRUE);
            is.g.minimizer <- (base::abs(x = DF.output[,'myCART.g'] - min.CART.g) < tolerance);
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
            DF.output[,'myCART.g'] <- (DF.output[,'riskWgtd'] - DF.output[,'riskLeaves']) / (DF.output[,'nLeaves'] - 1);
            return(DF.output);
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
        node = R6Class(
            classname = "node",

            public = list(

                nodeID   = NULL,
                parentID = NULL,
                depth    = NULL,
                rowIDs   = NULL,
                impurity = NULL,
                risk     = NULL,

                splitCriterion = NULL,
                birthCriterion = NULL,

                satisfiedChildID    = NULL,
                notSatisfiedChildID = NULL,

                initialize = function(
                    nodeID   = NULL,
                    parentID = NULL,
                    depth    = NULL,
                    rowIDs   = NULL,
                    impurity = NULL,
                    risk     = NULL,

                    splitCriterion = NULL,
                    birthCriterion = NULL,

                    satisfiedChildID    = NULL,
                    notSatisfiedChildID = NULL
                    ) {
                        self$nodeID   <- nodeID;
                        self$parentID <- parentID;
                        self$depth    <- depth;
                        self$rowIDs   <- rowIDs;
                        self$impurity <- impurity;
                        self$risk     <- risk;

                        self$splitCriterion <- splitCriterion;
                        self$birthCriterion <- birthCriterion;

                        self$satisfiedChildID    <- satisfiedChildID;
                        self$notSatisfiedChildID <- notSatisfiedChildID;
                    },

                print_node = function(
                    indent     = '  ',
                    FUN.format = function(x) { return(x) }
                    ) {
                    cat("\n");
                    cat(paste0(rep(indent,self$depth),collapse="") );
                    cat(paste0("(",self$nodeID,") "));
                    if (0 == self$nodeID) {
                        cat("[root]");
                        }
                    else {
                        cat(paste0("[",
                            self$birthCriterion$varname,   " ",
                            self$birthCriterion$comparison," ",
                            # self$birthCriterion$threshold,
                            paste(self$birthCriterion$threshold,collapse=","),
                            "]"));
                        }
                    cat(paste0(", n = ",       length(self$rowIDs)      ));
                    cat(paste0(", impurity = ",FUN.format(self$impurity)));
                    cat(paste0(", risk = ",    FUN.format(self$risk    )));
                    }
                )

            )

        )

    );
