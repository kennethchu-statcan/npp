
update.branches <- function(
    DF.input = NULL
    ) {

    DF.output <- DF.input;

    DF.output[,'riskLeaves'] <- DF.output[is.leaf,'riskWgtd'];
    DF.output[,'nLeaves'   ] <- 0;

    is.leaf <- base::is.na(DF.output[,'satisfiedChildID']);
    DF.output[is.leaf,'nLeaves'] <- 1;

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

    }

##################################################
prune.branches <- function(
    DF.input  = NULL,
    tolerance = 1e-9
    ) {
    DF.output      <- DF.input;
    min.CART.g     <- base::min(DF.output[,'myCART.g'], na.rm  =TRUE);
    is.g.minimizer <- (base::abs(DF.output[,'myCART.g'] - min.CART.g) < tolerance);
    g.minimizers   <- base::setdiff(DF.output[is.g.minimizer,'nodeID'],NA);
    nodes.to.prune <- base::c();
    for ( g.minimizer in g.minimizers) {
        nodes.to.prune <- base::c(nodes.to.prune,get.descendant.nodeIDs(DF.input = DF.input,nodeID=g.minimizer));
        is.selected <- (DF.output[,'nodeID'] == g.minimizer);
        DF.output[is.selected,'riskLeaves'] <- DF.output[is.selected,'riskWgtd'];
        DF.output[is.selected,'nLeaves']    <- 1;
        DF.output[is.selected,c('satisfiedChildID','notSatisfiedChildID')] <- NA;
        }
    DF.output <- DF.output[!(DF.output[,'nodeID'] %in% nodes.to.prune),];
    return(DF.output);
    }

get.descendant.nodeIDs <- function(
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
    }
