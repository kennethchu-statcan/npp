
compute_AIC <- function(
    DF.retained.nodes         = NULL,
    DF.npdata.with.propensity = NULL,
    DF.pdata.with.nodeID      = NULL
    ) {

    thisFunctionName <- "compute_AIC";

    base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    base::cat(base::paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.leaves <- DF.retained.nodes[base::is.na(DF.retained.nodes['satisfiedChildID']),];
    DF.leaves[,'likelihood.summand'] <- base::apply(
        X      = DF.leaves[,base::c('p.weight','propensity')],
        MARGIN = 1,
        FUN    = function(x) { return( x[1] * ( x[2]*base::log(x[2]) + (1-x[2]) * base::log(1-x[2]) ) ) }
        );
    base::cat("\nDF.leaves\n");
    base::print( DF.leaves   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    likelihood.estimate <- base::sum(DF.leaves[,'likelihood.summand']);
    base::cat("\nlikelihood.estimate\n");
    base::print( likelihood.estimate   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.design.variances <- base::data.frame(
        nodeID          = DF.leaves[,'nodeID'],
        design.variance = base::rep(x = NA, times = base::nrow(DF.leaves))
        );

    for ( temp.nodeID in DF.design.variances[,'nodeID'] ) {
        DF.temp <- DF.pdata.with.nodeID[DF.pdata.with.nodeID[,'nodeID'] == temp.nodeID,];
        base::cat(base::paste0("\n# temp.nodeID: ",temp.nodeID,"\n"));
        base::cat("\nstr(DF.temp)\n");
        base::print( str(DF.temp)   );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::cat(base::paste0("\n# ",thisFunctionName,"() quits."));
    base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    base::return( NULL );

    }
