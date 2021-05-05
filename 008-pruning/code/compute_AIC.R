
compute_AIC <- function(
    DF.retained.nodes         = NULL,
    DF.npdata.with.propensity = NULL,
    DF.pdata.with.nodeID      = NULL,
    sampling.weight.varname   = NULL,
    replicate.weight.varnames = NULL,
    combined.weights          = FALSE
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
    # DF.design.variances <- base::data.frame(
    #     nodeID          = DF.leaves[,'nodeID'],
    #     design.variance = base::rep(x = NA, times = base::nrow(DF.leaves))
    #     );
    #
    # for ( temp.nodeID in DF.design.variances[,'nodeID'] ) {
    #     DF.temp <- DF.pdata.with.nodeID[DF.pdata.with.nodeID[,'nodeID'] == temp.nodeID,];
    #     base::cat(base::paste0("\n# temp.nodeID: ",temp.nodeID,"\n"));
    #     base::cat("\ncolnames(DF.temp)\n");
    #     base::print( colnames(DF.temp)   );
    #     base::cat("\nstr(DF.temp)\n");
    #     base::print( str(DF.temp)   );
    #     }

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
    base::cat("\nstr(template.svrepdesign)\n");
    base::print( str(template.svrepdesign)   );

    template.results.svyby <- survey::svyby(
        design  = template.svrepdesign,
        formula = as.formula("~ dummy.one"),
        by      = as.formula("~ nodeID"),
        FUN     = svytotal, # svymean # svyvar
        vartype = "var" # c("se","ci","ci","cv","cvpct","var")
        );
    # base::cat("\nstr(template.results.svyby)\n");
    # base::print( str(template.results.svyby)   );

    base::cat("\ntemplate.results.svyby\n");
    base::print( template.results.svyby   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::cat("\nDF.leaves\n");
    base::print( DF.leaves   );

    DF.leaves <- base::merge(
        x     = DF.leaves,
        y     = template.results.svyby,
        by    = "nodeID",
        all.x = TRUE,
        sort  = TRUE
        );

    base::cat("\nDF.leaves\n");
    base::print( DF.leaves   );

    DF.leaves[,'trace.summand'] <- base::apply(
        X      = DF.leaves[,c('p.weight','propensity','var')],
        MARGIN = 1,
        FUN    = function(x) { return( x[2] * x[3] / (x[1]*(1-x[2])) ) }
        );

    base::cat("\nDF.leaves\n");
    base::print( DF.leaves   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    likelihood.estimate <- base::sum(DF.leaves[,'likelihood.summand']);
    base::cat("\nlikelihood.estimate\n");
    base::print( likelihood.estimate   );

    trace.term <- base::sum(DF.leaves[,'trace.summand']);
    base::cat("\ntrace.term\n");
    base::print( trace.term   );

    output.AIC <- 2 * (base::nrow(DF.leaves) + trace.term - likelihood.estimate);
    base::cat("\noutput.AIC\n");
    base::print( output.AIC   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::cat(base::paste0("\n# ",thisFunctionName,"() quits."));
    base::cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    base::return( output.AIC );

    }
