
test.svyrepdesign <- function(
    ) {

    thisFunctionName <- "test.svyrepdesign";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    population.size <- 10000;

    DF.population <- getPopulation(
        seed            = 7654321,
        population.size = population.size,
        population.flag = "02"
        );
    cat("\nstr(DF.population)\n");
    print( str(DF.population)   );

    DF.population <- get.modified.population(DF.input = DF.population);
    cat("\nstr(DF.population)\n");
    print( str(DF.population)   );

    LIST.samples <- getSamples(
        DF.population  = DF.population,
        prob.selection = 0.1
        );
    cat("\nstr(LIST.samples)\n");
    print( str(LIST.samples) );

    DF.probability <- LIST.samples[['probability.sample']];
    DF.probability[,'sampling.fraction'] <- 1 / DF.probability[,'weight'];
    DF.probability[,'dummy.one'] <- 1;
    cat("\nstr(DF.probability)\n");
    print( str(DF.probability)   );

    my.svydesign.object <- svydesign(
        data = DF.probability,
        id   = ~1,
        fpc  = ~sampling.fraction
        );
    cat("\nstr(my.svydesign.object)\n");
    print( str(my.svydesign.object)   );

    my.svrepdesign.object <- as.svrepdesign(
        design     = my.svydesign.object,
        type       = "bootstrap",
        replicates = 500
        );
    cat("\nstr(my.svrepdesign.object)\n");
    print( str(my.svrepdesign.object)   );

    results.svyby <- svyby(
        design  = my.svrepdesign.object,
        formula = as.formula("~ dummy.one"),
        by      = as.formula("~ x2"),
        FUN     = svytotal, # svymean # svyvar
        vartype = "var" # c("se","ci","ci","cv","cvpct","var")
        );

    cat("\nstr(results.svyby)\n");
    print( str(results.svyby)   );

    cat("\nresults.svyby\n");
    print( results.svyby   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.repweights <- my.svrepdesign.object[['repweights']][['weights']];
    DF.repweights <- as.data.frame(DF.repweights);
    colnames(DF.repweights) <- paste0("repweight",seq(1,ncol(DF.repweights)));
    # cat("\nstr(DF.repweights)\n");
    # print( str(DF.repweights)   );

    DF.template <- cbind(
        my.svrepdesign.object[['variables']],
        DF.repweights
        );
    cat("\nstr(DF.template)\n");
    print( str(DF.template)   );

    template.svrepdesign <- svrepdesign(
        data       = DF.template,
        weights    = as.formula("~ weight"),
        type       = "bootstrap",
        repweights = "repweight[0-9]+",
        combined   = FALSE
        );
    cat("\nstr(template.svrepdesign)\n");
    print( str(template.svrepdesign)   );

    template.results.svyby <- svyby(
        design  = template.svrepdesign,
        formula = as.formula("~ dummy.one"),
        by      = as.formula("~ x2"),
        FUN     = svytotal, # svymean # svyvar
        vartype = "var" # c("se","ci","ci","cv","cvpct","var")
        );
    cat("\nstr(template.results.svyby)\n");
    print( str(template.results.svyby)   );

    cat("\ntemplate.results.svyby\n");
    print( template.results.svyby   );

    template.results.svyby <- svyby(
        design  = template.svrepdesign,
        formula = as.formula("~ dummy.one"),
        by      = as.formula("~ x2"),
        FUN     = svytotal, # svymean # svyvar
        vartype = "se" # c("se","ci","ci","cv","cvpct","var")
        );
    template.results.svyby[,'my.variance'] <- template.results.svyby[,'se']^2;

    cat("\nstr(template.results.svyby)\n");
    print( str(template.results.svyby)   );

    cat("\ntemplate.results.svyby\n");
    print( template.results.svyby   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    return( NULL );

    }

##################################################
