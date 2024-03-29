
test.nppCART.sanity.tree.growing <- function(
    seed            = 1234567,
    population.size =   10000,
    n.replicates    =     500
    ) {

    thisFunctionName <- "test.nppCART.sanity.tree.growing";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.directory <- normalizePath(getwd());
    temp.directory     <- file.path(normalizePath(original.directory),'output-00-sanity-tree-growing');
    Sys.sleep(time = 5); if ( !dir.exists(temp.directory) ) { dir.create(temp.directory); }
    Sys.sleep(time = 5); setwd(temp.directory);
    Sys.sleep(time = 5);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    file.output  <- file(description = "sink-output.txt",  open = "wt");
    file.message <- file(description = "sink-message.txt", open = "wt");

    sink(file = file.output,  type = "output" );
    sink(file = file.message, type = "message");

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(rpart);
    require(tree);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.nppCART_get.population(
        seed            = seed,
        population.flag = "sanity",
        population.size = population.size,
        ordered.x1      = FALSE,
        ordered.x2      = TRUE
        );

    cat("\nstr(DF.population)\n");
    print( str(DF.population)   );

    visualizePopulation(
        population     = DF.population,
        textsize.title = 30,
        textsize.axis  = 20,
        inputIsNumeric = FALSE,
        target_density_limits = c(-5,165),
        target_density_breaks = seq(0,160,20)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.samples  <- test.nppCART_get.samples(
        DF.population         = DF.population,
        prob.selection        = 0.999999999,
        n.replicates          = n.replicates,
        RData.non.probability = paste0("DF-sanity-non-probability.RData"),
        RData.probability     = paste0("DF-sanity-probability.RData")
        );

    cat("\nstr(list.samples[['DF.probability']])\n");
    print( str(list.samples[['DF.probability']])   );

    cat("\nsummary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])\n");
    print( summary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    write.csv(
        x         = DF.population,
        file      = paste0("DF-sanity-population-with-self-selection.csv"),
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # results.tree <- tree(
    #     formula = self.selected ~ .,
    #     data    = DF.population[,c('x1','x2','x3','self.selected')],,
    #     split   = "gini",
    #     control = tree.control(
    #         nobs    = nrow(DF.population),
    #         mincut  = 1,
    #         minsize = 2,
    #         mindev  = 1e-50
    #         )
    #     );
    #
    # cat("\nresults.tree\n");
    # print( results.tree   );
    #
    # cat("\nstr(results.tree)\n");
    # print( str(results.tree)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = self.selected ~ .,
        data    = DF.population[,c('x1','x2','x3','self.selected')],
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    cat("\nresults.rpart\n");
    print( results.rpart   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART <- nppCART(
        np.data                   = list.samples[['DF.non.probability']],
        p.data                    = list.samples[['DF.probability'    ]],
        predictors                = c("x1","x2","x3"),
        sampling.weight           = "design.weight",
      # bootstrap.weights         = paste0("repweight",seq(1,n.replicates)),
        min.cell.size.np          = 1,
        min.impurity              = 1e-300, # 1e-9,
        n.levels.approx.threshold = 4
        );

    my.nppCART$grow();
    cat("\nmy.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} )\n");
    my.nppCART$print( FUN.format = function(x) {return(round(x,digits=10))} );

    DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
    cat("\nstr(DF.npdata.with.propensity)\n");
    print( str(DF.npdata.with.propensity)   );

    DF.impurity.alpha.AIC <- my.nppCART$get_impurities_alphas_AICs();
    cat("\nDF.impurity.alpha.AIC\n");
    print( DF.impurity.alpha.AIC   );

    write.csv(
        x         = DF.impurity.alpha.AIC,
        file      = paste0("DF-sanity-nppCART-impurity-alpha-AIC.csv"),
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.rpart.leaves <- results.rpart[['frame']][results.rpart[['frame']][,'var'] == '<leaf>',c('var','n','complexity')];
    cat("\nDF.rpart.leaves\n");
    print( DF.rpart.leaves   );

    DF.nppCART.leaves <- unique(DF.npdata.with.propensity[,c('nodeID','propensity','np.count','p.weight','impurity')]);
    cat("\nDF.nppCART.leaves\n");
    print( DF.nppCART.leaves   );

    write.csv(
        x         = DF.rpart.leaves,
        file      = paste0("DF-sanity-leaves-rpart.csv"),
        row.names = FALSE
        );

    write.csv(
        x         = DF.nppCART.leaves,
        file      = paste0("DF-sanity-leaves-nppCART.csv"),
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    leaf.sizes.rpart   <- sort(DF.rpart.leaves[,  'n'       ]);
    leaf.sizes.nppCART <- sort(DF.nppCART.leaves[,'p.weight']);

    cat("\nlength(leaf.sizes.rpart)\n");
    print( length(leaf.sizes.rpart)   );

    cat("\nlength(leaf.sizes.nppCART)\n");
    print( length(leaf.sizes.nppCART)   );

    cat("\nsummary(leaf.sizes.rpart)\n");
    print( summary(leaf.sizes.rpart)   );

    cat("\nsummary(leaf.sizes.nppCART)\n");
    print( summary(leaf.sizes.nppCART)   );

    if ( length(leaf.sizes.rpart) == length(leaf.sizes.nppCART) ) {
        DF.leaf.sizes <- data.frame(leaf.sizes.rpart = leaf.sizes.rpart, leaf.sizes.nppCART = leaf.sizes.nppCART);
        DF.leaf.sizes[,'abs.diff'] <- abs(DF.leaf.sizes[,'leaf.sizes.rpart'] - DF.leaf.sizes[,'leaf.sizes.nppCART']);
        cat("\nDF.leaf.sizes\n");
        print( DF.leaf.sizes   );
        cat("\nmax(DF.leaf.sizes[,'abs.diff'])\n");
        print( max(DF.leaf.sizes[,'abs.diff'])   );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");
    # print warning messages to log
    cat("\n##### warnings()\n")
    print(warnings());

    # print session info to log
    cat("\n##### sessionInfo()\n")
    print( sessionInfo() );

    # print system time to log
    cat(paste0("\n##### Sys.time(): ",Sys.time(),"\n"));

    # print elapsed time to log
    stop.proc.time <- proc.time();
    cat("\n##### start.proc.time() - stop.proc.time()\n");
    print( stop.proc.time - start.proc.time );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    sink(file = NULL, type = "output" );
    sink(file = NULL, type = "message");
    sink();

    Sys.sleep(time = 5);
    setwd(original.directory);
    Sys.sleep(time = 5);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# ",thisFunctionName,"() quits."));
    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###\n");

    return( NULL );

    }

##################################################
