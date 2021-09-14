
test.nppCART.sanity.tree.hierarchy <- function(
    seed            = 1234567,
    population.flag = "sanity",
    population.size = 10000,
    n.replicates    =   500
    ) {

    thisFunctionName <- "test.nppCART.sanity.tree.hierarchy";

    cat("\n### ~~~~~~~~~~~~~~~~~~~~ ###");
    cat(paste0("\n# ",thisFunctionName,"() starts.\n\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(nppR);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.directory <- normalizePath(getwd());
    temp.directory     <- file.path(normalizePath(original.directory),'output-00-sanity-tree-hierarchy');
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
    DF.population <- test.nppCART_get.population(
        seed            = seed,
        population.flag = "03", # "sanity",
        population.size = population.size,
        ordered.x1      = TRUE,
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
        colnames.np           = c("unit.ID","y","x1","x2","x1.jitter","x2.jitter"),
        colnames.p            = c("unit.ID","x1","x2"),
        prob.selection        = 0.999999999,
        n.replicates          = n.replicates,
        RData.non.probability = paste0("DF-",population.flag,"-non-probability.RData"),
        RData.probability     = paste0("DF-",population.flag,"-probability.RData")
        );

    cat("\nstr(list.samples[['DF.probability']])\n");
    print( str(list.samples[['DF.probability']])   );

    cat("\nsummary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])\n");
    print( summary(list.samples[['DF.probability']][,c('unit.ID','x1','x2','sampling.fraction','design.weight')])   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART <- nppCART(
        np.data                   = list.samples[['DF.non.probability']],
        p.data                    = list.samples[['DF.probability'    ]], # DF.probability,
        predictors                = c("x1","x2"),
        sampling.weight           = "design.weight",
        bootstrap.weights         = paste0("repweight",seq(1,n.replicates)),
        min.cell.size.np          = 1,
        min.impurity              = 1e-300,
        n.levels.approx.threshold = 4
        );

    my.nppCART$grow();
    cat("\nmy.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} )\n");
    my.nppCART$print( FUN.format = function(x) {return(round(x,digits=3))} );

    DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();
    cat("\nstr(DF.npdata.with.propensity)\n");
    print( str(DF.npdata.with.propensity)   );

    DF.impurity.alpha.AIC <- my.nppCART$get_impurities_alphas_AICs();
    cat("\nDF.impurity.alpha.AIC\n");
    print( DF.impurity.alpha.AIC   );

    write.csv(
        x         = DF.impurity.alpha.AIC,
        file      = "DF-sanity-nppCART-impurity-alpha-AIC.csv",
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    write.csv(
        x         = DF.population,
        file      = "DF-sanity-population-with-self-selection.csv",
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = self.selected ~ .,
        data    = DF.population[,c('x1','x2','self.selected')],
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    # cat("\nresults.rpart\n");
    # print( results.rpart   );

    png("plot-sanity-rpart.png");
    rpart.plot(x = results.rpart);
    dev.off();

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
