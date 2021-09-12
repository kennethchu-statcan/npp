
testthat::context(desc = "correctness test suite");

###################################################
test.correctness <- function() {
    my.tolerance <- ifelse("windows" == base::.Platform[["OS.type"]],1e-3,1e-6);
    test.correctness_tree.growing(  my.tolerance = my.tolerance);
    # test.correctness_tree.hierarchy(my.tolerance = my.tolerance);
    }

###################################################
test.correctness_tree.growing <- function(
    seed         = 1234567,
    my.tolerance = 1e-3
    ) {

    population.size <- 10000;
    n.replicates    <-     0;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- test.correctness_get.population(
        seed            = seed,
        population.flag = "tree.growing",
        population.size = population.size,
        ordered.x1      = FALSE,
        ordered.x2      = TRUE
        );

    list.samples  <- test.correctness_get.samples(
        DF.population  = DF.population,
        prob.selection = 0.999999999,
        n.replicates   = n.replicates
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected <- (DF.population[,'unit.ID'] %in% list.samples[['DF.non.probability']][,'unit.ID']);
    DF.population[                ,'self.selected'] <- FALSE;
    DF.population[is.self.selected,'self.selected'] <- TRUE;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart::rpart(
        formula = self.selected ~ .,
        data    = DF.population[,base::c('x1','x2','x3','self.selected')],
        control = list(minsplit = 1, minbucket = 1, cp = 0)
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.nppCART <- nppCART(
        np.data                   = list.samples[['DF.non.probability']],
        p.data                    = list.samples[['DF.probability'    ]],
        predictors                = base::c("x1","x2","x3"),
        sampling.weight           = "design.weight",
      # bootstrap.weights         = paste0("repweight",seq(1,n.replicates)),
        min.cell.size.np          = 1,
        min.cell.size.p           = 3,
        min.impurity              = 1e-300, # 1e-9,
        n.levels.approx.threshold = 4
        );
    my.nppCART$grow();

    DF.npdata.with.propensity <- my.nppCART$get_npdata_with_propensity();

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.rpart.leaves   <- results.rpart[['frame']][results.rpart[['frame']][,'var'] == '<leaf>',base::c('var','n','complexity')];
    DF.nppCART.leaves <- base::unique(DF.npdata.with.propensity[,c('nodeID','propensity','np.count','p.weight','impurity')]);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    leaf.sizes.rpart   <- base::sort(DF.rpart.leaves[,  'n'       ]);
    leaf.sizes.nppCART <- base::sort(DF.nppCART.leaves[,'p.weight']);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    testthat::test_that(
        desc = "tree growing: correctness of number of leaves",
        code = {
            testthat::expect_equal(
                object    = base::length(leaf.sizes.nppCART),
                expected  = base::length(leaf.sizes.rpart  ),
                tolerance = my.tolerance
                );
            }
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( base::length(leaf.sizes.rpart) == base::length(leaf.sizes.nppCART) ) {
        DF.leaf.sizes <- data.frame(leaf.sizes.rpart = leaf.sizes.rpart, leaf.sizes.nppCART = leaf.sizes.nppCART);
        DF.leaf.sizes[,'abs.diff'] <- base::abs(DF.leaf.sizes[,'leaf.sizes.rpart'] - DF.leaf.sizes[,'leaf.sizes.nppCART']);
        cat("\n# test.correctness_tree.growing(): DF.leaf.sizes\n");
        print(DF.leaf.sizes);
        cat("\n")
        testthat::test_that(
            desc = "tree growing: correctness of ordered sequence of leaf sizes",
            code = {
                testthat::expect_equal(
                    object    = base::max(DF.leaf.sizes[,'abs.diff']),
                    expected  = 0.0,
                    tolerance = my.tolerance
                    );
                }
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    }

test.correctness_tree.hierarchy <- function(
    my.tolerance = 1e-3
    ) {

    }

###################################################
test.correctness_get.population <- function(
    seed            = 1234567,
    population.flag = NULL,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {
    if ( "tree.growing" == population.flag ) {
        DF.population <- test.correctness_get.population_tree.growing(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
    } else if ( "tree.hierarchy" == population.flag ) {
        DF.population <- test.correctness_get.population_tree.hierarchy(
            seed            = seed,
            population.size = population.size,
            ordered.x1      = ordered.x1,
            ordered.x2      = ordered.x2
            );
        }
    return( DF.population );
    }

test.correctness_get.samples <- function(
    DF.population  = NULL,
    colnames.np    = c("unit.ID","y","x1","x2","x3","x1.jitter","x2.jitter","x3.hidden"),
    colnames.p     = c("unit.ID","x1","x2","x3"),
    prob.selection = 0.1,
    n.replicates   = 500
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.self.selected   <- sapply(
        X   = DF.population[,"true.propensity"],
        FUN = function(x) { sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
        );
    DF.non.probability <- DF.population;
    DF.non.probability[,"self.selected"] <- is.self.selected;
    DF.non.probability <- DF.non.probability[DF.non.probability[,"self.selected"],colnames.np];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection, 1 - prob.selection)
        );

    DF.probability <- DF.population[is.selected,colnames.p];
    DF.probability[,"sampling.fraction"] <- prob.selection;
    DF.probability[,"design.weight"    ] <- 1 / prob.selection;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( n.replicates > 0 ) {

        my.svydesign.object <- survey::svydesign(
            data = DF.probability,
            id   = ~1,
            fpc  = ~sampling.fraction
            );
        cat("\nstr(my.svydesign.object)\n");
        print( str(my.svydesign.object)   );

        my.svrepdesign.object <- survey::as.svrepdesign(
            design     = my.svydesign.object,
            type       = "bootstrap",
            replicates = n.replicates # 500
            );
        cat("\nstr(my.svrepdesign.object)\n");
        print( str(my.svrepdesign.object)   );

        DF.repweights <- my.svrepdesign.object[['repweights']][['weights']];
        DF.repweights <- as.data.frame(DF.repweights);
        colnames(DF.repweights) <- paste0("repweight",seq(1,ncol(DF.repweights)));

        DF.probability <- cbind(
            my.svrepdesign.object[['variables']],
            DF.repweights
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(list(
        DF.non.probability = DF.non.probability,
        DF.probability     = DF.probability
        ));

    }

###################################################
test.correctness_get.population_tree.growing <- function(
    seed            = 1234567,
    population.size = NULL,
    ordered.x1      = TRUE,
    ordered.x2      = TRUE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::set.seed(seed = seed);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(survey);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( ordered.x1 ) {
        levels.x1.hidden <- base::c("small","medium","large");
    } else {
        levels.x1.hidden <- base::c("apple","orange","banana");
        }

    if ( ordered.x2 ) {
        levels.x2.hidden <- base::c("petit","moyen", "grand");
    } else {
        levels.x2.hidden <- base::c("pomme","orange","banane");
        }

    levels.x3.hidden <- base::c("A","B","C");

    x1.hidden <- base::factor(x = base::sample(x = levels.x1.hidden, size = population.size, replace = TRUE), levels = levels.x1.hidden, ordered = ordered.x1);
    x2.hidden <- base::factor(x = base::sample(x = levels.x2.hidden, size = population.size, replace = TRUE), levels = levels.x2.hidden, ordered = ordered.x2);
    x3.hidden <- base::factor(x = base::sample(x = levels.x3.hidden, size = population.size, replace = TRUE), levels = levels.x3.hidden, ordered = FALSE     );

    c1 <- base::as.double(x1.hidden) - 0.5;
    c2 <- base::as.double(x2.hidden) - 0.5;
    c3 <- base::as.double(x3.hidden) - 0.5;
    is.off.diagonals <- (c2 - c1 == 1 | c2 - c1 == -1);

    is.high.propensity.A <- ( (x3.hidden == "A") &   is.off.diagonals  );
    is.high.propensity.B <- ( (x3.hidden == "B") & (!is.off.diagonals) );
    is.high.propensity.C <- ( (x3.hidden == "C") &   is.off.diagonals  );
    is.high.propensity   <- ( is.high.propensity.A | is.high.propensity.B | is.high.propensity.C );

    true.propensity                     <- stats::rnorm(n = population.size,               mean = 2e-1, sd = 1.5e-2);
    true.propensity[is.high.propensity] <- stats::rnorm(n = base::sum(is.high.propensity), mean = 4e-1, sd = 1.5e-2);

    y0 <- base::rep(x = 30, times = population.size);
    y0[is.high.propensity] <- 110;

    epsilon <- rnorm(n = population.size, mean = 0, sd = 1.0)
    y <- y0 + epsilon^2;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.subgroups <- 2;
    levels.x1   <- base::as.vector(base::sapply( X = levels.x1.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));
    levels.x2   <- base::as.vector(base::sapply( X = levels.x2.hidden, FUN = function(x) {return(paste(x,1:n.subgroups,sep="."))} ));

    DF.population <- data.frame(
        unit.ID         = seq(1,population.size),
        y               = y,
        x1.hidden       = x1.hidden,
        x2.hidden       = x2.hidden,
        subgroup.1      = base::sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        subgroup.2      = base::sample(x = 1:n.subgroups, size = population.size, replace = TRUE),
        x3              = x3.hidden,
        x3.hidden       = factor(x = base::as.character(x3.hidden), levels = levels.x3.hidden, ordered = FALSE),
        true.propensity = true.propensity
        );

    DF.population[,'x1'] <- base::apply(
        X      = DF.population[,base::c('x1.hidden','subgroup.1')],
        MARGIN = 1,
        FUN    = function(x) { return(base::paste(x,collapse=".")) }
        );

    DF.population[,'x2'] <- base::apply(
        X      = DF.population[,base::c('x2.hidden','subgroup.2')],
        MARGIN = 1,
        FUN    = function(x) { return(base::paste(x,collapse=".")) }
        );

    DF.population[,'x1'] <- factor(x = DF.population[,'x1'], levels = levels.x1, ordered = ordered.x1);
    DF.population[,'x2'] <- factor(x = DF.population[,'x2'], levels = levels.x2, ordered = ordered.x2);

    DF.population[,"x1.jitter"] <- base::as.double(DF.population[,"x1"]) + stats::runif(n = base::nrow(DF.population), min = -0.3, max = 0.3);
    DF.population[,"x2.jitter"] <- base::as.double(DF.population[,"x2"]) + stats::runif(n = base::nrow(DF.population), min = -0.3, max = 0.3);

    DF.population[,"x3"] <- base::as.double(DF.population[,"x3"]) + base::sample(x = c(-0.1,0.1), size = base::nrow(DF.population), replace = TRUE);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.population <- DF.population[,base::c('unit.ID','y','x1','x2','x3','true.propensity','x1.jitter','x2.jitter','x3.hidden')];
    return( DF.population );

    }

###################################################
test.correctness();
