
doSimulations <- function(
    FILE.results   = "results-simulations.csv",
    DF.population  = NULL,
    prob.selection =  0.1,
    n.iterations   = 10,
    n.cores        =  4
    ) {

    require(nppR);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    population.totals <- c(
        "(Intercept)" = nrow(DF.population),
        x1            = sum(DF.population[,"x1"]),
        x2            = sum(DF.population[,"x2"])
        );

    Y_total <- sum(DF.population[,"y"]);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    Y_total_hat_propensity  <- NA;
    Y_total_hat_tree        <- NA;
    Y_total_hat_calibration <- NA;
    Y_total_hat_naive       <- NA;
    Y_total_hat_CLW         <- NA;

    cor_propensity_tree <- NA;
    cor_propensity_CLW  <- NA;
    cor_response_CLW    <- NA;

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    n.cores <- base::max(n.cores,1);
    n.cores <- base::min(n.cores,parallel::detectCores());
    doParallel::registerDoParallel(cores = n.cores);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # for (i in seq(1,n.iterations)) {
    foreach::foreach (
        i = seq(1,n.iterations)
        # ,.export    = base::ls(name = base::environment())
        ) %dopar% {

        cat(paste0("\n### iteration: ",i,"\n"));

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        LIST.samples <- getSamples(
            DF.population  = DF.population,
            prob.selection = 0.1
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        nppTree <- nppR::nppCART(
            predictors = c("x1","x2"),
            np.data    = LIST.samples[['non.probability.sample']],
            p.data     = LIST.samples[['probability.sample']],
            weight     = "weight"
            );
        nppTree$grow();

        #print( str(nppTree) );

        nppTree$print(
            FUN.format = function(x) {return( round(x,digits=3) )}
            );

        DF.npdata_with_propensity <- nppTree$get_npdata_with_propensity();
        colnames(DF.npdata_with_propensity) <- gsub(
            x           = colnames(DF.npdata_with_propensity),
            pattern     = "propensity",
            replacement = "p_hat"
            );
        DF.npdata_with_propensity <- merge(
            x  = DF.npdata_with_propensity,
            y  = DF.population[,c("ID","propensity")],
            by = "ID"
            );
        DF.npdata_with_propensity <- DF.npdata_with_propensity[order(DF.npdata_with_propensity[,"ID"]),];

        Y_total_hat_tree <- sum(
            DF.npdata_with_propensity[,"y"] / DF.npdata_with_propensity[,"p_hat"]
            );

        cor_propensity_tree <- cor(
            x = DF.npdata_with_propensity[,"p_hat"],
            y = DF.npdata_with_propensity[,"propensity"]
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.temp <- merge(
            x  = LIST.samples[['non.probability.sample']][,c("ID","y")],
            y  = DF.population[,c("ID","propensity")],
            by = "ID"
            );

        Y_total_hat_propensity <- sum( DF.temp[,"y"] / DF.temp[,"propensity"] );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        Y_total_hat_calibration <- getCalibrationEstimate(
            DF.input          = LIST.samples[['non.probability.sample']],
            population.totals = population.totals
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        naive.factor      <- nrow(DF.population) / nrow(LIST.samples[['non.probability.sample']]);
        Y_total_hat_naive <- naive.factor * sum(LIST.samples[['non.probability.sample']][,"y"]);

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        results.CLW <- getChenLiWuEstimate(
            LIST.input = LIST.samples,
            formula    = y ~ x1 + x2,
            weight     = "weight",
            population = DF.population
            );

        Y_total_hat_CLW    <- results.CLW[["estimate"]];
        cor_propensity_CLW <- results.CLW[["cor.propensity"]];
        cor_response_CLW   <- results.CLW[["cor.response"]];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.temp <- data.frame(
            index                   = i,
            Y_total                 = Y_total,
            Y_total_hat_propensity  = Y_total_hat_propensity,
            Y_total_hat_tree        = Y_total_hat_tree,
            Y_total_hat_calibration = Y_total_hat_calibration,
            Y_total_hat_naive       = Y_total_hat_naive,
            Y_total_hat_CLW         = Y_total_hat_CLW,
            cor_propensity_tree     = cor_propensity_tree,
            cor_propensity_CLW      = cor_propensity_CLW,
            cor_response_CLW        = cor_response_CLW
            );

        FILE.temp <- paste0("tmp-",i,"-",FILE.results);

        write.csv(
            x         = DF.temp,
            file      = FILE.temp,
            row.names = FALSE
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.results <- data.frame(
        index                   = seq(1,n.iterations),
        Y_total                 = as.numeric(rep(NA,n.iterations)),
        Y_total_hat_propensity  = as.numeric(rep(NA,n.iterations)),
        Y_total_hat_tree        = as.numeric(rep(NA,n.iterations)),
        Y_total_hat_calibration = as.numeric(rep(NA,n.iterations)),
        Y_total_hat_naive       = as.numeric(rep(NA,n.iterations)),
        Y_total_hat_CLW         = as.numeric(rep(NA,n.iterations)),
        cor_propensity_tree     = as.numeric(rep(NA,n.iterations)),
        cor_propensity_CLW      = as.numeric(rep(NA,n.iterations)),
        cor_response_CLW        = as.numeric(rep(NA,n.iterations))
        );

    CSV.files <- list.files(
        path    = getwd(),
        pattern = paste0("tmp-[0-9].+",FILE.results)
        );

    for ( CSV.file in CSV.files ) {
        DF.temp   <- read.csv(CSV.file);
        row.index <- DF.temp[1,'index'];
        DF.results[row.index,] <- DF.temp[1,];
        }

    base::unlink(x = CSV.files);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    write.csv(
        x         = DF.results,
        file      = FILE.results,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(DF.results);

    }
