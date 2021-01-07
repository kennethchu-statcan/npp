
getSamples <- function(
    DF.population   = NULL,
    prob.selection  = 0.1,
    inputIsNumeric  = FALSE,
    inputHasFactors = TRUE
    ) {

    # ~~~~~~~~~~ #
    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection,1-prob.selection)
        );

    if( inputIsNumeric ) {
        DF.probability <- DF.population[is.selected,c("ID","x1","x2")];
    } else {
        DF.probability <- DF.population[is.selected,c("ID","x1","x1.numeric","x1.jitter","x2","x2.numeric","x2.jitter")];
        }

    DF.probability[,"weight"] <- 1 / prob.selection;

    if(inputHasFactors) {
        DF.probability <- as.data.frame(model.matrix( ~ -1 + . , data = as.data.frame(DF.probability) ));
        }

    # ~~~~~~~~~~ #
    DF.non.probability <- DF.population;
    DF.non.probability[,"is.self.selected"] <- sapply(
        X   = DF.non.probability[,"true.propensity"],
        FUN = function(x) { sample(x = c(FALSE,TRUE), size = 1, prob = c(1-x,x)) }
        );

    if( inputIsNumeric ) {
        DF.non.probability <- DF.non.probability[DF.non.probability[,"is.self.selected"],c("ID","y","x1","x2")]
    } else {
        DF.non.probability <- DF.non.probability[DF.non.probability[,"is.self.selected"],c("ID","y","x1","x1.numeric","x1.jitter","x2","x2.numeric","x2.jitter")]
        }

    if( inputHasFactors ) {
        DF.non.probability <- as.data.frame(model.matrix( ~ -1 + . , data = as.data.frame(DF.non.probability) ))
        }

    # ~~~~~~~~~~ #
    LIST.output <- list(
            probability.sample = DF.probability,
        non.probability.sample = DF.non.probability
        );

    return(LIST.output);

    }
