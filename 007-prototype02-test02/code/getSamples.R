
getSamples <- function(DF.population = NULL, prob.selection = 0.1, inputIsNumeric = FALSE, inputHasFactors = TRUE) {

    # ~~~~~~~~~~ #
    is.selected <- sample(
        x       = c(TRUE,FALSE),
        size    = nrow(DF.population),
        replace = TRUE,
        prob    = c(prob.selection,1-prob.selection)
        );

    if(!inputIsNumeric) {
        DF.probability <- DF.population[is.selected,c("ID","x1","x1.numeric","x1.jitter","x2","x2.numeric","x2.jitter")];
        DF.probability[,"weight"] <- 1 / prob.selection;
        }
    else {
        DF.probability <- DF.population[is.selected,c("ID","x1","x2")];
        DF.probability[,"weight"] <- 1 / prob.selection;
        }

    if(inputHasFactors) {
        DF.probability <- as.data.frame(model.matrix( ~ -1 + . , data = as.data.frame(DF.probability) ))
        DF.probability.names <- colnames(DF.probability[,!colnames(DF.probability) %in% c("ID","x1.numeric","x1.jitter","x2.numeric","x2.jitter","weight")])
        #for(name in DF.probability.names) {
        #    DF.probability[, name] <- as.factor(DF.probability[, name])
        #}
        }

    # ~~~~~~~~~~ #
    DF.non.probability <- DF.population;
    DF.non.probability[,"self.select"] <- sapply(
        X   = DF.non.probability[,"propensity"],
        FUN = function(x) { sample(x=c(0,1),size=1,prob=c(1-x,x)) }
        );
    
    #DF.non.probability[,"self.select"] <- sapply(
    #    X   = DF.non.probability[,"ID"],
    #    FUN = function(x) { if(x %in% DF.probability[,"ID"]) {return(1)} else {return(0)} }
    #    );
    #print(str(DF.non.probability))
    #print(summary(DF.non.probability))

    #DF.rpart           <- DF.non.probability[c("ID","y","self.select","x1","x1.numeric","x1.jitter","x2","x2.numeric","x2.jitter")]
        
    if(!inputIsNumeric) {
        DF.non.probability <- DF.non.probability[1 == DF.non.probability[,"self.select"],c("ID","y","x1","x1.numeric","x1.jitter","x2","x2.numeric","x2.jitter")]
        }
    else {
        DF.non.probability <- DF.non.probability[1 == DF.non.probability[,"self.select"],c("ID","y","x1","x2")]
    }

    if(inputHasFactors) {
        DF.non.probability <- as.data.frame(model.matrix( ~ -1 + . , data = as.data.frame(DF.non.probability) ))
        DF.non.probability.names <- colnames(DF.non.probability[,!colnames(DF.non.probability) %in% c("ID","x1.numeric","x1.jitter","x2.numeric","x2.jitter","y")])
        #for(name in DF.non.probability.names) {
        #    DF.non.probability[, name] <- as.factor(DF.non.probability[, name])
        #}
        }

    # ~~~~~~~~~~ #
    LIST.output <- list(
            probability.sample = DF.probability,
        non.probability.sample = DF.non.probability
                  #rpart.sample = DF.rpart
        );

    return(LIST.output);

    }

