
test.myCART <- function(
    FILE.results   = "results-simulations.csv",
    n.iterations   = 10,
    DF.population  = NULL,
    prob.selection = 0.1
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    data(iris);
    DF.iris <- get.modified.iris();
    
    #data(stackloss);
    #DF.stackloss <- get.modified.stackloss();    

    #DF.iris <- iris;

    print( str(    DF.iris) );
    print( summary(DF.iris) );
    print( head(   DF.iris) );

    #print( str(    DF.stackloss) );
    #print( summary(DF.stackloss) );
    #print( head(   DF.stackloss) );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    myTree <- myCART$new(
        formula = Species ~ .,
        #formula = stack.loss ~ .,
        data    = DF.iris
        #data    = DF.stackloss
        );
    #print( str(myTree) );

    myTree$grow();
    #cat("\nmyTree$nodes\n" );
    #print( myTree$nodes    );

    myTree$print(
        FUN.format = function(x) {return( round(x,digits=3) )} 
        );

    myPruningSequence <- myTree$get_pruning_sequence();
    print( length(myPruningSequence) );
    print( myPruningSequence );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    results.rpart <- rpart(
        formula = Species ~ .,
        #formula = stack.loss ~ .,
        data    = DF.iris,
        #data    = DF.stackloss,
        control = list(
            minsplit  = 1,
            minbucket = 1,
            cp        = 0
            )
        );

    cat("\nresults.rpart\n");
    print( results.rpart   );
    printcp( x = results.rpart, digits = 3 );

    palette.iris <- brewer.pal(3,"Set1")[c(3,2,1)]; # c("green","blue","red");
    names(palette.iris) <- c("setosa","versicolor","virginica");

    #palette.stackloss <- brewer.pal(3,"Set1")[c(3,2,1)]; # c("green","blue","red");
    #names(palette.stackloss) <- c("small","mediumflow","large");

    palette.iris.light        <- c("#99ff99","#99ccff","#ffad99");
    names(palette.iris.light) <- c("setosa","versicolor","virginica");

    #palette.stackloss.light        <- c("#99ff99","#99ccff","#ffad99");
    #names(palette.stackloss.light) <- c("small","mediumflow","large");

    FILE.ggplot <- "plot-rpart.png";
    png(filename = FILE.ggplot, height = 30, width = 30, units = "in", res = 300);
    prp(
        x           = results.rpart,
        extra       = 101,
        cex         = 3.5, # 3.5,
        legend.cex  = 3.5,
        box.palette = as.list(palette.iris.light)
        #box.palette = as.list(palette.stackloss.light)
        );
    dev.off();

    #rpart(
    #    formula = ,
    #    data
    #    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    #myTree <- myCART$new(
    #    predictors = c("x1","x2"),
    #    np.data    = LIST.samples[['non.probability.sample']],
    #    p.data     = LIST.samples[['probability.sample']],
    #    weight     = "weight"
    #    );
    #myTree$grow();

    #print( str(myTree) );

    #myTree$print(
    #    FUN.format = function(x) {return( round(x,digits=3) )} 
    #    );


    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

###################################
get.modified.stackloss <- function() {

    data(stackloss)

    ####################################################

    DF.stackloss <- stackloss;
    DF.stackloss[,"factor.Air.Flow"] <- character(nrow(DF.stackloss))    
    
    afQuantiles <- quantile(
        x     = DF.stackloss[,"Air.Flow"],
        probs = c(1,2,3,4,5)/5
        );

    is.verylow      <- ifelse(DF.stackloss[,"Air.Flow"] <  afQuantiles[1],TRUE,FALSE);
    is.low          <- ifelse(DF.stackloss[,"Air.Flow"] >= afQuantiles[1] & DF.stackloss[,"Air.Flow"] < afQuantiles[2],TRUE,FALSE);
    is.mediumflow   <- ifelse(DF.stackloss[,"Air.Flow"] >= afQuantiles[2] & DF.stackloss[,"Air.Flow"] < afQuantiles[4],TRUE,FALSE);
    is.high         <- ifelse(DF.stackloss[,"Air.Flow"] >= afQuantiles[4] & DF.stackloss[,"Air.Flow"] < afQuantiles[5],TRUE,FALSE);
    is.veryhigh     <- ifelse(DF.stackloss[,"Air.Flow"] >= afQuantiles[5],TRUE,FALSE);
    
    DF.stackloss[is.verylow, "factor.Air.Flow"]    <- "verylowflow";
    DF.stackloss[is.low, "factor.Air.Flow"]        <- "lowflow";
    DF.stackloss[is.mediumflow,"factor.Air.Flow"]  <- "mediumflow";
    DF.stackloss[is.high,  "factor.Air.Flow"]      <- "highflow";
    DF.stackloss[is.veryhigh, "factor.Air.Flow"]   <- "veryhighflow";

    DF.stackloss[,"factor.Air.Flow"] <- factor(
        x      = DF.stackloss[,"factor.Air.Flow"],
        levels = c("verylowflow","lowflow","mediumflow","highflow","veryhighflow")
        );

    DF.stackloss <- DF.stackloss[,setdiff(colnames(DF.stackloss),"Air.Flow")];
    colnames(DF.stackloss) <- gsub(
        x           = colnames(DF.stackloss),
        pattern     = "factor.Air.Flow",
        replacement = "Air.Flow"
        );
    DF.stackloss <- DF.stackloss[,colnames(stackloss)];

    stackloss <- DF.stackloss;
    remove(DF.stackloss);

    ####################################################

    DF.stackloss <- stackloss;
    DF.stackloss[,"factor.Water.Temp"] <- character(nrow(DF.stackloss))    
    
    wtQuantiles <- quantile(
        x     = DF.stackloss[,"Water.Temp"],
        probs = c(1,2,3,4,5)/5
        );

    is.verylow      <- ifelse(DF.stackloss[,"Water.Temp"] <  wtQuantiles[1],TRUE,FALSE);
    is.low          <- ifelse(DF.stackloss[,"Water.Temp"] >= wtQuantiles[1] & DF.stackloss[,"Water.Temp"] < wtQuantiles[2],TRUE,FALSE);
    is.mediumflow   <- ifelse(DF.stackloss[,"Water.Temp"] >= wtQuantiles[2] & DF.stackloss[,"Water.Temp"] < wtQuantiles[4],TRUE,FALSE);
    is.high         <- ifelse(DF.stackloss[,"Water.Temp"] >= wtQuantiles[4] & DF.stackloss[,"Water.Temp"] < wtQuantiles[5],TRUE,FALSE);
    is.veryhigh     <- ifelse(DF.stackloss[,"Water.Temp"] >= wtQuantiles[5],TRUE,FALSE);
    
    DF.stackloss[is.verylow, "factor.Water.Temp"]    <- "verylowtemp";
    DF.stackloss[is.low, "factor.Water.Temp"]        <- "lowtemp";
    DF.stackloss[is.mediumflow,"factor.Water.Temp"]  <- "mediumtemp";
    DF.stackloss[is.high,  "factor.Water.Temp"]      <- "hightemp";
    DF.stackloss[is.veryhigh, "factor.Water.Temp"]   <- "veryhightemp";

    DF.stackloss[,"factor.Water.Temp"] <- factor(
        x      = DF.stackloss[,"factor.Water.Temp"],
        levels = c("verylowtemp","lowtemp","mediumtemp","hightemp","veryhightemp")
        );

    DF.stackloss <- DF.stackloss[,setdiff(colnames(DF.stackloss),"Water.Temp")];
    colnames(DF.stackloss) <- gsub(
        x           = colnames(DF.stackloss),
        pattern     = "factor.Water.Temp",
        replacement = "Water.Temp"
        );
    DF.stackloss <- DF.stackloss[,colnames(stackloss)];

    stackloss <- DF.stackloss;
    remove(DF.stackloss);

    ####################################################

    DF.stackloss <- stackloss;
    DF.stackloss[,"factor.Acid.Conc."] <- character(nrow(DF.stackloss))    
    
    afQuantiles <- quantile(
        x     = DF.stackloss[,"Acid.Conc."],
        probs = c(1,2,3,4,5)/5
        );

    is.verylow      <- ifelse(DF.stackloss[,"Acid.Conc."] <  afQuantiles[1],TRUE,FALSE);
    is.low          <- ifelse(DF.stackloss[,"Acid.Conc."] >= afQuantiles[1] & DF.stackloss[,"Acid.Conc."] < afQuantiles[2],TRUE,FALSE);
    is.mediumflow   <- ifelse(DF.stackloss[,"Acid.Conc."] >= afQuantiles[2] & DF.stackloss[,"Acid.Conc."] < afQuantiles[4],TRUE,FALSE);
    is.high         <- ifelse(DF.stackloss[,"Acid.Conc."] >= afQuantiles[4] & DF.stackloss[,"Acid.Conc."] < afQuantiles[5],TRUE,FALSE);
    is.veryhigh     <- ifelse(DF.stackloss[,"Acid.Conc."] >= afQuantiles[5],TRUE,FALSE);
    
    DF.stackloss[is.verylow, "factor.Acid.Conc."]    <- "verylowconc";
    DF.stackloss[is.low, "factor.Acid.Conc."]        <- "lowconc";
    DF.stackloss[is.mediumflow,"factor.Acid.Conc."]  <- "mediumconc";
    DF.stackloss[is.high,  "factor.Acid.Conc."]      <- "highconc";
    DF.stackloss[is.veryhigh, "factor.Acid.Conc."]   <- "veryhighconc";

    DF.stackloss[,"factor.Acid.Conc."] <- factor(
        x      = DF.stackloss[,"factor.Acid.Conc."],
        levels = c("verylowconc","lowconc","mediumconc","highconc","veryhighconc")
        );

    DF.stackloss <- DF.stackloss[,setdiff(colnames(DF.stackloss),"Acid.Conc.")];
    colnames(DF.stackloss) <- gsub(
        x           = colnames(DF.stackloss),
        pattern     = "factor.Acid.Conc.",
        replacement = "Acid.Conc."
        );
    DF.stackloss <- DF.stackloss[,colnames(stackloss)];

    stackloss <- DF.stackloss;
    remove(DF.stackloss);

    ####################################################

    DF.stackloss <- stackloss;
    DF.stackloss[,"factor.stack.loss"] <- character(nrow(DF.stackloss))    
    
    slQuantiles <- quantile(
        x     = DF.stackloss[,"stack.loss"],
        probs = c(1,2,3)/3
        );

    is.small        <- ifelse(DF.stackloss[,"stack.loss"] <  slQuantiles[1],TRUE,FALSE);
    is.mediumloss   <- ifelse(DF.stackloss[,"stack.loss"] >= slQuantiles[1] & DF.stackloss[,"stack.loss"] < slQuantiles[3],TRUE,FALSE);
    is.large        <- ifelse(DF.stackloss[,"stack.loss"] >= slQuantiles[3],TRUE,FALSE);
    
    DF.stackloss[is.small, "factor.stack.loss"]      <- "smallloss";
    DF.stackloss[is.mediumloss,"factor.stack.loss"]  <- "mediumloss";
    DF.stackloss[is.large,  "factor.stack.loss"]     <- "largeloss";

    DF.stackloss[,"factor.stack.loss"] <- factor(
        x      = DF.stackloss[,"factor.stack.loss"],
        levels = c("smallloss","mediumloss","largeloss")
        );

    DF.stackloss <- DF.stackloss[,setdiff(colnames(DF.stackloss),"stack.loss")];
    colnames(DF.stackloss) <- gsub(
        x           = colnames(DF.stackloss),
        pattern     = "factor.stack.loss",
        replacement = "stack.loss"
        );
    DF.stackloss <- DF.stackloss[,colnames(stackloss)];

    stackloss <- DF.stackloss;
    remove(DF.stackloss);

    ####################################################

    return( stackloss );
}


###################################
get.modified.iris <- function() {

    data(iris);

    ####################################################

    DF.iris <- iris;
    DF.iris[,"factor.Sepal.Length"] <- character(nrow(DF.iris))    
    
    plQuantiles <- quantile(
        x     = DF.iris[,"Sepal.Length"],
        probs = c(1,2,3,4,5,6,7)/7
        );

    is.shortest     <- ifelse(DF.iris[,"Sepal.Length"] <  plQuantiles[1],TRUE,FALSE);
    is.veryshort    <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[1] & DF.iris[,"Sepal.Length"] < plQuantiles[2],TRUE,FALSE);
    is.short        <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[2] & DF.iris[,"Sepal.Length"] < plQuantiles[3],TRUE,FALSE);
    is.mediumlength <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[3] & DF.iris[,"Sepal.Length"] < plQuantiles[5],TRUE,FALSE);
    is.long         <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[5] & DF.iris[,"Sepal.Length"] < plQuantiles[6],TRUE,FALSE);
    is.verylong     <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[6] & DF.iris[,"Sepal.Length"] < plQuantiles[7],TRUE,FALSE);
    is.longest      <- ifelse(DF.iris[,"Sepal.Length"] >= plQuantiles[7],TRUE,FALSE);

    DF.iris[is.shortest, "factor.Sepal.Length"]     <- "shortest";
    DF.iris[is.veryshort, "factor.Sepal.Length"]    <- "veryshort";
    DF.iris[is.short, "factor.Sepal.Length"]        <- "short";
    DF.iris[is.mediumlength,"factor.Sepal.Length"]  <- "mediumlength";
    DF.iris[is.long,  "factor.Sepal.Length"]        <- "long";
    DF.iris[is.verylong, "factor.Sepal.Length"]     <- "verylong";
    DF.iris[is.longest, "factor.Sepal.Length"]      <- "longest";

    DF.iris[,"factor.Sepal.Length"] <- factor(
        x      = DF.iris[,"factor.Sepal.Length"],
        levels = c("shortest", "veryshort","short","mediumlength","long","verylong", "longest")
        );

    DF.iris <- DF.iris[,setdiff(colnames(DF.iris),"Sepal.Length")];
    colnames(DF.iris) <- gsub(
        x           = colnames(DF.iris),
        pattern     = "factor.Sepal.Length",
        replacement = "Sepal.Length"
        );
    DF.iris <- DF.iris[,colnames(iris)];

    iris <- DF.iris;
    remove(DF.iris);

    ####################################################

    DF.iris <- iris;
    DF.iris[,"factor.Petal.Width"] <- character(nrow(DF.iris))
    
    swQuantiles <- quantile(
        x     = DF.iris[,"Petal.Width"],
        probs = c(1,2,3,4,5)/5
        );

    is.verynarrow   <- ifelse(DF.iris[,"Petal.Width"] <  swQuantiles[1],TRUE,FALSE);
    is.narrow       <- ifelse(DF.iris[,"Petal.Width"] >= swQuantiles[1] & DF.iris[,"Petal.Width"] < swQuantiles[2],TRUE,FALSE);
    is.mediumwidth  <- ifelse(DF.iris[,"Petal.Width"] >= swQuantiles[2] & DF.iris[,"Petal.Width"] < swQuantiles[4],TRUE,FALSE);
    is.wide         <- ifelse(DF.iris[,"Petal.Width"] >= swQuantiles[4] & DF.iris[,"Petal.Width"] < swQuantiles[5],TRUE,FALSE);
    is.verywide     <- ifelse(DF.iris[,"Petal.Width"] >= swQuantiles[5],TRUE,FALSE);
    
    DF.iris[is.verynarrow, "factor.Petal.Width"]    <- "verynarrow";
    DF.iris[is.narrow, "factor.Petal.Width"]        <- "narrow";
    DF.iris[is.mediumwidth,"factor.Petal.Width"]    <- "mediumwidth";
    DF.iris[is.wide,  "factor.Petal.Width"]         <- "wide";
    DF.iris[is.verywide, "factor.Petal.Width"]      <- "verywide";

    DF.iris[,"factor.Petal.Width"] <- factor(
        x      = DF.iris[,"factor.Petal.Width"],
        levels = c("verynarrow","narrow","mediumwidth","wide","verywide")
        );

    DF.iris <- DF.iris[,setdiff(colnames(DF.iris),"Petal.Width")];
    colnames(DF.iris) <- gsub(
        x           = colnames(DF.iris),
        pattern     = "factor.Petal.Width",
        replacement = "Petal.Width"
        );
    DF.iris <- DF.iris[,colnames(iris)];

    iris <- DF.iris;
    remove(DF.iris);

    ####################################################

    return( iris );

    }
