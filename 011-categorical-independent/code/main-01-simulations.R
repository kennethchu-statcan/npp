
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(cowplot);
require(foreach);
require(ggplot2);
require(parallel);
require(doParallel);
require(R6);
require(RColorBrewer);
require(rpart);
require(rpart.plot);
require(stringr);
require(survey);

files.R <- c(
    'compute_AIC.R',
    'getPopulation.R',
    'get_pruned_nodes.R',
    'getSamples.R',
    'initializePlot.R',
    'myCART.R',
    'myCART-pruning.R',
    'nppCART.R',
    'test-myCART-categorical-predictors.R',
    'test-myCART-grow.R',
    'test-myCART-get-pruned-nodes.R',
    'test-nppCART-AIC.R',
    'test-nppCART-AIC-aggregate.R',
    'test-nppCART-AIC-graphics.R',
    'test-nppCART-sanity.R',
    'test-nppCART-utils.R',
    'test-svyrepdesign.R',
    'visualizePopulation.R'
    );

for ( file.R in files.R ) {
    source(file.path(code.directory,file.R));
    }

##################################################
##################################################
is.macOS        <- grepl(x = sessionInfo()[['platform']], pattern = 'apple', ignore.case = TRUE);
#population.size<- ifelse(test = is.macOS, yes = 5000, no = 40000); # 1000 ==> error
population.size <- ifelse(test = is.macOS, yes = 5e5, no = 5e6); # 1000 ==> error
n.simulations   <- ifelse(test = is.macOS, yes =  16, no = 200);
prob.selection  <- as.numeric(pi/200); # as.numeric(pi/20); # 0.1570796,
n.replicates    <- 500;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
global.seed <- 1234567;
cat("\nglobal.seed\n");
print( global.seed   );

cat("\npopulation.size\n");
print( population.size   );

cat("\nn.simulations\n");
print( n.simulations   );

cat("\nprob.selection\n");
print( prob.selection   );

cat("\nn.replicates\n");
print( n.replicates   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
   original.directory <- getwd();
 population.directory <- file.path(normalizePath(original.directory),"output-01-population" );
simulations.directory <- file.path(normalizePath(original.directory),"output-02-simulations");
  aggregate.directory <- file.path(normalizePath(original.directory),"output-03-aggregate"  );

RData.population <- "DF-population.RData";

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
if ( !dir.exists(population.directory) ) {

    dir.create(population.directory);
    Sys.sleep(time = 2);
    setwd(population.directory);

    DF.population    <- test.nppCART_get.population(
        seed             = global.seed,
        population.flag  = "independent", # "mixed",
        population.size  = population.size,
        ordered.x1       = FALSE,
        ordered.x2       = TRUE,
        RData.population = RData.population
        );

    cat("\nstr(DF.population)\n");
    print( str(DF.population)   );

    cat("\ntable(DF.population[,c('x1','x2','x3.hidden')])\n");
    print( table(DF.population[,c('x1','x2','x3.hidden')])   );

    visualizePopulation(
        population     = DF.population,
        textsize.title = 30,
        textsize.axis  = 20,
        inputIsNumeric = FALSE
        );

    setwd(original.directory);
    remove(list = "DF.population");
    Sys.sleep(time = 5);

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
if ( !dir.exists(simulations.directory) ) {

    cat("\n\n# Parallel simulations: starts\n\n");

    dir.create(simulations.directory);
    Sys.sleep(time = 5); setwd(simulations.directory); Sys.sleep(time = 5);

    n.cores <- parallel::detectCores();
    cat("\nn.cores\n");
    print( n.cores   );

    doParallel::registerDoParallel(n.cores);

    foreach ( iteration.index = seq(1,n.simulations) ) %dopar% {
        iteration.seed <- global.seed + iteration.index;
        DF.population  <- readRDS(file.path(population.directory,RData.population));
        test.nppCART.AIC_do.one.simulation(
            seed           = iteration.seed,
            DF.population  = DF.population,
            prob.selection = prob.selection,
            n.replicates   = n.replicates
            );
        remove(list = "DF.population");
        }

    stopImplicitCluster();

    Sys.sleep(time = 5); setwd(original.directory); Sys.sleep(time = 5);
    cat("\n\n# Parallel simulations: complete\n\n");

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.population <- readRDS(file.path(population.directory,RData.population));
test.nppCART.AIC_graphics(
    simulations.directory      = simulations.directory,
    DF.population              = DF.population,
    scale_fill_gradient_limits = c(  0,5.5e2),
    scale_fill_gradient_breaks = seq(0,5.0e2,1.0e2)
    );
Sys.sleep(time = 5);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
if ( !dir.exists(aggregate.directory) ) { dir.create(aggregate.directory) }
setwd(aggregate.directory);

DF.population <- readRDS(file.path(population.directory,RData.population));
test.nppCART.AIC_aggregate(
    simulations.directory = simulations.directory,
    n.simulations         = n.simulations,
    DF.population         = DF.population,
    bin.width             = 5e5,           # 3000,
    limits                = c(  0,1e8),    # c(  0,6e6),    # c(  0,2e6),
    breaks                = seq(0,1e8,2e7) # seq(0,6e6,1e6) # seq(0,2e6,5e5)
    );

setwd(original.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

##################################################
##################################################
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

quit(save = "no");
