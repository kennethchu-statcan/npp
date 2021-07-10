
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
population.size <- ifelse(test = is.macOS, yes = 5000, no = 40000); # 1000 ==> error
n.simulations   <- ifelse(test = is.macOS, yes =   16, no =   200);
prob.selection  <- as.numeric(pi/20); # 0.1570796,
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
    temp.directory <- file.path(normalizePath(original.directory),"output-population");
if ( !dir.exists(temp.directory) ) { dir.create(temp.directory) }
setwd(temp.directory);

DF.population <- test.nppCART_get.population(
    seed             = global.seed,
    population.flag  = "mixed",
    population.size  = population.size,
    ordered.x1       = FALSE,
    ordered.x2       = TRUE,
    RData.population = "DF-population.RData"
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
original.directory <- getwd();
    temp.directory <- file.path(normalizePath(original.directory),"output-simulations");
if ( !dir.exists(temp.directory) ) { dir.create(temp.directory) }
setwd(temp.directory);

n.cores <- parallel::detectCores();
cat("\nn.cores\n");
print( n.cores   );

doParallel::registerDoParallel(n.cores);

foreach ( iteration.index = seq(1,n.simulations) ) %dopar% {
    iteration.seed <- global.seed + iteration.index;
    test.nppCART.AIC_do.one.simulation(
        seed           = iteration.seed,
        DF.population  = DF.population,
        prob.selection = prob.selection,
        n.replicates   = n.replicates
        );
    }

stopImplicitCluster();

setwd(original.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# original.directory <- getwd();
#     temp.directory <- file.path(normalizePath(original.directory),"output-aggregate");
# if ( !dir.exists(temp.directory) ) { dir.create(temp.directory) }
# setwd(temp.directory);
#
# test.nppCART.AIC_aggregate(
#     simulation.directory = simulation.directory
#     );
#
# setwd(original.directory);

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
