
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(cowplot);
require(foreach);
require(ggplot2);
require(parallel);
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
    'test-nppCART-sanity-tree-growing.R',
    'test-nppCART-utils.R',
    'test-svyrepdesign.R',
    'visualizePopulation.R'
    );

for ( file.R in files.R ) {
    source(file.path(code.directory,file.R));
    }

###################################################
###################################################
# test.myCART.grow();
# test.myCART.get.pruned.nodes();
# test.myCART.categorical.predictors();

test.nppCART.sanity.tree.growing(
    seed            = 1234567, # 7654321,
    population.size =   20000  # 10000 # 200
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

###################################################
###################################################
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
