
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(foreach);
require(parallel);
require(R6);
require(RColorBrewer);
require(rpart);
require(rpart.plot);
require(survey);

files.R <- c(
    'getPopulation.R',
    'getSamples.R',
    'myCART.R',
    'myCART-pruning.R',
    'nppCART.R',
    'test-myCART.R',
    'test-nppCART.R',
    'test-svyrepdesign.R'
    );

for ( file.R in files.R ) {
    source(file.path(code.directory,file.R));
    }

###################################################
###################################################
# test.svyrepdesign();

# test.myCART();

test.nppCART(seed = 7654321);

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
