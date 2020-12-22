
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
library(R6);
library(RColorBrewer);
library(rpart);
library(rpart.plot);
library(survey);

source(paste0(code.directory,'/nppCART.R'));
source(paste0(code.directory,'/test-nppCART.R'));
source(paste0(code.directory,'/getPopulation.R'));
source(paste0(code.directory,'/doOneSimulation.R'));
source(paste0(code.directory,'/getSamples.R'));
source(paste0(code.directory,'/getSamples-01.R'));

#source(paste0(code.directory,'/doSimulations.R'));
#source(paste0(code.directory,'/getCalibrationEstimate.R'));
#source(paste0(code.directory,'/getChenLiWuEstimate.R'));
#source(paste0(code.directory,'/visualizePopulation.R'));
#source(paste0(code.directory,'/visualizePropensity.R'));
#source(paste0(code.directory,'/visualizeSimulations.R'));

###################################################
###################################################
my.seed <- 7654321; #1234567;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
test.nppCART();

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

quit();

