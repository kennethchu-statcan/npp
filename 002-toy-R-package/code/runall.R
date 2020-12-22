
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);
     package.name <- command.arguments[3];

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
library(R6);

source(paste0(code.directory,'/buildRPackage.R'));

###################################################
###################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
description.fields <- list(
    `Authors@R` = 'person(
        given   = "Kenneth",
        family  = "Chu",
        email   = "kenneth.chu@canada.ca",
        role    = "cre",
        comment = c(ORCID = "https://orcid.org/0000-0002-0270-4752")
        )',
    Language = "en"
    );

packages.import <- c(
    "testthat",
    "R6",
    "dplyr",
    "ggplot2"
    );

packages.suggest <- c(
    "ComplexHeatmap",
    "caret"
    );

files.R <- c(
    "dummyAdd.R"
    );

files.R <- file.path( code.directory , files.R );

buildRPackage(
    package.name       = package.name,
    copyright.holder   = "Kenneth Chu",
    description.fields = description.fields,
    packages.import    = packages.import,
    packages.suggest   = packages.suggest,
    files.R            = files.R
    );

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

