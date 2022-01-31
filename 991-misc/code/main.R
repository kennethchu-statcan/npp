
command.arguments <- commandArgs(trailingOnly = TRUE);
   code.directory <- normalizePath(command.arguments[1]);
 output.directory <- normalizePath(command.arguments[2]);

cat(paste0("##### Sys.time(): ",Sys.time(),"\n"));
start.proc.time <- proc.time();

setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(arrow);
require(cowplot);
require(ggplot2);
require(RColorBrewer);

files.R <- c(
    'initializePlot.R',
    'plot-impurity-alpha-AIC.R'
    );

for ( file.R in files.R ) {
    source(file.path(code.directory,file.R));
    }

##################################################
##################################################
is.macOS <- grepl(x = sessionInfo()[['platform']], pattern = 'apple', ignore.case = TRUE);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.impurity.alpha.AIC <- arrow::read_parquet(
    file = "crowd-LFS-DF-impurities-alphas-AICs.parquet"
    );

my.ggplot.impurity.alpha.AIC <- plot.impurity.alpha.AIC(
    DF.input = DF.impurity.alpha.AIC
    );

PNG.output <- paste0("plot-impurity-alpha-AIC.png");
ggsave(
    filename = PNG.output,
    plot     = my.ggplot.impurity.alpha.AIC,
    dpi      = 300,
    height   =   5,
    width    =  20,
    units    = 'in'
    );

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
