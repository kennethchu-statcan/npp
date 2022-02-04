
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
iteration.data <- list(
    list(file.stem = 'nppCART-alone',       breaks.impurity = seq(0,600,50)),
    list(file.stem = 'Model3-homog-groups', breaks.impurity = seq(0,300,20))
    );

for ( index in seq(1,length(iteration.data)) ) {

    file.stem       <- iteration.data[[index]][['file.stem']];
    breaks.impurity <- iteration.data[[index]][['breaks.impurity']];

    DF.impurity.alpha.AIC <- arrow::read_parquet(
        file = paste0("crowd-LFS-DF-impurities-alphas-AICs-",file.stem,".parquet")
        );

    write.csv(
        file      = paste0("DF-impurities-alphas-AICs-",file.stem,".csv"),
        x         = DF.impurity.alpha.AIC,
        row.names = FALSE
        );

    my.ggplot.impurity.alpha.AIC <- plot.impurity.alpha.AIC(
        DF.input        = DF.impurity.alpha.AIC,
        breaks.impurity = breaks.impurity
        );

    PNG.output <- paste0("plot-impurity-alpha-AIC-",file.stem,".png");
    ggsave(
        filename = PNG.output,
        plot     = my.ggplot.impurity.alpha.AIC,
        dpi      = 300,
        height   =   5,
        width    =  20,
        units    = 'in'
        );

    }

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
