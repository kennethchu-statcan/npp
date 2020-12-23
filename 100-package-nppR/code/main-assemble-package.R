
command.arguments <- base::commandArgs(trailingOnly = TRUE);
   code.directory <- base::normalizePath(command.arguments[1]);
 output.directory <- base::normalizePath(command.arguments[2]);
     package.name <- command.arguments[3];

base::cat(base::paste0("##### Sys.time(): ",base::Sys.time(),"\n"));
start.proc.time <- base::proc.time();

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
base::setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
base::require(foreach);
base::require(logger);
base::require(R6);
base::source(base::file.path(code.directory,'assemble-package.R'));
base::source(base::file.path(code.directory,'build-package.R'));

###################################################
###################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
string.authors <- "base::c(
    person(
    	given   = 'Jean-François',
        family  = 'Beaumont',
        email   = 'jean-francois.beaumont@canada.ca',
        role    = 'aut'
        ),
    person(
        given   = 'Kenneth',
        family  = 'Chu',
        email   = 'kenneth.chu@canada.ca',
        role    = 'cre',
        comment = c(ORCID = 'https://orcid.org/0000-0002-0270-4752')
        ),
    person(
        given   = 'Janahan',
        family  = 'Dhushenthen',
        email   = 'janahan.dhushenthen@canada.ca',
        role    = 'aut',
        )
    )";
base::Encoding(string.authors) <- "UTF-8";

description.fields <- base::list(
    Title           = "Inference on non-probability sample data via integrating probabilty sample data",
    Version         = "1.1.003",
    `Authors@R`     = string.authors,
    Description     = "This package provides a collection of tools for making inference based on non-probability sample data by integrating auxiliary probability sample data.",
    Language        = "fr",
    VignetteBuilder = "R.rsp"
    );

packages.import <- base::c(
    "dplyr",
    "R6"
    # "base",
    # "doParallel",
    # "dplyr",
    # "foreach",
    # "ggplot2",
    # "jsonlite",
    # "logger",
    # "magrittr",
    # "rlang",
    # "R6",
    # "stats",
    # "stringi",
    # "stringr",
    # "tools",
    # "utils",
    # "xgboost"
    );

packages.suggest <- base::c(
    "caret",
    "ComplexHeatmap",
    "ggplot2",
    "png",
    "R.rsp",
    "testthat"
    # "fs",
    # "knitr",
    # "rmarkdown",
    # "testthat"
    );

files.R <- base::c("getChenLiWuEstimate.R", "nppCART.R", "package-init.R");
files.R <- base::file.path( code.directory , files.R );

# files.R <- base::c(
#     "crop-yield-predict.R",
#     "crop-yield-train-model.R",
#     "get-learner-metadata.R",
#     "get-mock-production-errors.R",
#     "get-performance-metrics.R",
#     "getData-synthetic.R",
#     "getLearner.R",
#     "initializePlot.R",
#     "input-validity-checks-learner-metadata.R",
#     "input-validity-checks-parameters.R",
#     "input-validity-checks-predict.R",
#     "input-validity-checks-variables.R",
#     "input-validity-checks-window-compatibility.R",
#     "learner-abstract.R",
#     "learner-byGroup.R",
#     "learner-multiphase.R",
#     "learner-xgboost.R",
#     "package-init.R",
#     "preprocessor.R",
#     "rollingWindowForwardValidation.R",
#     "validation-single-year.R",
#     "weighted-statistics.R"
#     );
# files.R <- base::file.path( code.directory , files.R );

tests.R <- base::c("test-inputIntegrity.R");
tests.R <- base::file.path( code.directory , tests.R );

# tests.R <- base::c(
#     "test-correctness.R"
#     );
# tests.R <- base::file.path( code.directory , tests.R );

# list.vignettes.Rmd <- list(
#     'rwFV-protocol' = list(
#         file  = base::file.path( code.directory , 'vignette-rwFV-protocol.Rmd'       ),
#         asis  = base::file.path( code.directory , 'vignette-rwFV-protocol.html.asis' )
#         ),
#     'rwFV-demo-production' = list(
#         file  = base::file.path( code.directory , 'vignette-rwFV-demo-production.Rmd'       ),
#         asis  = base::file.path( code.directory , 'vignette-rwFV-demo-production.html.asis' )
#         ),
#     'rwFV-xgboost' = list(
#         file  = base::file.path( code.directory , 'vignette-rwFV-xgboost.Rmd'      ),
#         asis  = base::file.path( code.directory , 'vignette-rwFV-xgboost.html.asis' )
#         )
#     );

list.vignettes.pdf <- list(
    'BeaumontChu2019' = list(
        file  = base::file.path( code.directory , 'Beaumont-Chu_SSC-2019_nppCART.pdf'      ),
        asis  = base::file.path( code.directory , 'Beaumont-Chu_SSC-2019_nppCART.pdf.asis' )
        )
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# We will build the package twice.
# The package is built without vignettes the first time.
# The package is then temporarily installed using the resulting
# without-vignettes package tarball to a temporary R library.
# The package is then built a second time, this time with vignette construction.
#
# This is because we are using pre-built (more precisely, 'asis') vignettes,
# whose construction requires that the package have been installed.
# Hence, whenever the package is not installed, a package build attempt
# will fail at the vignette construction.
# We overcome this problem by building the package twice, first without
# vignettes, followed by a second time with vignettes.
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
write.to.directory <- "build-no-vignettes";

package.path <- assemble.package(
    write.to.directory = write.to.directory,
    package.name       = package.name,
    copyright.holder   = "Kenneth Chu",
    description.fields = description.fields,
    packages.import    = packages.import,
    packages.suggest   = packages.suggest,
    files.R            = files.R,
    tests.R            = tests.R
    );

build.package(
    write.to.directory = write.to.directory,
    package.path       = package.path
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# temp.RLib <- "temp-RLib";
#
# if ( !dir.exists(temp.RLib) ) {
#     dir.create(path = temp.RLib, recursive = TRUE);
#     }
#
# .libPaths(unique(c(temp.RLib,.libPaths())));
#
# package.directory <- base::dirname(package.path);
# package.file      <- base::list.files(path = package.directory, pattern = "\\.tar\\.gz")[1];
# package.file      <- file.path(package.directory,package.file);
#
# install.packages(
#     pkgs  = package.file,
#     lib   = temp.RLib,
#     repos = NULL
#     );
#
# cat("\ntemp.RLib\n");
# print( temp.RLib   );
#
# cat("\nnormalizePath(temp.RLib)\n");
# print( normalizePath(temp.RLib)   );
#
# cat("\nlist.files(temp.RLib)\n");
# print( list.files(temp.RLib)   );
#
# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# set.seed(13);
#
# n.ecoregions    <-    3;
# n.crops         <-    5;
# n.predictors    <-    7;
# avg.n.parcels   <- 1000;
# min.num.parcels <-   50;
#
# DF.synthetic <- stcCropYield::getData.synthetic(
#     years         = seq(2000,2020),
#     n.ecoregions  = n.ecoregions,
#     n.crops       = n.crops,
#     n.predictors  = n.predictors,
#     avg.n.parcels = avg.n.parcels
#     );
#
# stcCropYield::rollingWindowForwardValidation(
#     training.window      = 5,
#     validation.window    = 5,
#     DF.input             = DF.synthetic,
#     year                 = "my_year",
#     ecoregion            = "my_ecoregion",
#     crop                 = "my_crop",
#     response.variable    = "my_yield",
#     harvested.area       = "my_harvested_area",
#     seeded.area          = "my_seeded_area",
#     evaluation.weight    = "my_evaluation_weight",
#     predictors           = grep(x = colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
#     min.num.parcels      = min.num.parcels,
#     learner              = "xgboost_multiphase",
#     by.variables.phase01 = c("my_ecoregion","my_crop"),
#     by.variables.phase02 = c("my_crop"),
#     by.variables.phase03 = c("my_ecoregion"),
#     search.grid = list(
#         alpha  = c(1,12,23),
#         lambda = c(1,12,23)
#         ),
#     output.directory = file.path(code.directory,"rwFV"),
#     log.threshold    = logger::TRACE
#     );
#
# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
if ( "windows" != base::.Platform[["OS.type"]] ) {

    write.to.directory <- "build-vignettes";

    package.path <- assemble.package(
        write.to.directory = write.to.directory,
        package.name       = package.name,
        copyright.holder   = "Kenneth Chu",
        description.fields = description.fields,
        packages.import    = packages.import,
        packages.suggest   = packages.suggest,
        files.R            = files.R,
        tests.R            = tests.R,
      # list.vignettes.Rmd = list.vignettes.Rmd,
        list.vignettes.pdf = list.vignettes.pdf
        );

    build.package(
        write.to.directory = write.to.directory,
        package.path       = package.path
        );

    }

###################################################
###################################################
# print warning messages to log
base::cat("\n##### warnings()\n")
base::print(base::warnings());

base::cat("\n##### getOption('repos'):\n");
base::print(       getOption('repos')    );

base::cat("\n##### .libPaths():\n");
base::print(       .libPaths()    );

# print session info to log
base::cat("\n##### sessionInfo()\n")
base::print( utils::sessionInfo() );

# print system time to log
base::cat(base::paste0("\n##### Sys.time(): ",base::Sys.time(),"\n"));

# print elapsed time to log
stop.proc.time <- base::proc.time();
base::cat("\n##### start.proc.time() - stop.proc.time()\n");
base::print( stop.proc.time - start.proc.time );

base::quit(save="no");
