
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

copyright.holder <- "His Majesty the King in Right of Canada, as represented by the Minister of Statistics Canada";

description.fields <- base::list(
    Title           = "Inference on non-probability sample data via integrating probabilty sample data",
    Version         = "1.12.001",
    `Authors@R`     = string.authors,
    Description     = "This package provides a collection of tools for making inference based on non-probability sample data by integrating auxiliary probability sample data.",
    License         = 'MIT + file LICENCE',
    Language        = "fr",
    VignetteBuilder = "R.rsp"
    );

packages.import <- base::c(
    "R6",
    "survey",
    "tibble"
    # "base",
    # "doParallel",
    # "dplyr",
    # "foreach",
    # "ggplot2",
    # "jsonlite",
    # "logger",
    # "magrittr",
    # "rlang",
    # "stats",
    # "stringi",
    # "stringr",
    # "tools",
    # "utils",
    # "xgboost"
    );

packages.suggest <- base::c(
    "ggplot2",
    "rmarkdown",
    "rpart",
    "R.rsp",
    "testthat"
    # "caret",
    # "ComplexHeatmap",
    # "png",
    # "fs",
    # "knitr"
    );


FILE.licence <- base::file.path( code.directory , "LICENCE" );

files.R <- base::c(
    "get-npp-samples.R",
    "get-synthetic-population.R",
    "getChenLiWuEstimate.R",
    "nppCART.R",
    "package-init.R"
    );
files.R <- base::file.path( code.directory , files.R );

tests.R <- base::c(
    "test-inputIntegrity.R",
    "test-correctness.R"
    );
tests.R <- base::file.path( code.directory , tests.R );

scripts.py <- base::c(
    "sklearn-impurity-alpha.py"
    );
scripts.py <- base::file.path( code.directory , scripts.py );

list.vignettes.Rmd <- list(
    'nppCART-usage-no-pruning' = list(
        file  = base::file.path( code.directory , 'nppCART-usage-no-pruning.Rmd'       ),
        asis  = base::file.path( code.directory , 'nppCART-usage-no-pruning.html.asis' )
        ),
    'nppCART-usage-with-pruning' = list(
        file  = base::file.path( code.directory , 'nppCART-usage-with-pruning.Rmd'       ),
        asis  = base::file.path( code.directory , 'nppCART-usage-with-pruning.html.asis' )
        )
    );

list.vignettes.pdf <- list(
    'nppCART-article' = list(
        file  = base::file.path( code.directory , 'nppCART-article.pdf'      ),
        asis  = base::file.path( code.directory , 'nppCART-article.pdf.asis' )
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
    copyright.holder   = copyright.holder,
    FILE.licence       = FILE.licence,
    description.fields = description.fields,
    packages.import    = packages.import,
    packages.suggest   = packages.suggest,
    files.R            = files.R,
    tests.R            = tests.R,
    scripts.py         = scripts.py
    );

build.package(
    write.to.directory = write.to.directory,
    package.path       = package.path
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
temp.RLib <- "temp-RLib";

if ( !dir.exists(temp.RLib) ) {
    dir.create(path = temp.RLib, recursive = TRUE);
    }

.libPaths(unique(c(temp.RLib,.libPaths())));

package.directory <- base::dirname(package.path);
package.file      <- base::list.files(path = package.directory, pattern = "\\.tar\\.gz")[1];
package.file      <- file.path(package.directory,package.file);

install.packages(
    pkgs  = package.file,
    lib   = temp.RLib,
    repos = NULL
    );

cat("\ntemp.RLib\n");
print( temp.RLib   );

cat("\nnormalizePath(temp.RLib)\n");
print( normalizePath(temp.RLib)   );

cat("\nlist.files(temp.RLib)\n");
print( list.files(temp.RLib)   );

# ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
if ( "windows" != base::.Platform[["OS.type"]] ) {

    write.to.directory <- "build-vignettes";

    package.path <- assemble.package(
        write.to.directory = write.to.directory,
        package.name       = package.name,
        copyright.holder   = copyright.holder,
        FILE.licence       = FILE.licence,
        description.fields = description.fields,
        packages.import    = packages.import,
        packages.suggest   = packages.suggest,
        files.R            = files.R,
        tests.R            = tests.R,
        scripts.py         = scripts.py,
        list.vignettes.Rmd = list.vignettes.Rmd,
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
