
buildRPackage <- function(
    package.name       = "ToyRPackage",
    copyright.holder   = "Kenneth Chu",
    description.fields = list(),
    packages.import    = c(),
    packages.depend    = c(),
    packages.suggest   = c(),
    packages.enhance   = c(),
    files.R            = c()
    ) {

    require(usethis);
    require(devtools);
    require(roxygen2);

    # ~~~~~~~~~~ #
    initial.wd   <- normalizePath(getwd());
    path.package <- file.path(initial.wd,package.name);

    # ~~~~~~~~~~ #
    usethis::create_package(
        path    = path.package,
        fields  = description.fields,
        rstudio = FALSE,
        open    = FALSE
        );

    setwd( normalizePath(path.package) );

    # ~~~~~~~~~~ #
    usethis::use_mit_license(name = copyright.holder);
    usethis::use_testthat();

    # ~~~~~~~~~~ #
    for ( temp.package in packages.import ) {
        usethis::use_package(package = temp.package, type = "Imports");
        }

    for ( temp.package in packages.depend ) {
        usethis::use_package(package = temp.package, type = "Depends");
        }

    for ( temp.package in packages.suggest ) {
        usethis::use_package(package = temp.package, type = "Suggests");
        }

    for ( temp.package in packages.enhance ) {
        usethis::use_package(package = temp.package, type = "Enhances");
        }

    # ~~~~~~~~~~ #
    for ( temp.file.R in files.R ) {
        file.copy(
            from = temp.file.R,
            to   = file.path(".","R")
            );
        }

    # ~~~~~~~~~~ #
    devtools::document();

    # ~~~~~~~~~~ #
    setwd(initial.wd);
    return( NULL );

    }

