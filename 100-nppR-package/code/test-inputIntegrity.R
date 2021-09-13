
testthat::context('Testing with all valid inputs')
testthat::test_that (
    desc = 'initialize nppCART, with all possible inputs, does not output an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = NA
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with all required inputs, does not output an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target        = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), design.weight = 1  );
        sampling.weight  <- "design.weight";
        testthat::expect_error(
            object = nppCART(
                np.data         = np.data,
                p.data          = p.data,
                sampling.weight = sampling.weight
                ),
            regexp = NA
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the np.data parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL np.data, outputs an error',
    code = {
        np.data          <- NULL;
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1);
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(np.data) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input numeric np.data, outputs an error',
    code = {
        np.data          <- 10;
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1);
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = 'base::is.data.frame(np.data) | base::is.matrix(np.data) | tibble::is_tibble(np.data) is not TRUE',
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input empty data frame np.data, outputs an error',
    code = {
        np.data          <- base::data.frame();
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1);
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = 'base::nrow(np.data) > 0 is not TRUE',
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the p.data parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL p.data, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- NULL;
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(p.data) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input numeric p.data, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- 10;
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = 'base::is.data.frame(p.data) | base::is.matrix(p.data) | tibble::is_tibble(p.data) is not TRUE',
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input empty data frame p.data, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame();
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = 'base::nrow(p.data) > 0 is not TRUE',
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the predictors parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL predictors, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- NULL;
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(predictors) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input numeric predictors, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- 10;
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.character(predictors) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with input empty string predictors, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- "";
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::length(base::setdiff(predictors, base::colnames(np.data))) ==  .... is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with input predictors not in colnames(np.data), outputs an error',
    code = {
        np.data          <- base::data.frame(oof = 1:4, rab = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- base::c("foo", "bar");
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::length(base::setdiff(predictors, base::colnames(np.data))) ==  .... is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with input predictors not in colnames(p.data), outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(oof = 1:4, rab = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- base::c("foo", "bar");
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::length(base::setdiff(predictors, base::colnames(p.data))) ==  .... is not TRUE",
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the weight parameter for errors')
testthat::test_that (
    desc = 'initialize nppCART, with input NULL weight, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- NULL;
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(sampling.weight) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input numeric weight, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- 10;
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.character(sampling.weight) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector weight, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- base::c("weight1", "weight2");
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(base::length(sampling.weight) == 1) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input weight not in colnames(p.data), outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "test";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(sampling.weight %in% base::colnames(p.data)) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector of strings p.data$sampling.weight, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target    = 5:8   );
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), my_weight = "test");
        sampling.weight  <- "my_weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <-  0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.numeric(p.data[, sampling.weight]) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector of zeros p.data$sampling.weight, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target    = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), my_weight = 0  );
        sampling.weight  <- "my_weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <-  0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::all(p.data[, sampling.weight] > 0) is not TRUE",
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the min.cell.size.np parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL min.cell.size.np, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- NULL;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(min.cell.size.np) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input string min.cell.size.np, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- "character_string_inadmissible_as_min.cell.size.np";
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.numeric(min.cell.size.np) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector min.cell.size.np, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- base::c(1, 2, 3);
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(base::length(min.cell.size.np) == 1) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input decimal min.cell.size.np, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 3.14;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(min.cell.size.np == as.integer(min.cell.size.np)) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input zero min.cell.size.np, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 0;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
			      regexp = "(min.cell.size.np > 0) is not TRUE",
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the min.cell.size.p parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL min.cell.size.p, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.p  <- NULL;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.p  = min.cell.size.p,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(min.cell.size.p) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input string min.cell.size.p, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.p  <- "character_string_inadmissible_as_min.cell.size.p";
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.p  = min.cell.size.p,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.numeric(min.cell.size.p) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector min.cell.size.p, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.p  <- base::c(1, 2, 3);
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.p  = min.cell.size.p,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(base::length(min.cell.size.p) == 1) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input decimal min.cell.size.p, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.p  <- 3.14;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.p  = min.cell.size.p,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(min.cell.size.p == as.integer(min.cell.size.p)) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input zero min.cell.size.p, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.p  <- 0;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.p  = min.cell.size.p,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
			      regexp = "(min.cell.size.p > 0) is not TRUE",
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the min.impurity parameter for errors')
testthat::test_that(
    desc = 'initialize nppCART, with input NULL min.impurity, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- NULL;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(min.impurity) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with input string min.impurity, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- "character_inadmissible_as_min.impurity";
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.numeric(min.impurity) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector min.impurity, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- base::c(1, 2, 3);
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(base::length(min.impurity) == 1) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that (
    desc = 'initialize nppCART, with input zero min.impurity, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0;
        n.levels.approx.threshold <- 10;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "min.impurity > 0 is not TRUE",
            fixed  = TRUE
            );
        }
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
testthat::context('Testing the n.levels.approx.threshold parameter for errors');
testthat::test_that(
    desc = 'initialize nppCART, with input NULL n.levels.approx.threshold, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- NULL;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "!base::is.null(n.levels.approx.threshold) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input string n.levels.approx.threshold, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- "character_inadmissible_as_n.levels.approx.threshold";
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "base::is.numeric(n.levels.approx.threshold) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input vector n.levels.approx.threshold, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- base::c(1, 2, 3);
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(base::length(n.levels.approx.threshold) == 1) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input decimal n.levels.approx.threshold, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- 3.14;
        testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(n.levels.approx.threshold == as.integer(n.levels.approx.threshold)) is not TRUE",
            fixed  = TRUE
            );
        }
    );

testthat::test_that(
    desc = 'initialize nppCART, with input negative n.levels.approx.threshold, outputs an error',
    code = {
        np.data          <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), target = 5:8);
        p.data           <- base::data.frame(foo = 1:4, bar = base::factor(base::c("A","B","C","D")), weight = 1  );
        sampling.weight  <- "weight";
        predictors       <- setdiff(base::colnames(p.data),sampling.weight);
        min.cell.size.np <- 10;
        min.impurity     <- 0.095;
        n.levels.approx.threshold <- -1;
				testthat::expect_error(
            object = nppCART(
                np.data          = np.data,
                p.data           = p.data,
                sampling.weight  = sampling.weight,
                predictors       = predictors,
                min.cell.size.np = min.cell.size.np,
                min.impurity     = min.impurity,
                n.levels.approx.threshold = n.levels.approx.threshold
                ),
            regexp = "(n.levels.approx.threshold >= 0) is not TRUE",
            fixed  = TRUE
            );
        }
		);
