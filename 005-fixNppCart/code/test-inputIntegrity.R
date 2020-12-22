context('Testing with all valid inputs')
test_that (
	'initialize nppCART, with all possible inputs, does not output an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), NA)
	}
)
test_that (
	'initialize nppCART, with all required inputs, does not output an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		weight <- "weight"

		expect_error(R6_nppCART$new(np.data = np.data, p.data = p.data, weight = weight), NA)
	}
)

context('Testing the np.data parameter for errors')
test_that (
	'initialize nppCART, with input NULL np.data, outputs an error',
	{
		np.data <- NULL
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(np.data) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input numeric np.data, outputs an error',
	{
		np.data <- 10
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors),
					 	'is.data.frame(np.data) | is.matrix(np.data) is not TRUE',
					 	fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input empty data frame np.data, outputs an error',
	{
		np.data <- data.frame()
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors),
					 	'nrow(np.data) != 0 is not TRUE',
					 	fixed = TRUE	)
	}
)

context('Testing the p.data parameter for errors')
test_that (
	'initialize nppCART, with input NULL p.data, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- NULL
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(p.data) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input numeric p.data, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- 10
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors),
					 	'is.data.frame(p.data) | is.matrix(p.data) is not TRUE',
					 	fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input empty data frame p.data, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame()
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors),
					 	'nrow(p.data) != 0 is not TRUE',
					 	fixed = TRUE	)
	}
)

context('Testing the predictors parameter for errors')
test_that (
	'initialize nppCART, with input NULL predictors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- NULL
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(predictors) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input numeric predictors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- 10
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.character(predictors) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input empty string predictors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- ""
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"length(setdiff(predictors, colnames(np.data))) == 0 is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input predictors not in colnames(np.data), outputs an error',
	{
		np.data <- data.frame(oof = 1:4, rab = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- c("foo", "bar")
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"length(setdiff(predictors, colnames(np.data))) == 0 is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input predictors not in colnames(p.data), outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(oof = 1:4, rab = c("A","B","C","D"), weight = 1)
		predictors <- c("foo", "bar")
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"length(setdiff(predictors, colnames(p.data))) == 0 is not TRUE", 
						fixed = TRUE	)
	}
)

context('Testing the weight parameter for errors')
test_that (
	'initialize nppCART, with input NULL weight, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- NULL
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(weight) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input numeric weight, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- 10
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.character(weight) & (length(weight) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector weight, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- c("weight1", "weight2")
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.character(weight) & (length(weight) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input weight not in colnames(p.data), outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "test"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"length(setdiff(weight, colnames(p.data))) == 0 is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector of strings p.data$weight, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = "test")
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(p.data$weight) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector of zeros p.data$weight, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 0)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"all(p.data$weight > 0) is not TRUE", 
						fixed = TRUE	)
	}
)

context('Testing the min.cell.size parameter for errors')
test_that (
	'initialize nppCART, with input NULL min.cell.size, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- NULL
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(min.cell.size) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input string min.cell.size, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- "test"
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(min.cell.size) & (length(min.cell.size) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector min.cell.size, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- c(1, 2, 3)
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(min.cell.size) & (length(min.cell.size) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input decimal min.cell.size, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 3.14
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"(min.cell.size%%1 == 0) & (min.cell.size > 0) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input zero min.cell.size, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 0
		min.impurity <- 0.095
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"(min.cell.size%%1 == 0) & (min.cell.size > 0) is not TRUE", 
						fixed = TRUE	)
	}
)

context('Testing the min.impurity parameter for errors')
test_that (
	'initialize nppCART, with input NULL min.impurity, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- NULL
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(min.impurity) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input string min.impurity, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- "test"
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(min.impurity) & (length(min.impurity) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector min.impurity, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- c(1, 2, 3)
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(min.impurity) & (length(min.impurity) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input zero min.impurity, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0
		max.factors <- 10

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"min.impurity > 0 is not TRUE", 
						fixed = TRUE	)
	}
)

context('Testing the max.factors parameter for errors')
test_that (
	'initialize nppCART, with input NULL max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- NULL

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"!is.null(max.factors) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input string max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- "test"

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(max.factors) & (length(max.factors) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input vector max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- c(1, 2, 3)

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"is.numeric(max.factors) & (length(max.factors) == 1) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with input decimal max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 3.14

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"(max.factors%%1 == 0) & (max.factors >= 0) is not TRUE", 
						fixed = TRUE	)
	}
)
test_that (
	'initialize nppCART, with negative input max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- -1

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"(max.factors%%1 == 0) & (max.factors >= 0) is not TRUE", 
						fixed = TRUE	)
	}
)

context('Testing the computed length(self$predictors_factor) for errors')
test_that (
	'initialize nppCART, with more factors than max.factors, outputs an error',
	{
		np.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		p.data <- data.frame(foo = 1:4, bar = c("A","B","C","D"), weight = 1)
		predictors <- colnames(np.data)
		weight <- "weight"
		min.cell.size <- 10
		min.impurity <- 0.095
		max.factors <- 1

		expect_error(	R6_nppCART$new(predictors, np.data, p.data, weight, min.cell.size, min.impurity, max.factors), 
						"length(self$predictors_factor) <= self$max.factors is not TRUE", 
						fixed = TRUE	)
	}
)