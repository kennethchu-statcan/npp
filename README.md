
# Tree-based Inverse Propensity Weighted Estimators

The Tree-based Inverse Propensity Weighted (TrIPW) estimator is intended to be
used as a population total estimator for a population characteristic _Y_ of
interest based on a data set from a non-probability sample (which collects
values of _Y_ of the self-selected units, as well as a number of auxiliary
variables, say, _X1_, _X2_, ... , _Xp_).

The self-selection bias is corrected for by weighting each (self-selected)
unit in the non-probability sample with the reciprocal of its own
__estimated__ self-selection propensity.

The estimation procedure is a CART-inspired algorithm which combines data from
the non-probability sample in question, as well as those from a separate
probability sample.

More precisely, the non-probability and proability sample must share the
``predictor/auxiliary'' variables _X1_, _X2_, ... , _Xp_.
The CART-inspired procedure divides the non-probability sample units in the
(_X1_,_X2_,...,_Xp_)-space, by executing recursive binary partitioning
(as in CART), but it optimizes a modified tree impurity (instead of the usual
tree impurity as used in CART).

## Project Objectives/Deliverables

*	Build an R package that contains the prototype.

	*	Implement all needed input integrity checks.  
	*	Implement necessary unit tests.  
	*	Implement Chen-Li-Wu Doubly Robust estimator (prototype also already
		available).  
	*	Write documentation (manual pages + vignettes).
	*	Check resulting package builds for different platforms (mainly, Windows
		vs Linux).

*	Correct the current deficiency of the prototype in the handling of
	categorical variables.

	*	Currently, the TrIPW can only handle categorical variables
		with at most three levels.
	*	Reimplement the deficient component of the prototype in order to
		allow for an arbitrary number of levels
		(subject to a user-supplied upper limit provided at run time).

*	Investiage the apparent problem of extreme outliers of
	the Chen-Li-Wu Doubly Robust estimator.

	*	The prototype also includes an implemention of the Chen-Li-Wu
		Doubly Rosbust estimator.
		However, in the simulation studies, a small number of extreme outlying
		values, as well as some NA (Not-A-Number) errors, were encountered
		among the Chen-Li-Wu Doubly Robust estimates.  
	*	Investigate the nature of these extreme outliers and NA errors by
		examining the corresponding pairs of non-probability and probability
		samples. In particular, try to ensure that these outliers/errors are
		inherent characteristics of the Chen-Li-Wu method, rather than
		faulty implementations in the current prototype.

*	Write manuscript accompanying release of the R package.

## Learning Objectives

*	Scientific software development in R, including:

	*	construction of an R package,  
	*	implementation of unit tests and input integrity tests,  
	*	writing of documentation (vignettes and help pages),  
	*	testing of R package builds for Windows and Linux, 
	*	etc.

*	The CART algorithm, including:

	*	its implementation,  
	*	how to modify its implementation for the research problem at hand.

*	Basic design-based population total estimators, including:

	*	the unbiasedness of the Horvitz-Thompson estimator,  
	*	the similarity of the TrIPW estimator to the Horvitz-Thompson.

*	How to write a research article, in particular a technical note that
	announces the publication of software tools that provide implementations
	for previously/separately published methodologies.

*	The Chen-Li-Wu Doubly Robust Estimator (time permitting and if desired)

	*	Simple linear regression
	*	Logistic regression, including application of Newton-Raphson
		in the logistic model inference procedure
	*	How Chen-Li-Wu cleverly combines the above two techniques,
		as well as the Horvitz-Thompson estimator to produce their
		Doubly Robust estimator
