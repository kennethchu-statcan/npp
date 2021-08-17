011-categorical-independent
===========================

Hastie-Tibshirani-Friedman, in Section 9.2.4 of
The Elements of Statistical Learning (2009),
mentioned that, when the response variable is (non-ordered) binary, a simplified
best-split search strategy is known for (non-ordered) categorical predictor
variables. This task contains experiments to see whether it is possible to adapt
that simplified best-split search strategy for nppCART.

This computational experiment is a modification of 010-categorical-predictors,
in that the target variable (y) in the synthetic population is now independent
of the self-selection propensity as well as the predictor variables..

