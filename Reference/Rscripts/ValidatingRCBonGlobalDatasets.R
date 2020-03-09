Validating RCB ANOVA calculations in Velocity


A 1,160 datasets were analyzed in the production environment in Velocity.
The input data and results were read into the development environment in Domino running
R-Studio.  The input data were reanalyzed using the development code in Domino.
Then the results from the two analyses were compared.

The output from a single input dataset consists of five tables:
a table of estimates of the average response for each level of the treatment factor,
a table of the differences between the treatment level average response estimates,
an analysis of variance table for the treatment factor,
a table of variance components,
and an estimate of the least-significant-difference for a single alpha level.

The tables have several columns of information.
The tables of the estimated averages of the treatment level response and
the table of differences both have columns for the standard error of the estimate,
the lower and upper confidence interval limits,
the degrees-of-freedom associated with the standard error estimate.
The analysis-of-variance table also has several columns of numbers,
the variance component data has a single column of numbers and
the least-significant-difference is a single number.

To compare the results, the corresponding columns of like tables were subtracted from
each other and the absolute values of the difference taken.
The sum of the absolute differences round to ten decimal places were used as a measure of
how well the column values match.  These totals were summed to give a general idea of
how well the two tables compared.  This was done for all five outputs from each analysis
pair from each dataset.

This was done for 1,160 global datasets.  The sum across these datasets are given next for
each output.  As can be seen, with the exception of variance components, the total
comparison are 0.  The Variance components are the same out to 7-8 decimal places.
In effect, the total difference across 5 outputs from 1,160 datasets is 0 out at
least 7 decimal places.

compareLSM  compareDeltas compareVarComp   compareAnova     compareLSD
0.00e+00       0.00e+00       3.38e-08       0.00e+00       0.00e+00



