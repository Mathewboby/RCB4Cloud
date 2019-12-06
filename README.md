# Phase I, single factor random complete block design (RCBD1) model is implemented.

Model fitting is conducted using restricted maximum likelihood or REML for short.  In the twentieth century such model fitting was done using least-squares.  All of the variants of maximum likelihood fitting are nonlinear optimization algorithms that iterate from an initial guess at the model coefficients to the final set of coefficients.  On the other hand least-squares requires only one iteration because its final set of coefficients can be solved for analytically.  This simplicity is a result of more restrictive assumptions on the form of the distribution of the response variable.  What this means is that sometimes the maximum likelihood method does not find a solution or set of final model coefficients or it takes a very long time to find a final set of coefficients because it initial guess was not very good for dataset being analyzed.  In such cases the output described below is not generated.

To do the analysis, only data which is NOT flagged by either DSR or QAQC is used in the analysis.  There are five possible variations of the RCBD1 model: there are three types of blocking and two estimation methods.  This would produce six combinations of blocking and estimation, but one of the combinations is never used. The five models are selected by giving the input argument, analysistype, one of the following strings, “P1”, “P2”, “P3”, “P4”, and “P5”.

#### P1 calls for using BLUE estimation on a single blocking factor RCBD1 model with replicates as the random blocking factor.
#### P2 calls for using BLUE estimation on a single blocking factor RCBD1 model with subsites as the random blocking factor.
#### P3 calls for using BLUP estimation on a single blocking factor RCBD1 model with subsites as the blocking factor. To obtain BLUP estimates, the treatment factor is also treated as a random factor.
#### P4 calls for using BLUE estimation on a two-blocking factor RCBD1 model with both subsites and replicates as the random blocking factors.
#### P5 calls for using BLUP estimation on a two-blocking factor RCBD1 model with both subsites and replicates as the random blocking factors. To obtain BLUP estimates, the treatment factor is also treated as a random factor.

The output from the module is a list of three to four tables depending on which estimation method was used.

Under BLUE estimation 
There is a table of the Treatment level mean response with level sample sizes, degrees of freedom, standard errors and confidence intervals.  
    "blueTable": [
    {
      "factorLevelId": "SS8027",
      "value": "24.6367687770914",
      "standardError": "1.64621386967149",
      "degreesFreedom": "35.8",
      "count": "14",
      "lowerConfidenceInterval": "21.8570595881111",
      "upperConfidenceInterval": "27.4164779660716",
      "meanSeparationGroup": "a"
    },}
    

A similar table of all possible differences between the treatment level mean estimates minus the column od sample sizes.  

An analysis-of-variance table for the treatment factor and a table of the variance components estimates are both provided.  The analysis-of-variance table contains the formal statistical test for any significant differences between the treatment levels.

	degreesFreedomNumerator | degreesFreedomDenominator | ssIncremental | ssConditional | margin | probability
(Intercept)	1	2	125.2	125.2	 	0.008090887
SeedProductName	5	29.2	0.4101	0.4101	A	0.837816962
The variance component table contains the estimates of the variances associated with each random term in the model.  For models P1 and P2 this will be two numbers, one for the variance of the blocking factor and one for the residual variance; for model P4, this will contain four numbers: one for subsites, one for reps within subsites, one for treatments by subsite interaction and one for the residual variance.

Under BLUP estimation, 

The table of treatment level mean estimates is provided as in the BLUE case but without the degrees-of-freedom, standard errors and confidence intervals.  No table of differences is created nor is there an analysis-of-variance table.  

The variance component table also includes a variance estimate for the treatment factor.


...
