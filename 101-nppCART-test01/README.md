101-nppCART-test01
=========
This task is a test suite to test the functionality of nppR::nppCART()
for handling continuous predictor variables. 

This task also demonstrates the behaviour of nppR::nppCART() via three
Monte Carlo simulation studies, by repeatedly applying TrIPW, as well as
a number of competing techniques, to pairs of
non-probability and probability samples taken from three
different synthetic populations (with increasingly relaxed linearity
assumptions).
The effectiveness of these different techniques, at least in the three
specific synthetic scenarios, can thus be compared.

How to execute the pipeline
---------------------------
### Clone this project if you have not already done so:

*    In a command line window, change directory to the following 
```
//fld6filer/icmic-cciim/DSCoE/300-projects/2019-2020/001-TrIPW/410-git-personal/
```

*    Clone the project by running the following at the command line:
```
git clone https://gccode.ssc-spc.gc.ca/STC-DSCoE/2019-2020/TrIPW.git <your_username>
```

### Change directory to the folder of this pipeline in the local cloned repository:
```
cd <LOCAL CLONED REPOSITORY>/001-TrIPW-prototype/<your_username>
```

### Execute the following batch file (in order to run the full pipeline):
```
.\runall.bat
```
This will trigger the creation of the output folder

__OUTDIR__ = `\\fld6filer\icmic-cciim\DSCoE\300-projects\2019-2020\001-TrIPW\600-temp\<your_username>\001-TrIPW-prototype\output\`

if it does not already exist.
See below for information about the contents of the output folder.

Details of the simulation studies
---------------------------------
The `runall` of this task demonstrates the behaviour of TrIPW by performing
the following:

*	Generate three synthetic populations:

	*   Population 01  
		Size: 10,000  
		Self-selection propensity = a logistic function of X1 and X2  
		Target variable Y = affine function of X1 and X2 + Gaussian noise

	*   Population 02  
		Size: 10,000  
		Self-selection propensity = a logistic function of X1 and X2  
		Target variable Y = affine function of X1 and X2 + non-Gaussian noise

	*   Population 03  
		Size: 10,000  
		Self-selection propensity = nonlinear function of X1 and X2  
		Target variable Y = nonlinear function of X1 and X2 + Gaussian noise

*	For each synthetic population, the following is repeated 200 times:

	*	A non-probability sample is taken from the given synthetic population
		by taking a Poisson sample using the unit-specific self-selection
		propensity. (Imagine each unit independently self-selects or not
		according to his/her own self-selection propensity.)

	*	A simple random sample without replacement is taken from the given
		synthetic population with smapling fraction 1/10.

	*	The above pair of non-probability and probablity samples are given
		to TrIWP to generate the resulting population total estimate for Y.

	*	In addition, four other methods are applied to the same pair of
		non-probability and probability samples:

		*	Chen-Li-Wu Doubly Robust estimator,  
		*	True-Propensity Inverse Propensity Weighted,  
		*	Calibration/Raking (only population totals of X1, X2 need to be known)  
		*	Naive weighting

	*	For each method cited above, a histogram is generated to visualize
		the empirical distribution of the corresponding 200 estimates of the
		population total of Y, and the proximity of these estimates to the
		known value of the true population total.

Input files
-----------
There are no input files required for this prototype.

Output files
------------
Upon execution, `runall.bat` first creates the output directory mentioned above.
When execution finishes without errors, the newly created output directory
will contain the following:

* 	`<OUTDIR>/code`  
	a copy of the code directory ./code (for reproducibility)

*	`<OUTDIR>/stderr.R.runall`  
	standard error log file (should be empty)

*	`<OUTDIR>/stdout.R.runall`  
	main output text file of the pipeline.
	It starts with summary and exploratory statistics of the input file,
	followed by model-fitting performance metrics of the aforementioned
	implemented machine learning techniques.

*	`<OUTDIR>/population-propensity-scatter-01.csv`,  
	`<OUTDIR>/population-propensity-scatter-02.csv`,  
	`<OUTDIR>/population-propensity-scatter-03.csv`  
	respective scatter plots of the three synthetic populations in the (X1,X2)-space,
	where the colour scheme indicates the synthetic propensity of self-selection.

*	`<OUTDIR>/population-propensity-density-01.csv`,  
	`<OUTDIR>/population-propensity-density-02.csv`,  
	`<OUTDIR>/population-propensity-density-03.csv`  
	respective density plots of the self-selection propensity of the three synthetic
	populations; note the bimodal nature of the density of self-selection propensity.

*	`<OUTDIR>/np-propensity-scatter-01.csv`,  
	`<OUTDIR>/np-propensity-scatter-02.csv`,  
	`<OUTDIR>/np-propensity-scatter-03.csv`  
	(X1,X2)-scatter plots for three respective non-probability samples
	-- one for each synthetic population --
	where the colour scheme inidates the TrIPW estimates for the self-selection propensity.

*	`<OUTDIR>/np-propensity-estimated-vs-true-01.csv`,  
	`<OUTDIR>/np-propensity-estimated-vs-true-02.csv`,  
	`<OUTDIR>/np-propensity-estimated-vs-true-03.csv`  
	scatter plots directly comparing true self-selection propensities against
	their TrIPW estimates, based on one pair of non-probability and probability samples,
	for each synthetic population.

*	`<OUTDIR>/np-propensity-estimated-vs-true-01.csv`,  
	`<OUTDIR>/np-propensity-estimated-vs-true-02.csv`,  
	`<OUTDIR>/np-propensity-estimated-vs-true-03.csv`  
	respective scatter plots -- for the three synthetic populations --
	of the TrIPW self-selection propensity estimates based on one pair of
	non-probability and probability samples.

*	`<OUTDIR>/simulation-results-01.csv`,  
	`<OUTDIR>/simulation-results-02.csv`,  
	`<OUTDIR>/simulation-results-03.csv`  
	CSV files containing the respective results of the three simulation studies.

*	`<OUTDIR>/histogram-Ty-hat-calibration-01.png`,  
	`<OUTDIR>/histogram-Ty-hat-calibration-02.png`,  
	`<OUTDIR>/histogram-Ty-hat-calibration-03.png`  
	histograms of the calibration/raking estimates from the three simulations
	(orange vertical line indicates the value of true population total)

*	`<OUTDIR>/histogram-Ty-hat-CLW-01.png`,  
	`<OUTDIR>/histogram-Ty-hat-CLW-02.png`,  
	`<OUTDIR>/histogram-Ty-hat-CLW-03.png`  
	histograms of the Chen-Li-Wu Doubly Robust estimates from the three simulations
	(orange vertical line indicates the value of true population total)

*	`<OUTDIR>/histogram-Ty-hat-naive-01.png`,  
	`<OUTDIR>/histogram-Ty-hat-naive-02.png`,  
	`<OUTDIR>/histogram-Ty-hat-naive-03.png`  
	histograms of the naive (global scaling) estimates from the three simulations
	(orange vertical line indicates the value of true population total)

*	`<OUTDIR>/histogram-Ty-hat-propensity-01.png`,  
	`<OUTDIR>/histogram-Ty-hat-propensity-02.png`,  
	`<OUTDIR>/histogram-Ty-hat-propensity-03.png`  
	histograms of the true-propensity estimates from the three simulations
	(orange vertical line indicates the value of true population total)

*	`<OUTDIR>/histogram-Ty-hat-tree-01.png`,  
	`<OUTDIR>/histogram-Ty-hat-tree-02.png`,  
	`<OUTDIR>/histogram-Ty-hat-tree-03.png`  
	histograms of the TrIPW estimates from the three simulations
	(orange vertical line indicates the value of true population total)
