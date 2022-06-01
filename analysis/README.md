# analysis directory

This is the main directory for the analysis. All of the scripts necessary to run the analysis are present in this directory. All raw datasets are in the `input` subdirectory and all constructed analytical data is placed in the `output` subdirectory. 

The entire analysis is conducted in R. To ensure that all the required packages for the analysis are installed locally, the user should first source the `check_packages.R` script in R. This script will check whether required packages are installed and if they are not installed, it will install them.

The entire analysis can be run from the provided shell script `run_entire_project.sh`. This script also specifies the order in which scripts should be run. I also describe this order below.

- `organize_data.R` - This script reads in the raw data and transforms it into analytical datasets used for the analysis and modeling. These analytical datasets consistt of the marriage choice sets. They are stored in the output directory. Due to their size, they are not included in the repository, but users can generate them by sourcing in this script. Because part of the analytical process is random selection of marriage partner alternates, the top of this script sets the seed which guarantees that the results will be identical to those in the paper. 
- `run_models.R` - This script runs the actual models that are used for the main analysis. **Warning**: This script will take a long time to run. It runs each model on five sets of data and each model contains millions of cases and numerous parameters. Its best to run this script overnight. The output of this script is saved as `models.RData` in the `output` directory and used in the `analysis.Rmd` script. Users who just want to examine finally results can load that file.
- `run_bendog_models.R` - This script runs models that test different definitions of birthplace endogamy for the 1.5 generation, as reported in Table 3 of the paper. The output of these models is stored in `models_bendog.RData` in the `output` directory.
- `analysis.Rmd` - This is the main analysis file. It is a R Markdown file that produces and `analysis.html` file that serves as a research log. 

In addition, I also conducted a sensitivity test using states as the marriage market designation. The model output from these results is available in the `output` directory as `models_sensitivity_mar_market.RData` and an alternative analysis using this designation is available in `analysis_sensitivity_mar_markets.html`.

- `check_packages.R` - You can put a list of all libraries used in any R script for the analysis and this script will check to make sure they are installed and if they are not installed it will attempt to install them.
- `useful_functions.R` - A script to keep track of R functions that might have uses across multiple scripts.
- `organize_data.R` - A script that will read in raw data, perform data manipulation, and output analytical dataset. In practice, projects often contain multiple scripts for this purpose depending on the complexity of the raw data.
- `analysis.Rmd` - I typically run the main analysis for the project in a single R Markdown file, output as HTML. This allows me to write a more detailed description of what I am doing and serves as a rough draft of the language that will ultimately go into the paper. The output file basically serves as a lab notebook and will contain a variety of analyses, only some of which will make it into the final paper. Sometimes I may split this into multiple Rmd files if I find that the quantity of analysis makes it slow to run this file.

In addition, I also include a shell script called `run_entire_project.sh` that will run the entire analysis from scratch. Keep in mind that this script will remove any HTML output and all files in the `logs` and `output` directories before attempting to re-run the scripts, in order to ensure that old output does not get mixed in with new output. As other scripts are added to the project, this shell script should be updated. Any user should be able to view this shell script and see how all the various scripts in this directory are used to reproduce the analysis.

I remove all identifying information from scripts and output in this directory so that I can share this information with reviewers through an anonymous link on [osf.io](https://osf.io). One good way to check and make sure that all names have been removed is to use the command line tool `grep` to search for names in all the documents. For example (from within the analysis directory):

```bash
grep -r Gullickson *
```

will search for any documents with the word "Gullickson" in them. 
