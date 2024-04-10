---
title: Protein Prediction Data Analysis Toolkit
author: Daniel Kassler  (`dkassler@g.harvard.edu`)
output:
  html_document:
    toc: true
---

# About this toolkit

This toolkit contains the following front-end scripts. These are meant to be executed by the user on the command line

- `write_output_tables.R`  
- `make_plots.R`  
- `make_umap_plots.R`  
- `correlations.R`  
- `auto_select_features.R`  

It also contains a number of backend scripts, which are sourced by the front-end scripts and provide critical functions, but are not designed to be run themselves by the user:

- `process_raw_data.R`  
- `analyze.R`  

In addition to these files, it is meant to work with a control file (details below). Input data is in the form of CSV files with a specified set of columns.

# Quick start

The quickest way to get started is to place all R scripts in a `scripts` sub-directory under your working directory. You will also need to create a control file. Then run the following:

```
> Rscript scripts/write_output_tables.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --output-dir PATH_TO_OUTPUT_DIR
> Rscript scripts/make_plots.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --output-file NAME_OF_OUTPUT_FILE
```

For UMAP plots and correlations, run the following, being sure to change the names of the output arguments and feature lists for each:

```
> Rscript scripts/correlations.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --features control/np_features.txt --output-prefix PATH_TO_OUTPUT_DIR/np
> Rscript scripts/correlations.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --features control/count_features.txt --output-prefix PATH_TO_OUTPUT_DIR/count
> Rscript scripts/make_umap_plots.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --features control/np_features.txt --output-file NAME_OF_NP_OUTPUT_FILE
> Rscript scripts/make_umap_plots.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --features control/count_features.txt --output-file NAME_OF_COUNT_OUTPUT_FILE
> Rscript scripts/make_umap_plots.R --control-table PATH_TO_CONTROL_TABLE.csv --data-dir PATH_TO_DATA_DIR --output-file NAME_OF_ALL_OUTPUT_FILE
```

# How to use

### Common Arguments

The following arguments are common to all front-end scripts:

- `-t FILE`, `--control-table FILE`: path to the control table, described in greater detail below.  
- `-d DIR`, `--data-dir DIR`: directory to look for the data in. This gets prepended to the filenames in the control table, so if those file names are complete paths or the data is located in the current working directory this argument can be omitted.  
- `-b DIR`, `--backend DIR`: path to directory containing the backend scripts. Can be omitted if you use the default location, `./scripts`.
- `-h`: Show quick help information for all the arguments.

### Control table

The control table should be a CSV file containing the following columns: `ShortName`, `DisplayName`, `Path`, `Color`. Names must be exact, but order doesn't have to be, as the program reads by name not position. Therefore, there can be other columns in the control table which the program will ignore. The control table should contain one row per input file (generally, each input file corresponds to one age category), and the order of rows determines the order age categories will be plotted along the x axis.

- `ShortName`: abreviated names to use internally, and in automatic naming of output files  
- `DisplayName`: full name of groups to use for plot labels  
- `Path`: filename (or, optionally, the full filepath) of csv files containing the raw PP data  
- `Color`: color to use for group in plots. Can be either hex colors or R color names, and can be overridden with the `--single-color` argument  

An example control file has been included with this toolkit.

# Individual front-end script details

### Write tables of processed and analyzed data (`write_output_tables.R`)

This script writes tables for the normalized and processed data (one per raw data input), summary statistics (one per raw data input), and a single table each containing adjusted p values from t tests and Wilcoxon-Mann-Whitney rank sum tests. All tables are written as CSV files.

In addition to the standard arguments, the following arguments are used:

- `-o DIR`, `--output-dir DIR`: Directory to write the output files to. Defaults to the current working directory.  
- `--p-adjust-method METHOD_STRING`: What method to use for p-value adjustments to reflect multiple testing. Use "BH" or "fdr" for Benjamini-Hochberg, "bonferroni" for Bonferroni, and "holm" for Holm. Also accepts some other common methods understood by R. If left unset, will use the Holm method.
- `--p-adjust-n INTEGER`: Sample size to use for p-value adjustments. If left unset, defaults to the number of p-values being generated. Use great caution when reducing the sample size manually with this argument, as this can easily throw off your p-values and invalidate your results.

Note that "There seems no reason to use the unmodified Bonferroni correction because it is dominated by Holm's method, which is also valid under arbitrary assumptions." (R documentation for `p.adjust`)

### Make boxplots and violin plots (`make_plots.R`)

This script writes a single PDF file containing hundreds of plots in several styles, all roughly summarising the distributions of various columns of data and how they differ across age categories.

In addition to the standard arguments, the following arguments are used:

- `-o FILE`, `--output-file FILE`: Filename (or full path to) the output file to be created. Must include the `.pdf` extension (including a different extension, such as `.png`, will cause R to try and write results to that format, but your milage may vary)  
- `--single-color COLOR`: Optional, set this flag if you want to override the colors in the control table with a single color used for all age categories. Argument is a single color specification string recognizable by R (either a known color name such as `lightblue2`, or a valid hex code such as `#0F13A5`). Valid R color names can be found by running `colors()` in an R prompt, but it's easier to just search for it online so you can see what they look like too.  
- `-g`, `--x-gap-after INTEGER`: Place a gap in the age categories along the x axis after this point. To place a gap between the fourth and fifth categories, set this flag to 4.  
- `-c`, `--compress`: Set this flag if you want to output 8 graphs per page, with smaller font and margins. Otherwise, graphs will be one per page with large font.  
- `--met-only`: Set this flag to only graph proteins starting with Methionine.  
- `--skip-outliers`: Set this flag to only show plots with y ranges adjusted to remove outliers. Otherwise two versions of each plot will be shown, one of which has its range adjusted and one of which also shows outliers.  
- `--signal-only`: Set this flag to only graph proteins with signal peptides.  


### Make UMAP plots (`make_umap_plots.R`)

Generates a PDF file containing UMAP plots for a given feature set: one for all age categories, and then one per individual age category.

The arguments used, in addition to the common ones, are:

- `-o FILE`, `--output-file FILE`: Filename (or full path to) the output file to be created. Must include the `.pdf` extension (including a different extension, such as `.png`, will cause R to try and write results to that format, but your milage may vary)  
- `-f`, `--features FILE` Path to a text file containing the names of the features you want to analyze, one per line.  
- `--na-method METHOD_STRING` How to deal with missing values in the data? One of `drop_rows` to drop all rows containing an NA, `drop_cols` to exclude features containing missing data. Defaults to `drop_cols`.  
- `--plotly` Make an interactive version of the plot, which will be found at `OUTPUT_FILE_NAME.html`  
- `--umap-config FILE` Path to a YAML-formatted text file containing configuration parameters for the UMAP algorithm.

If the `--umap-config` flag is left unset, the script uses the default parameters given by the `umap` package (favored by UMAP developers). These appear in the example control file `control/umap_defaults.yaml`. This file also serves as a reference for the parameters which can be set in the UMAP algorithm. For any parameters not set in a YAML file, the default will be used (allowing for partial config files containing only a few parameters).


### Make correlation tables and plots (`correlations.R`)

This script computes the correlations for a list of features specified by the user (in the form of a file, one feature name per line). It outputs a table and a plot for correlations within each age category, as well as one for all categories and one for all non-control (i.e., only PS and no igen or rand) categories. The order of the features in the plot is determined by their order in the input file.

The following arguments are used in addition to the common ones:

- `-o`, `--output-prefix PATH_AND_PREFIX` Directory to place the output files in, as well as an (optional) shared prefix to identify them. For example, `-o Output/corr/np` will produce output files that look like `np.ps31.corrplot.png` and `np.igen.corrtable.csv`.  
- `-f`, `--features FILE` Path to a text file containing the names of the features you want to analyze, one per line.  
- `--na-method METHOD_STRING` How to deal with missing values in the data? One of `drop_rows` to drop all rows containing an NA, `drop_cols` to exclude features containing missing data, or `pairwise` (the default), which calculates correlations using all pairwise-complete observations for each feature pair.

When selecting a method for missing data, `pairwise` is less conservative than `drop_rows` which uses as much data as it can for each pair of features, but may produce a correlation matrix which is not positive semidefinite. In general, dropping observations using `pairwise` or `drop_rows` may produce bias in your results. `drop_cols` is bias-free, and particularly helpful when columns containing NA are missing the majority of their data - but may be overzealous in tossing columns that are only missing a few observations.

### Generate features lists (`auto_select_features.R`)

This script automatically generates two lists of features from the data (one for count columns, and one for normalized-percent columns) using the same specifications that Kris used in her original correlation script. It is meant as a way to reproduce that automated feature selection behavior, while still giving you the option to pass in a manual list of features instead. It only uses common 

# Last thoughts

Changes to the columns of the input data (such as new additions to be included in analysis, or different column names) will necessitate changes to the code. I've tried to make it as easy as possible to make these changes by simply changing certain strings within some of the files.
