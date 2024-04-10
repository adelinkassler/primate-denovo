# This script writes the processed data, and nothing else.

# Suppressing startup messages so we don't flood the terminal when running from
# command line. Comment out this line and the closing bracket to load these
# packages with all their diagnostic noise.
suppressPackageStartupMessages({
  library(optparse)
})

# Arguments ---------------------------------------------------------------

# Manually set the arguments to use in interactive mode. Useful for running the
# script within Rstudio.
Args <- if (interactive()) {c(
  "--control-table", "control/control1.csv",
  "--data-dir",      "Data",
  "--output-dir",    "Output/"
)} else {commandArgs(trailingOnly = T)}

# List of arguments to the script. 
option_list <- list(
  # Location of the control file. This should be a CSV file containing the
  # following columns: ShortName, DisplayName, Path, Color. Names must be exact,
  # but order doesn't have to be, as the program reads by name not position.
  # Therefore, there can be other columns in the control file which the program
  # will ignore.
  make_option(
    c("-t", "--control-table"),
    dest = 'control_file',
    type = 'character',
    default = NULL,
    help = "Path to control table"
  ),
  # Where should we look for the data? Note that this will get prepended to the
  # filepaths listed in the control table, so don't duplicate any part of the
  # filepath between here and there
  make_option(
    c("-d", "--data-dir"),
    dest = 'data_dir',
    type = 'character',
    default = '.',
    help = "Path to the directory containing the data"
  ),
  # Where should we put the output files? This specifies the directory, and will
  # be prepended to the names of the output files that are generated.
  make_option(
    c("-o", "--output-dir"),
    dest = 'output_dir',
    type = 'character',
    default = '.',
    help = "Location of an existing directory to place the output files in"
  ),
  make_option(
    c("--p-adjust-method"),
    dest = 'p_adjust_method',
    type = 'character',
    default = 'holm',
    help = paste("Method to use for multiple-testing correction. Can be one of",
                 paste(p.adjust.methods, collapse = ', '),
                 " with holm or BH recommended [default %default]")
  ),
  make_option(
    c("--p-adjust-n"),
    dest = 'p_adjust_n',
    type = 'integer',
    default = NULL,
    help = paste("Sample size for p adjust methods. Defaults to number of observations.",
                 "Use caution when setting this value manually, as adjusting this value", 
                 "can easily invalidate your signficance testing.")
  ),
  make_option(
    c("-b", "--backend"),
    dest = 'backend_dir',
    type = 'character',
    default = './scripts',
    help = "Directory containing the backend scripts for this toolkit [default %default]"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser, args = Args)


# Source functions --------------------------------------------------------

# This file creates the processed data block
source(file.path(opts$backend_dir, 'process_raw_data.R'))

# This file contains all the analysis functions
source(file.path(opts$backend_dir, 'analyze.R'))


# Write output ------------------------------------------------------------

# Write the normalized/processed data
iwalk(data.proc.list, ~ write_csv(.x, file.path(
  opts$output_dir, paste0("proc_data_", .y, ".csv")
)))
# Write the summary statistics
iwalk(data.proc.list, ~ write_csv(basic_stat_block(.x), file.path(
  opts$output_dir, paste0("summ_stats_", .y, ".csv")
)))
# Write a table of t-tests
write_csv(
  test_across_dfs(data.proc.list, test = ttest_pval, non.num = 'null',
                  mc = opts$p_adjust_method, n = opts$p_adjust_n),
  file.path(opts$output_dir, "ttest.csv")
)
# Write a table of Wilcoxon-Mann-Whitney tests
write_csv(
  test_across_dfs(data.proc.list, test = wtest_pval, non.num = 'null',
                  mc = opts$p_adjust_method, n = opts$p_adjust_n),
  file.path(opts$output_dir, "wtest.csv")
)