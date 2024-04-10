# Runs correlations, producing tables and plots. Unlike Kris' original script,
# this version takes in a file with an explicit list of features, making it more
# flexible. Use auto_select_features.R to make a feature list if you want the
# same feature selection strategy Kris used, or write your own!

# Load libraries and setup ------------------------------------------------

suppressPackageStartupMessages({
  library(optparse)
  library(corrplot)
  library(viridis)
  library(tidyverse)
})

# Read/set arguments ------------------------------------------------------

# If running this script inside Rstudio or as an interactive, we won't have
# access to command line stuff - so this spoofs command arguments, but only in
# interactive mode
if (interactive()) {
  Args <- c('-d', "Data",
            '--control-table', "control/control1.csv",
            '--output-prefix', "Output/corr-all")
} else {
  # Reset to default if somehow there was a persistent override. Shouldn't ever
  # happen but just in case.
  Args <- base::commandArgs(trailingOnly = TRUE)
}

# Make a list of options for this script, then pass them to the parser and use
# them to interpret either the command line arguments or the manual substitute.
# Changing the value of "default" argument for each option controls what options
# are used when this script is run without arguments (such as in interactive
# mode in Rstudio).
option_list <- list(
  # Location of the control file. See README for specification.
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
  # Where should we put the output files? This specifies the directory and a
  # prefix string, and will be prepended to the names of the output files that
  # are generated.
  make_option(
    c("-o", "--output-prefix"),
    dest = 'output_prefix',
    type = 'character',
    default = '.',
    help = "Prefix to use for output files, including filepath"
  ),
  # List of features to restrict ourselves to.
  make_option(
    c("-f", "--features"),
    dest = 'features',
    type = 'character',
    default = NULL,
    help = "Path to a list of features to analyze, in a text file with one feature per line."
  ),
  # How to handle missing values? Can be one of drop_rows to drop rows
  # containing missing values, drop_cols to remove columns instead, or pairwise
  # to use pairwise-complete obs
  make_option(
    c("--na-method"),
    dest = 'na_method',
    type = 'character',
    default = 'pairwise',
    help = paste("How to handle missing values in the data: one of drop_rows,",
                 "drop_cols, or pairwise which",
                 "uses all pairwise-complete observations for each feature pair.")
  ),
  # Flexibility option for moving backend scripts. Can usually be ignored.
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

# Read in control file
control_table <- read_csv(
  opts$control_file,
  col_types = cols(
    ShortName = col_character(),
    DisplayName = col_character(),
    Path = col_character(),
    Color = col_character()
  )
)


# Load dependencies -------------------------------------------------------

# Provides data analysis
source(file.path(opts$backend_dir, "process_raw_data.R"))

# Combine into a single data frame
data.proc.1df <- bind_rows(data.proc.list, .id = "Age_cat")
data.proc.num <- data.proc.1df %>% keep(is.numeric)
group_labels <- data.proc.1df$Age_cat

# Feature selection -------------------------------------------------------

# Read in the list of features
feature_names <- if (is.null(opts$features)) {
  message(paste("Because the --features argument is not provided, attempting to",
                "use all columns of the data as features"))
  final_col_order
} else {
  read_lines(opts$features)  
}

# Check to see if the features we want are all in the data, and warn if not
if (!all(feature_names %in% names(data.proc.1df))) {
  warning(sprintf(paste("The following features are listed in the features file",
                        "but are not present in the data: %s"),
                  paste(setdiff(feature_names, names(data.proc.1df)),
                        collapse = ', ')))
  # Keep only those features that are present
  feature_names <- intersect(feature_names, names(data.proc.1df))
}

# Warn if we had to drop some of the requested features
if (!all(feature_names %in% names(data.proc.num))) {
  warning(sprintf(paste("The following requested features were not numeric", 
                        "and cannot be used for correlation: %s"),
                  paste(setdiff(feature_names, names(data.proc.num)),
                        collapse = ', ')))
  # Keep only those features that are present
  feature_names <- intersect(feature_names, names(data.proc.num))
}

# Subset the data to only those features specified
corr.data <- select(data.proc.num, all_of(feature_names))

# Handle missing data depending on what the na_method option is set to.
if (opts$na_method == 'drop_rows') {
  # Drop rows containing NAs. This ensures that you'll have all your requested
  # columns, but runs the risk of biasing your results by systemically omitting
  # some observations, or by throwing out a large proportion of observations.
  # Reports how many rows you are throwing away as a safety feature.
  corr.data <- drop_na(corr.data)
  n_rows_remove <- nrow(data.proc.num) - nrow(corr.data)
  if (n_rows_remove > 0) {
    warning(sprintf("Dropping %d rows containing missing data; %d remain",
                    n_rows_remove, nrow(corr.data)))
    # Adjust group labels to drop those rows as well
    group_labels <- drop_na(data.proc.1df, all_of(feature_names))$Age_cat 
  }
} else if (opts$na_method == 'drop_cols') {
  # Drops columns containing NAs. Free from bias, but some features requested
  # may be thrown out over a very small number of NAs.
  corr.data <- corr.data %>% discard(anyNA)
  cols_remove <- setdiff(feature_names, names(corr.data))
  if (length(cols_remove) > 0) {
    msg <- paste("The following %d columns contained missing data, and were dropped",
                 "from analysis: %s")
    warning(sprintf(msg, length(cols_remove), paste(cols_remove, collapse = ', ')))
    feature_names <- names(corr.data)
  }
} else if (opts$na_method == 'pairwise') {
  # Do nothing, this is handled later
  message(paste("Using --na-method pairwise may produce a correlation matrix which is",
                "not positive semi-definite"))
} else {
  stop(sprintf(paste("Unknown argument to --na-method. Please use one of drop_rows,",
                     "drop_cols, or pairwise")))
}

# Correlation function ----------------------------------------------------

write_corr_results <- function(.data, .name) {
  # Generate correlation matrix
  use <- if (opts$na_method == 'pairwise') 'pairwise.complete.obs' else 'everything'
  corr.mat <- cor(.data, use = use)
  
  # Create summary table and write to file
  corr_results.df <- as.data.frame(corr.mat) %>% 
    rownames_to_column('V1') %>% 
    pivot_longer(-V1, names_to = 'V2', values_to = 'Corr') %>% # Melt to long format
    filter(match(V1, feature_names) > match(V2, feature_names)) %>% # De-duplicate
    arrange(desc(Corr))
  table_file_name <- paste0(opts$output_prefix, '.', .name, '.corrtable.csv')
  write_csv(corr_results.df, table_file_name)
  
  # Replace NA for visual purposes only
  # corr.mat[is.na(corr.mat)] <- 0
  
  # Create correlation plot and write to file
  plot_file_name <- paste0(opts$output_prefix, '.', .name, '.corrplot.png')
  png(plot_file_name, height = 1500, width = 1500)
  corrplot(corr.mat, method="color", col=viridis(10), order="original", tl.cex = 1, 
           tl.col ="black", cl.cex=2, cl.align.text="r")
  dev.off()
  
  # Return table of correlations invisibly
  invisible(corr_results.df)
}


# Run correlations --------------------------------------------------------

# Individual correlations for each individual age category
walk(control_table$ShortName, function(shortname) {
  .data <- corr.data[group_labels == shortname,]
  .name <- shortname
  write_corr_results(.data, .name)
})

# One large correlation for all age categories
write_corr_results(corr.data, "all_groups")

# One correlation table for everything except the control categories
if (!all(c('igen', 'rand') %in% group_labels)) {
  warning("igen and/or rand not in group labels - did you name them something else?")
}
sel <- !(group_labels %in% c('igen', 'rand')) # Selection vector - logical
write_corr_results(corr.data[sel,], 'ps_only')

