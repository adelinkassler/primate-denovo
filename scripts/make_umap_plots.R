# This scripts makes UMAP plots from the pp data

# devtools::install_github("Jaugust7/scVisualizeR")
suppressPackageStartupMessages({
  library(yaml)
  library(plotly)
  library(umap)
  library(gridExtra)
  library(optparse)
  library(tidyverse)
})

# Arguments to script -----------------------------------------------------

# Manually set the arguments to use in interactive mode. Useful for running the
# script within Rstudio.
Args <- if (interactive()) {c(
  "--control-table", "control/control1.csv",
  "--data-dir",      "Data",
  "--output-file",   "Output/UMAP_np.pdf",
  "--features",      "control/np_features.txt",
  "--seed",          42#,
  #"--umap-config",   "control/umap_defaults.yaml"
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
    c("-o", "--output-file"),
    dest = 'output_file_name',
    type = 'character',
    default = "UMAP.pdf"
  ),
  # List of features to restrict ourselves to.
  make_option(
    c("-f", "--features"),
    dest = 'features',
    type = 'character',
    default = NULL,
    help = "Path to a list of features to analyze, in a text file with one feature per line."
  ),
  # UMAP doesn't work with missing data (at least, the preferred R
  # implementation by the UMAP authors doesn't). By default, columns containing
  # missing data will be (noisily) dropped from analysis. To drop rows
  # containing missing data instead, use this flag.
  # make_option(
  #   c("--drop-rows"),
  #   dest = 'drop_rows',
  #   action = 'store_true',
  #   default = FALSE,
  #   help = "Drop rows containing missing data. Default is to drop columns instead."
  # ),
  # How to handle missing values? Can be one of drop_rows to drop rows
  # containing missing values, drop_cols to remove columns instead, or pairwise
  # to use pairwise-complete obs
  make_option(
    c("--na-method"),
    dest = 'na_method',
    type = 'character',
    default = 'drop_cols',
    help = "How to handle missing values in the data: either drop_rows or drop_cols [default %default]"
  ),
  # Allows for manually setting the random seed to control plot appearance
  make_option(
    c("--seed"),
    dest = 'umap_seed',
    type = 'integer',
    default = NULL,
    help = "Manually set the random seed for the UMAP algorithm. Allows reproducibility."
  ),
  # Optional argument that controls the folder where we look for backend
  # scripts. Useful if you care a lot about making this work with your current
  # directory structure.
  make_option(
    c("-b", "--backend"),
    dest = 'backend_dir',
    type = 'character',
    default = './scripts',
    help = "Directory containing the backend scripts for this toolkit [default %default]"
  ),
  make_option(
    c("--plotly"),
    dest = 'use_plotly',
    type = 'logical',
    action = 'store_true',
    default = FALSE,
    help = "Produce interactive plotly objects instead of writing to file [default %default]"
  ),
  make_option(
    c("--umap-config"),
    dest = 'umap_config_file',
    type = 'character',
    help = paste("Path to a YAML file containing a set of non-default UMAP configurations.",
                 "If unset, uses system defaults. For format, see the README")
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

# Source functions --------------------------------------------------------

# This file creates the processed data block
source(file.path(opts$backend_dir, 'process_raw_data.R'))

# Combine into a single data frame, then split into numeric and non-numeric.
data.proc.1df <- bind_rows(data.proc.list, .id = "Age_cat")
data.proc.num <- data.proc.1df %>% keep(is.numeric)


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
  non_num_cols <- setdiff(feature_names, names(data.proc.num))
  warning(sprintf(paste("The following %d requested features were not numeric", 
                        "and cannot be used for UMAP: %s"),
                  length(non_num_cols),
                  paste(non_num_cols, collapse = ', ')))
  # Keep only those features that are present
  feature_names <- intersect(feature_names, names(data.proc.num))
}

# Subset the data to only those features specified
umap.data <- select(data.proc.num, all_of(feature_names))

# Handle missing data - either dropping rows or columns containing any NAs. This
# is required by the UMAP algo as implimented here, which is the preferred UMAP
# implimentation in R by the UMAP authors.
if (opts$na_method == 'drop_rows') {
  umap.data <- drop_na(umap.data)
  n_rows_remove <- nrow(data.proc.num) - nrow(umap.data)
  if (n_rows_remove > 0) {
    warning(sprintf("Dropping %d rows containing missing data; %d remain",
                    n_rows_remove, nrow(umap.data)))
  }
} else if (opts$na_method == 'drop_cols') {
  umap.data <- umap.data %>% discard(anyNA)
  cols_remove <- setdiff(feature_names, names(umap.data))
  if (length(cols_remove) > 0) {
    msg <- paste("The following %d columns contained missing data, and were dropped",
                 "from analysis: %s\nTo drop rows containing missing data instead,",
                 "set the --drop-rows flag")
    warning(sprintf(msg, length(cols_remove), paste(cols_remove, collapse = ', ')))
    feature_names <- names(umap.data)
  }
} else {
  stop("Unknown argument to --na-method. Use drop_rows or drop_cols.")
}

cat(sprintf("Using %d observations of %d features\n", nrow(umap.data), ncol(umap.data)))


# Plot inf0 ---------------------------------------------------------------

# plotting_theme <- theme(
#   panel.border = element_rect(color = 'black', fill = NA),
#   panel.background = element_rect(fill = 'white'),
#   axis.text = element_text(color = 'black', family = 'sans', size = 19), #sans = windows Arial TT
#   plot.title = element_text(color = 'black', family = 'sans', size = 20, face = 'bold'),
#   plot.subtitle = element_text(color = 'black', family = 'sans', size = 19),
#   #plot.margin=grid::unit(c(1.5,0.5,1.5,0.5), "in")
# )


# UMAP object -------------------------------------------------------------

# Read in UMAP controls from a config file, possibly
umap_config <- if (is.null(opts$umap_config_file)) {
  umap.defaults
} else {
  # Grab list from the yaml file
  uc <- read_yaml(opts$umap_config_file)
  # Make sure we don't allow junk names through
  names_diff <- setdiff(names(uc), names(umap.defaults))
  if(length(names_diff > 0)) {
    cat(sprintf("The following arguments in %s are not valid UMAP controls: %s\n",
                opts$umap_config_file, paste(names_diff, collapse = ',')))
    uc <- uc[names(uc) %in% names(umap.defaults)]
  }
  # Parse "NA" as NA (no better way to do this)
  uc[uc == "NA"] <- NA
  # Format correctly
  class(uc) <- 'umap.config'
  uc
}

# Create labels, accounting for the possibility that we dropped rows
umap_lab <- if (opts$na_method == 'drop_rows') {
  drop_na(data.proc.1df, all_of(feature_names))$Age_cat 
} else {
  data.proc.1df$Age_cat
}

# Prepare gene names for plotly hover text in the same way
gene_names <- if (opts$na_method == 'drop_rows') {
  drop_na(data.proc.1df, all_of(feature_names))$Gene_Name
} else {
  data.proc.1df$Gene_Name
}

# Make the UMAP object we will use to plot. Then create a plotting data frame
# from the UMAP info and labels
set.seed(opts$seed)
pp.umap <- umap(umap.data, config = umap_config)
umap_layout.df <- pp.umap$layout %>% 
  `colnames<-`(c('x1', 'x2')) %>% 
  as_tibble() %>% 
  mutate(lab = umap_lab,
         gene_name = gene_names)

# Tabular output -----------------------------------------------------------

# Create a table of umap plots with information about each point, and position
# in the first two PCs. This is meant to be loaded into something like excel to
# explore subsetting and gene names on the fly, as a companion to the visuals

# Create formatted table of results
table_out <- umap_layout.df %>% 
  rename(PC1 = x1, PC2 = x2, Group = lab, Gene = gene_name) %>% 
  left_join(y = data.proc.1df %>% select(Gene = Gene_Name, Gene_Desc), by = 'Gene') %>% 
  mutate(Group = set_names(control_table$DisplayName, control_table$ShortName)[Group])

# Generate output filepath
outpath_base <- sub(pattern = '\\.[0-9a-zA-Z]$', replacement = '',
                opts$output_file_name)

# Write to a CSV file
write_csv(table_out, file = paste0(outpath_base, '.csv'))


# Make plots --------------------------------------------------------------

# Generic function that will plot a UMAP object we give it. Note we assume the
# existance of x1, x2, and lab columns
plot_umap <- function(.data) {
  # Randomize the order of the rows. Because the z scale of the plot is
  # determined by the row order of the data frame, this makes sure no age
  # category appears over or under represented due to zscale.
  .data <- .data[sample(nrow(.data)),]
  
  # Then make the plot
  ggplot(.data, aes(x = x1, y = x2, label = gene_name)) +
    # Adding custom aesthetic gene_name to pass labels to plotly
    geom_point(aes(color = lab), shape = 19) +
    scale_color_manual(limits = control_table$ShortName, values = control_table$Color,
                       labels = control_table$DisplayName, name = "Age") +
    theme_classic() +
    theme(axis.text = element_text(family = 'sans', size = 16),
          legend.text = element_text(family = 'sans', size = 16)) +
    labs(x = NULL, y = NULL)
}

# Make the main umap plot
main_plt <- plot_umap(umap_layout.df)
main_plt

# Make a different plot for each layer - i.e., each age category.
age_cats.plts <- umap_layout.df %>% 
  split(.$lab) %>% 
  map(plot_umap) %>% 
  map(~ . + geom_point(data = umap_layout.df, aes(x = x1, y = x2), shape = NA, na.rm = T))


# Write output to file ----------------------------------------------------

# Make a list of all plot objects, then write the output file
ggs <- c(list(main_plt), age_cats.plts[control_table$ShortName]) %>% 
  map(ggplotGrob)
ggsave(
  filename = opts$output_file_name,
  plot = marrangeGrob(ggs, nrow = 1, ncol = 1),
  width = 9,
  height = 7
)

# Plotly interactive plotting ---------------------------------------------

# Generate filepath
outpath_base <- sub(pattern = '\\.[0-9a-zA-Z]$', replacement = '',
                    opts$output_file_name) 

# if (opts$use_plotly) {
  cat("Creating interactive plot\n")
  ply_main <- ggplotly(main_plt, tooltip = 'gene_name')
  htmlwidgets::saveWidget(ply_main, paste0(outpath_base,'.html'), selfcontained = F)
# }
