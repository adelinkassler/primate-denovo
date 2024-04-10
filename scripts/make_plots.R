# This script exports all the graphs we are interested in out into a PDF.
# Options exist for writing multiple graphs per page ("compressed" version).
# Producing a variety of plots, including box plots and violin plots.

suppressPackageStartupMessages({
  library(optparse)
  library(gridExtra)
  library(tidyverse)
})

options(dplyr.summarise.inform = FALSE)

# Script options ----------------------------------------------------------

# Manually set the arguments to use in interactive mode. Useful for running the
# script within Rstudio.
Args <- if (interactive()) {c(
  "--control-table", "control/control1.csv",
  "--data-dir",      "Data",
  "--output",        "Output/Rplots.pdf"
)} else {commandArgs(trailingOnly = T)}

# List of arguments to the script.
option_list <- list(
  # make_option(
  #   c("-u", "--usage"),
  #   action = "store_true",
  #   type = "logical",
  #   default = FALSE,
  #   help = "Print usage"
  # ), 
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
    help = "Path to control table."
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
  # Path to the file we want to write as output. File extension will determine file type.
  make_option(
    c("-o", "--output-file"),
    dest = 'output_file_name',
    type = 'character',
    default = "Rplots.pdf",
    help = "Full filepath to the output file where we will write our images."
  ),
  # If this option is set, the single color (either an R color name or a hex
  # code of the format #123456) provided as an argument will override the
  # default colors given in the control file.
  make_option(
    c('--single-color'),
    dest = 'single_color',
    type = 'character',
    default = NULL,
    help = 'An R or hex color string to override the colors in the control file'
  ),
  make_option(
    c("-c", "--compress"),
    action = "store_true",
    type = "logical",
    default = FALSE,
    help = "Make graphs in a 4x2 grid per page. [default %default]"
  ),
  make_option(
    c("--met-only"),
    action = "store_true",
    dest = 'met_only',
    type = "logical",
    default = FALSE,
    help = "Graph only proteins starting with methionine? [default %default]"
  ),
  make_option(
    c("--skip-outliers"),
    action = "store_true",
    dest = "skip_outliers",
    type = "logical",
    default = FALSE,
    help = "Skip second set of graphs that have outliers? [default %default]"
  ),
  make_option(
    c("--signal-only"),
    action = "store_true",
    dest = "signal_only",
    type = "logical",
    default = FALSE,
    help = "Graph only signal peptide proteins? [default %default]"
  ),
  make_option(
    c("-g", "--x-gap-after"),
    dest = "x_gap",
    type = "integer",
    default = NULL,
    help = "Where to place a gap in the x-axis. NULL for no gap. [default %default]"
  ),
  make_option(
    c("-v", "--variables"),
    dest = 'var_names',
    type = 'character',
    default = NULL,
    help = "Option under development."
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

# Imports and settings ----------------------------------------------------

library(gridExtra)
library(tidyverse)


# Control files and program options ---------------------------------------

plt_ctrl <- read_csv(
  opts$control_file,
  col_types = cols(
    ShortName = col_character(),
    DisplayName = col_character(),
    Path = col_character(),
    Color = col_character()
  )
)

# Override plot colors if we've chosen a single color instead
if (!is.null(opts$single_color)) plt_ctrl$Color <- opts$single_color

# We'll only plot these columns if --signal-only is set
cols_for_SP_positive = c(
    "#S-S", "Norm-#S-S", "S-Slen", "NormS-S_len", "#Cys",
    "Norm-#Cys", "Norm-#S-S_Potential", "S-S realized/possible"
)

# Import data -------------------------------------------------------------

# This script provides functions that produce our final, normalized and
# processed, data for plotting with. It also provides the processed data list.
source(file.path(opts$backend_dir, "process_raw_data.R"))

# Combine into a single data frame for compatibility with ggplot
data.proc.1df <- bind_rows(data.proc.list, .id = "Age_cat")

# Filter to met only, signal only, or both
if (opts$met_only) {
  cat("Filtering to only proteins starting with Met\n")
  data.proc.1df <- data.proc.1df %>% filter(toupper(substr(Sequence, 1, 1)) == 'M')
}
if (opts$signal_only) {
  cat("Filtering only to proteins with signal peptides predicted\n")
  data.proc.1df <- data.proc.1df %>% 
    filter(TP_loc == 'signal') %>% 
    select(c(Age_cat, one_of(cols_for_SP_positive)))
}

# Plotting functions ------------------------------------------------------

insert_blank <- function(x, after) {
  if (is.null(after)) return(x)
  if (is.character(after)) after <- match(after, x)
  c(x[1:after], ' ', x[(after+1):length(x)])
}

# A single plotting theme re-used across all our plotting functions. Allows us
# to modify all plots and maintain a unified aesthetic.
plotting_theme <- if (!opts$compress) {
  cat("Using full page plotting size\n")
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.background = element_rect(fill = 'white'),
    axis.text = element_text(color = 'black', family = 'sans', size = 19), #sans = windows Arial TT
    plot.title = element_text(color = 'black', family = 'sans', size = 20, face = 'bold'),
    plot.subtitle = element_text(color = 'black', family = 'sans', size = 19),
    plot.margin=grid::unit(c(1.5,0.5,1.5,0.5), "in")
  )
} else {
  cat("Using compressed plotting size\n")
  # We use a slightly different theme for the compressed version: smaller font sizes and margins.
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.background = element_rect(fill = 'white'),
    axis.text = element_text(color = 'black', family = 'sans', size = 10), #sans = windows Arial TT
    plot.title = element_text(color = 'black', family = 'sans', size = 12, face = 'bold'),
    plot.subtitle = element_text(color = 'black', family = 'sans', size = 10),
    plot.margin=grid::unit(c(0.1,0.1,0.1,0.1), "in")
  )
}
plotting_xscale <- scale_x_discrete(
  limits = insert_blank(plt_ctrl$ShortName, opts$x_gap), 
  labels = plt_ctrl$DisplayName,
  breaks = plt_ctrl$ShortName
)

# Utility function that finds the 5 critical values for drawing boxplots. We
# don't use it directly for drawing the plots (ggplot does that for us) but we
# do use these values for adding other labels and geoms to the plot
boxplot_crit_vals <- function(.data, var_name) {
  y <- sym(var_name)
  boxplot_stats <- .data %>% 
    group_by(Age_cat) %>% 
    summarize(out = list(boxplot.stats(!!y, coef = 1.5)$stats), .groups = ) %>%
    quietly(unnest_wider)(out, names_sep = "") %>% `$`(result) %>%
    rename_at(-1, ~ str_replace(., 'out', 'x'))
  return(boxplot_stats)
}

# Since we share so much formatting between plot types, this is a wrapper
# function to prevent needless reuse of code. It set up the plotting data, calls
# the core plot specified by `geom`, which will be one of 'boxplot' or 'violin',
# and then adds ancillary layers for labelling and trimming
make_distrplot <- function(.data, var_name, outliers = TRUE, nonzero = FALSE, 
                           coef = 1.5, geom = 'boxplot') {
  y <- sym(var_name)
  
  # Restrict data to just plotting data for computational efficiency
  plt_data <- .data %>% select(Age_cat, !!y)
  if (nonzero) plt_data <- plt_data %>% filter(!!y != 0)
  
  # Generate the subtitle from arguments to the function
  subt <- case_when(
    !outliers & !nonzero ~ "With outliers trimmed",
    outliers & nonzero   ~ "Nonzero values only",
    !outliers & nonzero  ~ "Outliers trimmed, nonzero values only",
  )
  if (is.na(subt)) subt <- NULL
  
  # Find boxplot crit vals
  bplot_cv <- boxplot_crit_vals(plt_data, var_name)
  
  # Generate base plot, which will be either a boxplot or a violin plot.
  gg <- if (geom == 'boxplot') {
    # ~~~~~~~~~~ Core boxplot ~~~~~~~~~~
    ggplot(plt_data, aes(x = Age_cat, y = !!y)) +
      geom_errorbar(data = bplot_cv, aes(x = Age_cat, y = x3, ymin = x1, ymax = x5),
                    inherit.aes = FALSE, width = 0.5) +
      geom_boxplot(aes(fill = Age_cat),
                   outlier.shape = ifelse(outliers, 19, NA), coef = coef)
  } else if (geom == 'violin') {
    # ~~~~~~~~ Core violin plot ~~~~~~~~
    ggplot(plt_data, aes(x = Age_cat, y = !!y)) +
      geom_violin(aes(fill = Age_cat), scale = 'width', size = 1) +
      geom_crossbar(data = bplot_cv, aes(y = x3, ymin = x2, ymax = x4), 
                    width = 0.1, fill = 'white')
  } else stop ("`geom` must be one of 'boxplot' or 'violin'")
  
  # Control scales, axis labels, and general appearance
  gg <- gg +
    plotting_xscale +
    scale_fill_manual(limits = plt_ctrl$ShortName, values = plt_ctrl$Color, guide = FALSE) +
    labs(x = NULL, y = NULL, title = var_name, subtitle = subt) +
    # labs(x = "Age Category") +
    plotting_theme
  
  # Find appropriate range for y and shrink the borders of the plot if we've
  # decided we want to trim outliers.
  if (!outliers) {
    yrange <- plt_data %>% 
      group_by(Age_cat) %>% 
      group_map(~boxplot.stats(.[[y]], coef = coef)$stats[c(1,5)]) %>% 
      range()
    gg <- gg + coord_cartesian(ylim = yrange * 1.05)
  }
  
  # If this is a plot for nonzero elements, display the number of nonzero
  # elements and total number of elements as an annotation in each column of the
  # plot
  if (nonzero) {
    # Extracting the y range the plot will use before its built, in order to
    # find a good y value for the labels. Slightly more likely to break on a
    # future ggplot2 update than the rest of the code, but replaceable if so
    plt_yrange <- ggplot_build(gg)$layout$panel_params[[1]]$y.range
    nz_counts <- nonzero_counts_table(.data, var_name)
    gg <- gg + geom_text(data = nz_counts, vjust = 1.5,
                         aes(label = paste(nonzero, total, sep = '/'), 
                             # Adjust proportions to move labels vertically. But
                             # they should sum to 1 still.
                             y = 1.00 * plt_yrange[2] + 0.00 * plt_yrange[1]))
  }
  
  # Finally, return the finished plot
  return(gg)
}
make_boxplot <- partial(make_distrplot, geom = 'boxplot')
make_violin <- partial(make_distrplot, geom = 'violin')

# This function decides whether we have too many zeros, and need to make a
# seperate plot without the zero values
too_many_zeros <- function(x) {
  # For now, this is a placehold function - we're just checking if it's in the
  # manually specified list Amir used. TODO: future-proof this with some
  # conditions, or farm it out to a control argument
}
cols_with_zeros <- c("Norm-#Dbind", "Norm-#TMD", "Norm-TMD_Tot_Length",
                     "Norm-#Coils", "%Coil", "Norm-D-boxes", 
                     "Norm-#KEN-boxes", "Norm-#(D+KEN)")

# For a given variable in our data (given as name, in string), this function
# creates a table showing the number of zero values, nonzero values, and total
# number of non-missing values (total should be sum of zero and nonzero). Note
# that this might not equal the number of observations in the data due to
# possible missing values, which aren't counted.
nonzero_counts_table <- function(.data, var_name) {
  y <- sym(var_name)
  .data %>% 
    group_by(Age_cat) %>% 
    summarize(zero = sum(!!y == 0, na.rm = TRUE),
              nonzero = sum(!!y != 0, na.rm = TRUE),
              total = sum(!is.na(!!y)))
}

# Function will create a bar plot showing the proportion of zeros in the data,
# for a given variable named as a string
make_zeroprop_plot <- function(.data, var_name) {
  # Make a table of zero counts (which we keep for annotations)
  nz_counts <- nonzero_counts_table(.data, var_name) 
  
  # Main body of plot. Note that the order of layers is important, as we're
  # plotting colors short bars after and therefore on top of the gray bars for
  # total observations
  ggplot(nz_counts, aes(x = Age_cat)) +
    geom_bar(aes(y = total), fill = 'grey', color = 'black', size = 1, stat = 'identity') +
    geom_bar(aes(y = nonzero, fill = Age_cat), color = 'black', size = 1, stat = 'identity') +
    geom_text(aes(y = total, label = total), vjust = 1.1) +
    geom_text(aes(y = nonzero, label = nonzero), vjust = -0.5) +
    plotting_xscale +
    scale_fill_manual(limits = plt_ctrl$ShortName, values = plt_ctrl$Color, guide = FALSE) +
    labs(x = NULL, y = NULL, title = var_name, subtitle = "Count of nonzero values") +
    plotting_theme
}

# Wrapper function to run all the different variations of plots for each data
# column and return them as a list. We'll loop this over all columns we want to
# plot.
make_all_plots <- function(.data, var_name, skip_outliers = F, met) {
  plist <- list()
  
  # Plot basic boxplot, with and without outliers
  plist$pb2 <- make_boxplot(.data, var_name, outliers = F)
  plist$pv2 <- make_violin(.data, var_name, outliers = F)
  if (!skip_outliers) {
    plist$pb1 <- make_boxplot(.data, var_name, outliers = T)
    plist$pv1 <- make_violin(.data, var_name, outliers = T)
  }
  
  # Check for presence of lots of zeros. If there are, add additional plots
  if (var_name %in% cols_with_zeros) {
    plist$zbar <- make_zeroprop_plot(.data, var_name)
    plist$pbz2 <- make_boxplot(data.proc.1df, var_name, nonzero = T, outliers = F)
    plist$pvz2 <- make_violin(data.proc.1df, var_name, nonzero = T, outliers = F)
    if (!skip_outliers) {
      plist$pbz1 <- make_boxplot(data.proc.1df, var_name, nonzero = T, outliers = T)
      plist$pvz1 <- make_violin(data.proc.1df, var_name, nonzero = T, outliers = T)
    }
  }
  
  # Return all of our plots as a list
  return(plist)
}

# Create plots ------------------------------------------------------------

# Identify the plottable columns (numeric or logical) and give us a vector of
# their names
plottable_col.names <- data.proc.1df %>% 
  keep(~ is.numeric(.) | is.logical(.)) %>% 
  names()

# Loop over plottable columns, generating plots for each
ggs <- map(plottable_col.names, make_all_plots, .data = data.proc.1df, 
           skip_outliers = opts$skip_outliers) %>% 
  flatten() %>% 
  map(ggplotGrob) # Prevents blank first page
cat(sprintf("Generated %d plots\n", length(ggs)))

# Create grid dimensions
if (opts$compress) {
  page_grid_shape <- c(4, 2)
} else {
  page_grid_shape <- c(1, 1)
}

# Export the resulting plot list
ggsave(
  filename = opts$output_file_name,
  plot = marrangeGrob(ggs, nrow = page_grid_shape[1], ncol = page_grid_shape[2],
                      layout_matrix = matrix(1:prod(page_grid_shape), 
                                             nrow = page_grid_shape[1],
                                             ncol = page_grid_shape[2],
                                             byrow = TRUE)),
  width = 7, height = 9
)
cat(sprintf("Plots have been saved in %s\n", opts$output_file_name))
