# This script provides functions for creating the blocks of statistics Victor
# wants to see at the bottom of his main analysis sheet.

# Created by Daniel Kassler on 4/21/21


# Import and setup for testing --------------------------------------------

library(moments)

# Utility functions -------------------------------------------------------

# Manually calculate mode, because R doesn't have a builtin for it
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Basic stat block --------------------------------------------------------

# This function calculates some basic statistics for a single vector (one column
# of raw data) and returns them as a one-row data frame. This output format
# is so that we can line them all up as a giant data frame of summary data for
# easy viewing. Note that all functions which allow an na.rm argument have it
# set to TRUE so that we still compute the statistic in the presence of NA
# values. This may not be appropriate in the presence of a large proportion of
# NAs in the data, so future uses are encouraged to think over whether this
# makes sense in their context.
calculate_basic_stats <- function(x, non.num = 'na') {
  # non.num specifies what to do with a non-numeric input. 'stop' fails with an
  # error, 'na' passes a row of nas, and 'null' passes a zero-length data frame
  # that can be rbind-ed with other output data frames from this function.
  if (!(is.numeric(x) | is.logical(x))) {
    if (non.num == 'stop') 
      stop("Non numeric argument. Set non.num to 'na' or 'null' to safely pass.")
    else if (non.num == 'na') {
      return(tibble(
        Average = NA, 
        STD = NA,
        CV = NA,
        SEM = NA,
        SEM_over_mean = NA,
        Median = NA,
        Mode = NA, 
        Skew = NA,
        Kurtosis = NA
      ))
    } else if (non.num == 'null') {
      return(tibble(
        Average = integer(0), 
        STD = integer(0),
        CV = integer(0),
        SEM = integer(0),
        SEM_over_mean = integer(0),
        Median = integer(0),
        Mode = integer(0), 
        Skew = integer(0),
        Kurtosis = integer(0)
      ))
    } else 
      stop("`non.num` must be one of 'stop', 'na', or 'null'")
  }
    
  # Output a tibble of the basic statistics we are interested in.
  tibble(
    Average = mean(x, na.rm = TRUE),
    STD = sd(x, na.rm = TRUE),
    CV = STD / Average,
    SEM = STD / sqrt(sum(!is.na(x))), #NB: not counting NAs in denominator
    SEM_over_mean = SEM / Average,
    Median = median(x, na.rm = TRUE),
    Mode = getmode(x),
    Median_over_avg = Median / Average,
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Range = Max - Min,
    Percentile_75 = quantile(x, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(x, 0.25, na.rm = TRUE),
    IQR = Percentile_75 - Percentile_25,
    IQR_over_range = IQR / Range,
    Skew = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  )
}

# Loops the above function over every column of a data frame, returning a data
# frame with one row per input column. Broken into its own function for
# modularity, and to make it easier to add functionality to this step later own.
basic_stat_block <- function(.data, ...) {
  .data %>% 
    map_dfr(calculate_basic_stats, .id = "Variable", ...)
}


# T-tests and other statistical testing -----------------------------------

# Drop in function to do the t-tests we want
ttest_pval <- function(x, y, non.num = 'warn') {
  if (!(is.numeric(x) | is.logical(x)) | !(is.numeric(y) | is.logical(y))) {
    if (isTRUE(non.num == 'na')) return(NA)
    else if (isTRUE(non.num == 'null')) return(NULL)
    else { 
      warning("Argument to t test is not numeric or logical. Returning NA")
      return(NA)
    }
  }
  tryCatch(t.test(x, y)[['p.value']], 
           error = function(e) NA)
           #error = function(e) {browser(); warning(e); NA})
}

# Drop in function to do the Wilcoxon-Mann-Whitnew rank-sum test
wtest_pval <- function(x, y, non.num = 'warn') {
  if (!(is.numeric(x) | is.logical(x)) | !(is.numeric(y) | is.logical(y))) {
    if (isTRUE(non.num == 'na')) return(NA)
    else if (isTRUE(non.num == 'null')) return(NULL)
    else {
      warning("Argument to Mann-Whiteney test is not numeric or logical. Returning NA")
      return(NA)
    }
  }
  tryCatch(wilcox.test(x, y)[['p.value']], 
           error = function(e) NA)
           #error = function(e) {warning(e); NA})
}

# Run tests on all unordered pairwise combos of variables in a list. Test is one
# of our drop-in replacement testing functions, which will be applied to pairs
# of variables
test_combos <- function(l, test, ...) {
  # Generate a list of combinations of the elements in l
  pairs_xy <- combn(l, 2, simplify = FALSE)
  # ... then do the t-test for each
  tp <- map2_dbl(map(pairs_xy, 1), map(pairs_xy, 2), ~test(.x, .y))
  
  # Create columns for names, and return the whole thing as a data frame
  pairs_names.df <- combn(names(l), 2, simplify = TRUE) %>% t() %>% 
    as_tibble() %>% set_names(c("V1", "V2"))
  pairs_names.df %>% mutate(pval = tp)
}

# Given a list of data frames, apply the pairwise testing to the variables in
# it. Note that we rely on the list being a NAMED list- if it wasn't, we
# wouldn't have much in the way of intelligible output anyway. 
test_across_dfs <- function(l, test, wide = TRUE, mc = "holm", n = NULL, ...) {
  df_names <- names(l)
  df_vars <- map(l, names) %>% unlist() %>% unname() %>% unique() %>% set_names()
  out <- map_dfr(df_vars, ~ {test_combos(map(l, .), test, ...)}, .id = "Variable")
  
  # Adjust p-values with the specified method
  if (is.null(n)) n <- length(out$pval)
  out <- out %>% mutate(pval = p.adjust(pval, method = mc, n = n))
  
  # Here we optimize for display with a wide-format. If desired, can be switched
  # off and a long-format data frame will be returned, which is better for
  # computation and plotting with ggplot.
  if (wide) {
    out <- out %>% pivot_wider(names_from = Variable, values_from = pval)
  }
  return(out)
}
