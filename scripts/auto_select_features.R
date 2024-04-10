# This script automatically creates two lists of features, one for
# normalized-percent and one for counts. It's based on the algorithm that Kris
# used in the runCorViridis script.

suppressPackageStartupMessages({
  library(tidyverse)
})

#TODO: source process_raw_data, which requires that I can pass it an opts list
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
  # Optional argument that controls the folder where we look for backend
  # scripts. Useful if you care a lot about making this work with your current
  # directory structure.
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


# Source data -------------------------------------------------------------

# This file creates the processed data block
source(file.path(opts$backend_dir, 'process_raw_data.R'))

# Discard names (manual) --------------------------------------------------

# This variable contains every column name manually excluded from the analysis, including
# the first 7 columns.
discard_names <- 
  c( "Name", "XML_file", "Sequence", "Gene_Name", "Gene_Desc", "%Buried", "Helix_stddev", 
     "Strand_stddev", "Loop_stddev", "%Buried", "#Cys", "Norm-#Cys", "Norm-#S-S_Potential ",
     "Norm-#S-S_Potential", "#Prol", "Norm-#Pro", "#NLS", "NLS_start", "NLS_end", "NLS_len",
     "TMD_stddev", "TP_loc", "TP_conf", "Waltz_stddev", "Tango_stddev", "TopClan",
     "#PfamB_Doms", "Norm-#PfamB_Doms", "%CovB", "Norm-IEP", "PAPA_Position", "%Coil",
     "Last6AA", "Chromosome", "Gene_Start", "Gene_End", "Strand", "Uniprot evidence text",
     "Has UP protein evidence", "Has UP transcript evidence", "ALL", "Norm-Volume of 100 AA",
     "Norm-Protein radius 100 AA", "Non-essential AA: R,C,G,Q,P,Y, A, D, N, E. S",
     "NegativeD,E", "Positive:R,K,H", "Norm-Cost per 100 AA", "T-Max", "Neither_AAs",
     "Volume", "Disorder_AAs", "Total Absolute charge: (Positive + Negative)", "S-Max",
     "S-S realized/possible", "#Tango", "#Waltz", "MD_Tot_Len", "Tango_avg", "#KEN-boxes",
     "#D-boxes", "Avg_Len", "Coils_Tot_Len", 
     "Total Relative charge: (Positive - Negative)/(Positive + Negative)",
     "Conditionally essential: R,C,G,Q,P,Y", "A & T", "Gly/Pro", "Waltz_avg", "TMD_avg",
     "SP_end", "SP_Hend", "SP_Hstart", "Norm-#Waltz", "Norm-TMD_Tot_Length", "Norm-#Tango",
     "Tango_aggreg%", "%V", "Norm-#Loop", "%G", "Norm-T-CaM-II", "Norm-S-CaM-II", "Norm-Y-Max",
     "Norm-Y-INSR", "Norm-Total_charge", "SizeNorm-%Expose", "ApproxFinalNorm-%Exposed",
     "Norm-Disorder_Aas: K E P S Q R A", "%A", "%P", "Norm-#KEN-boxes", "Norm-D-boxes",
     "InvApproxFinalNorm-%Exposed", "InvSizeNorm-%Expose", "%Q", "Norm-#Coils", "%K",
     "Norm-S-PKC", "Norm-S-Max", "Norm-#(S+T+Y)-Max", "Norm-T-PKC", "Norm-T-Max", "%T", "Y-Max")  

# Feature selection -------------------------------------------------------

# Norm percent names are everything that matches a regex, and we assume count
# names to be everything else. This procedure is taken from Kris' correlation
# script, runCorViridis.R
kept_names <- setdiff(final_col_order, discard_names)
norm_pct_names <- grep('*Norm|%|avg|/|radius)*', kept_names, value = T)
count_names <- setdiff(kept_names, norm_pct_names)


# Write output ------------------------------------------------------------

write(count_names, 'features_count.txt')
write(norm_pct_names, 'features_np.txt')

