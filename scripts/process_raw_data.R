# This script provides functions that normalize the raw protein data according
# to how Victor has been doing in his excel files. It will need to be run in an
# environement with an opts list which includes data_dir and control_file.

# Created by Daniel Kassler on 4/19/21

# Suppressing startup messages so we don't flood the terminal when running from
# command line. Comment out this line and the closing bracket to load these
# packages with all their diagnostic noise.
suppressPackageStartupMessages({
  library(tidyverse) # Keep last to avoid overriding key functions
})

# Control strings and keys ------------------------------------------------

# These are all of the UNTRANSFORMED columns that will get the 100*x/Seqlen
# treatment. 
basic_cols_to_normalize = {c(
  "Order_AAs",
  "Disorder_AAs",
  "Neither_AAs",
  "#Helix",
  "#Strand",
  "#Loop",
  "%Expose",
  "#S-S",
  "#Cys",
  "#FlexDR",
  "#LongDR",
  "#Prol",
  "#Pbind",
  "#Dbind",
  "#TMD",
  "TMD_Tot_Len",
  "#Waltz",
  "Waltz_Tot_len",
  "#Tango",
  "#PfamA_Doms",
  "#PfamB_Doms",
  "#D-boxes",
  "#KEN-boxes",
  "#D+KEN",
  "IEP",
  "#S+T+Y",
  "S+T+Y-Max",
  "S-Max",
  "S-unsp",
  "S-CaM-II",
  "S-PKC",
  "T-Max",
  "T-unsp",
  "T-CaM-II",
  "T-PKC",
  "Y-Max",
  "Y-unsp",
  "Y-INSR",
  "PAPA_Max_Score",
  "#coils"
)}

# These are a list of DERIVED columns to get the 100*x/Seqlen treatment. Some
# columns may be manually normalized in the processing function because their
# normalized version is used in the calculation for another derived column.
derived_cols_to_normalize = {c(
  "Tango_Tot_len",
  "Coils_Tot_Len",
  "Total charge: (Positive - Negative)",
  "Total Absolute charge: (Positive + Negative)",
  "Total Relative charge: (Positive - Negative)/(Positive + Negative)",
  # "Volume", #volume is done manually since it's needed sooner
  "Cost"
)}

# This contains the final column order. Any variable NOT included in this list
# WILL NOT BE INCLUDED IN THE FINAL OUTPUT.
final_col_order <- {c(
  "Name",
  "XML_file",
  "Sequence",
  "Seqlen",
  "Gene_Name",
  "Gene_Desc",
  "Phylostratum",
  "Order_AAs",
  "Norm-Order_Aas: W C F I Y V L",
  "Disorder_AAs",
  "Norm-Disorder_Aas: K E P S Q R A",
  "Neither_AAs",
  "Norm-Neither_Aas: H M G D N T",
  "% (H+Sh)",
  "Helix%",
  "Strand%",
  "Loop%",
  "Other%",
  "#Helix",
  "Norm-#Helix",
  "Helix_avg",
  "Helix_stddev",
  "#Strand",
  "Norm-#Strand",
  "(Norm#Helix - Norm#Strand)/(Norm#Helix + Norm#Strand)",
  "Strand_avg",
  "Strand_stddev",
  "#Loop",
  "Norm-#Loop",
  "Loop_avg",
  "Loop_stddev",
  "%Expose",
  "SizeNorm-%Expose",
  "InvSizeNorm-%Expose",
  "New-Norm-lbExpose",
  "ApproxFinalNorm-%Exposed",
  "InvApproxFinalNorm-%Exposed",
  "%Buried",
  "#S-S",
  "Norm-#S-S",
  "S-Slen",
  "NormS-S_len",
  "#Cys",
  "Norm-#Cys",
  "Norm-#S-S_Potential",
  "S-S realized/possible",
  "#Disordered",
  "%Disordered",
  "#FlexDR",
  "Norm-#FlexDR",
  "#LongDR",
  "Norm-#LongDR",
  "#Prol",
  "Norm-#Pro",
  "#Pbind",
  "Norm-#Pbind",
  "#Dbind",
  "Norm-#Dbind",
  "SP_Hstart",
  "SP_Hend",
  "SP_end",
  "SP_len",
  "#NLS",
  "NLS_start",
  "NLS_end",
  "NLS_len",
  "#TMD",
  "Norm-#TMD",
  "TMD_Tot_Len",
  "Norm-TMD_Tot_Length",
  "TMD_avg",
  "TMD_stddev",
  "TP_loc",
  "TP_conf",
  "TP_len",
  "#Waltz",
  "Norm-#Waltz",
  "Waltz_Tot_len",
  "Norm-Waltz_Tot_len",
  "Waltz_avg",
  "Waltz_stddev",
  "#Tango",
  "Norm-#Tango",
  "(Tango+Waltz)/2",
  "Tango_Tot_len",
  "Norm-Tango_Tot_len",
  "Tango_avg",
  "Tango_stddev",
  "Tango_aggreg%",
  "Avg(Norm-Tango_Tot_len + Norm_Waltz_Tot_len)",
  "#PfamA_Doms",
  "Norm-#PfamA_Doms",
  "%CovA",
  "#Clans",
  "TopClan",
  "%InTop",
  "#PfamB_Doms",
  "Norm-#PfamB_Doms",
  "%CovB",
  "#D-boxes",
  "Norm-D-boxes",
  "#KEN-boxes",
  "Norm-#KEN-boxes",
  "#D+KEN",
  "Norm-#(D+KEN)",
  "IEP",
  "Norm-IEP",
  "#S+T+Y",
  "Norm-#(S+T+Y)",
  "S+T+Y-Max",
  "Norm-#(S+T+Y)-Max",
  "S-Max",
  "Norm-S-Max",
  "S-unsp",
  "Norm-S-unsp",
  "S-CaM-II",
  "Norm-S-CaM-II",
  "S-PKC",
  "Norm-S-PKC",
  "T-Max",
  "Norm-T-Max",
  "T-unsp",
  "Norm-T-unsp",
  "T-CaM-II",
  "Norm-T-CaM-II",
  "T-PKC",
  "Norm-T-PKC",
  "Y-Max",
  "Norm-Y-Max",
  "Y-unsp",
  "Norm-Y-unsp",
  "Y-INSR",
  "Norm-Y-INSR",
  "#S-Max/#S",
  "#T-Max/#T",
  "#Y-Max/#Y",
  "#(S+T+Y)-Max/#(S+T+Y)",
  "PAPA_Max_Score",
  "Norm-#PAPA_Max_score",
  "PAPA_Position",
  "#coils",
  "Norm-#Coils",
  "Avg_Len",
  "Coils_Tot_Len",
  "Norm-Coils_Tot_len",
  "%Coil",
  "Last6AA",
  "Chromosome",
  "Gene_Start",
  "Gene_End",
  "Strand",
  "Uniprot evidence text",
  "Uniprot evidence #",
  "Has UP protein evidence",
  "Has UP transcript evidence",
  "%A",
  "%C",
  "%D",
  "%E",
  "%F",
  "%G",
  "%H",
  "%I",
  "%K",
  "%L",
  "%M",
  "%N",
  "%P",
  "%Q",
  "%R",
  "%S",
  "%T",
  "%V",
  "%W",
  "%Y",
  "Basic AA: H,R,K",
  "Non-polar hydrophobic: F,A,L,M,I,W,P,V",
  "Polar uncharged: C,G,Q,N,S,Y,T",
  "Acidic AA: D,E",
  "ALL",
  "Gly/Pro",
  "Total charge: (Positive - Negative)",
  "Norm-Total_charge",
  "Total Absolute charge: (Positive + Negative)",
  "Norm-Total Absolute charge: (Positive + Negative)",
  "Total Relative charge: (Positive - Negative)/(Positive + Negative)",
  "Norm-Total Relative charge: (Positive - Negative)/(Positive + Negative)",
  "Volume",
  "Norm-Volume of 100 AA",
  "Norm-AA Volume per AA",
  "Protein radius",
  "Norm-Protein radius 100 AA",
  "Essential AA: F, V, T, W, M, L, I, K H",
  "Non-essential AA: R,C,G,Q,P,Y, A, D, N, E, S",
  "Conditionally essential: R,C,G,Q,P,Y",
  "Dispensable: A,D,N,E,S",
  "AT- only genetic code:I,F,L,Y,N,K",
  "Hydrophobic: V,I,L,F,M,W,Y,C",
  "Positive:R,K,H",
  "NegativeD,E",
  "Conformational:G,P",
  "Polar:N,Q,S",
  "A & T",
  "Cost",
  "Norm-Cost per 100 AA",
  "Norm-AA Cost per AA",
  "Oiliness: F,I,L,V",
  "%Exposed/Volume",
  "%Exposed/(Length^3)"
)}
# TODO: add in checks that we're not throwing out any columns due to errors in
# manually editing the order process

# Most normalized columns follow a pattern of Norm-x for their column name, but
# some don't. Here, we control any columns that will need to be manually renamed
# away from this pattern (note we start by assuming they will get renamed
# according to the standard pattern, and then do ANOTHER rename afterwards for
# the ones that need to change.)
rename_key_1 <- c( # The basic columns to rename
  `Norm-%Expose`        = "ApproxFinalNorm-%Exposed",
  `Norm-#Prol`          = "Norm-#Pro",
  `Norm-TMD_Tot_Len`    = "Norm-TMD_Tot_Length",
  `Norm-S+T+Y-Max`      = "Norm-#(S+T+Y)-Max",
  `Norm-PAPA_Max_Score` = "Norm-#PAPA_Max_score",
  `Norm-#coils`         = "Norm-#Coils",
  `Norm-#D+KEN`         = "Norm-#(D+KEN)",
  `Norm-#D-boxes`       = "Norm-D-boxes",
  `Norm-#S+T+Y`         = "Norm-#(S+T+Y)",
  `Norm-Disorder_AAs`   = "Norm-Disorder_Aas: K E P S Q R A",
  `Norm-Neither_AAs`    = "Norm-Neither_Aas: H M G D N T",
  `Norm-Order_AAs`      = "Norm-Order_Aas: W C F I Y V L"
)
rename_key_2 <- c( # Derived columns to rename
  `Norm-Coils_Tot_Len`                       = "Norm-Coils_Tot_len",
  `Norm-Total charge: (Positive - Negative)` = "Norm-Total_charge",
  `Norm-Cost`                                = "Norm-Cost per 100 AA"
)


# Normalizing function ----------------------------------------------------

create_derived_cols <- function(.data) {
  .data %>% 
  mutate(
    #These are nonstandard transformed columns (ones that are not in the basic
    #100*x/Seqlen). They are all in the format of "Output column name" =
    #FORMULA. NB: A *few* 100*x/Seqlen columns exist, due to dependencies on
    #transformed colums.
    "% (H+Sh)" =
        `Helix%` + `Strand%`,
    "(Norm#Helix - Norm#Strand)/(Norm#Helix + Norm#Strand)" =
        (`Norm-#Helix` - `Norm-#Strand`) / (`Norm-#Helix` + `Norm-#Strand` + 1e-15),
    "SizeNorm-%Expose" =
        `%Expose` / Seqlen^(1/3),
    "InvSizeNorm-%Expose" =
        `%Expose` * Seqlen^(1/3),
    "New-Norm-lbExpose" =
        `%Expose` * Seqlen / (6 * Seqlen^(2/3) - 12 * Seqlen^(1/3) + 8),
    "InvApproxFinalNorm-%Exposed" = 
        1e-3 * `%Expose` * Seqlen,
    "NormS-S_len" = 
        `S-Slen` / Seqlen,
    "Norm-#S-S_Potential" = 
        (100 * `#Cys` / Seqlen)/2,
    "S-S realized/possible" = 
        `Norm-#S-S` / `Norm-#S-S_Potential`,
    "(Tango+Waltz)/2" = 
        (`Norm-#Tango` + `Norm-#Waltz`) / 2,
    "Tango_Tot_len" = 
        `Tango_avg` * `#Tango`,
    "Avg(Norm-Tango_Tot_len + Norm_Waltz_Tot_len)" = 
        100*(`Tango_Tot_len`/Seqlen + `Waltz_Tot_len`/Seqlen) / 2,
    "#S-Max/#S" = 
        100 * `S-Max` / (`%S` * Seqlen),
    "#T-Max/#T" =
        100 * `T-Max` / (`%T` * Seqlen),
    "#Y-Max/#Y" =
        100 * `Y-Max` / (`%Y` * Seqlen),
    "#(S+T+Y)-Max/#(S+T+Y)" =
        `S+T+Y-Max` / `#S+T+Y`,
    "Coils_Tot_Len" =
        `#coils` * `Avg_Len`,
    "Basic AA: H,R,K" =
        `%H` + `%R` + `%K`,
    "Non-polar hydrophobic: F,A,L,M,I,W,P,V" = 
        `%F` + `%A` + `%L` + `%M` + `%I` + `%W` + `%P` + `%V`,
    "Polar uncharged: C,G,Q,N,S,Y,T" =
        `%C` + `%G` + `%Q` + `%N` + `%S` + `%Y` + `%T`,
    "Acidic AA: D,E" =
        `%D` + `%E`,
    "ALL" = 
        `Basic AA: H,R,K` + `Non-polar hydrophobic: F,A,L,M,I,W,P,V` + 
        `Polar uncharged: C,G,Q,N,S,Y,T` + `Acidic AA: D,E`,
    "Gly/Pro" = 
        `%G` / `%P`,
    "Total charge: (Positive - Negative)" = 
        (`Basic AA: H,R,K` - `Acidic AA: D,E`) * Seqlen / 100,
    "Total Absolute charge: (Positive + Negative)" = 
        (`Basic AA: H,R,K` + `Acidic AA: D,E`) * Seqlen / 100,
    "Total Relative charge: (Positive - Negative)/(Positive + Negative)" =
        `Total charge: (Positive - Negative)` / `Total Absolute charge: (Positive + Negative)`,
    "Volume" = Seqlen * ((`%A`*88.6+
                          `%C`*108.5+
                          `%D`*111.1+
                          `%E`*138.4+
                          `%F`*189.9+
                          `%G`*60.1+
                          `%H`*153.2+
                          `%I`*166.7+
                          `%K`*168.6+
                          `%L`*166.7+
                          `%M`*162.9+
                          `%N`*114.1+
                          `%P`*112.7+
                          `%Q`*143.8+
                          `%R`*173.4+
                          `%S`*89+
                          `%T`*116.1+
                          `%V`*140+
                          `%W`*227.2+
                          `%Y`*193.6) / 100),
    "Norm-Volume of 100 AA" =
        100 * Volume / Seqlen,
    "Norm-AA Volume per AA" = 
        Volume / Seqlen,
    "Protein radius" = 
        (Volume * 3 / (4 * pi))^(1/3),
    "Norm-Protein radius 100 AA" = 
        (`Norm-Volume of 100 AA` * 3 / (4 * pi))^(1/3),
    "Essential AA: F, V, T, W, M, L, I, K H" = 
        `%F` + `%V` + `%T` + `%W` + `%M` + `%L` + `%I` + `%K` + `%H`,
    "Non-essential AA: R,C,G,Q,P,Y, A, D, N, E, S" = 
        `%R` + `%C` + `%G` + `%Q` + `%P` + `%Y` + `%A` + `%D` + `%N` + `%E` + `%S`,
    "Conditionally essential: R,C,G,Q,P,Y" =
        `%R` + `%C` + `%G` + `%Q` + `%P` + `%Y`,
    "Dispensable: A,D,N,E,S" = 
        `%A` + `%D` + `%N` + `%E` + `%S`,
    "AT- only genetic code:I,F,L,Y,N,K" = 
        `%I` + `%F` + `%L` + `%Y` + `%N` + `%K`,
    "Hydrophobic: V,I,L,F,M,W,Y,C" = 
        `%V` + `%I` + `%L` + `%F` + `%M` + `%W` + `%Y` + `%C`,
    "Positive:R,K,H" =
        `%R` + `%K` + `%H`,
    "NegativeD,E" = 
        `%D` + `%E`,
    "Conformational:G,P" =
        `%G` + `%P`,
    "Polar:N,Q,S" =
        `%N` + `%Q` + `%S`,
    "A & T" = 
        `%A` + `%T`,
    "Cost" = Seqlen * ((`%A`*14.5+
                        `%C`*26.5+
                        `%D`*15.5+
                        `%E`*9.5+
                        `%F`*61+
                        `%G`*14.5+
                        `%H`*29+
                        `%I`*38+
                        `%K`*36+
                        `%L`*37+
                        `%M`*36.5+
                        `%N`*18.5+
                        `%P`*14.5+
                        `%Q`*10.5+
                        `%R`*20.5+
                        `%S`*14.5+
                        `%T`*21.5+
                        `%V`*29+
                        `%W`*75.5+
                        `%Y`*59) / 100),
    "Norm-AA Cost per AA" =
        Cost / Seqlen,
    "Oiliness: F,I,L,V" = 
        `%F` + `%I` + `%L` + `%V`,
    "%Exposed/Volume" =
        100 * `%Expose` / Volume,
    "%Exposed/(Length^3)" =
        10^6 * `%Expose` / (Seqlen^3)
  )
}

# This is the main processing pipeline, which wraps all the components above
# together.
process_raw <- function(.data) {
  .data %>% 
    # Normalize all the basic columns with standard Seqlen normalization
    mutate(across(.cols = all_of(basic_cols_to_normalize),
                  .fn = ~ 100 * . / Seqlen,
                  .names = 'Norm-{col}')) %>%
    # Rename those columns which have special names
    rename_with(.cols = all_of(names(rename_key_1)),
                .fn = ~ rename_key_1[.]) %>% 
    # Create the derived columns - those with special nonstandard formulae
    create_derived_cols() %>% 
    # Normalize any derived columns that subsequently get the standard norm
    mutate(across(.cols = all_of(derived_cols_to_normalize),
                  .fn = ~ 100 * . / Seqlen,
                  .names = 'Norm-{col}')) %>%
    # Rename derived columns whose normed versions have special names
    rename_with(.cols = all_of(names(rename_key_2)),
                .fn = ~ rename_key_2[.]) %>% 
    # Select and reorder columns based on the specified column order
    select(all_of(final_col_order))
}

# Load data ---------------------------------------------------------------

control_table <- read_csv(
  opts$control_file,
  col_types = cols(
    ShortName = col_character(),
    DisplayName = col_character(),
    Path = col_character(),
    Color = col_character()
  )
)


# Specifiy column types manually which we don't want readr to guess for us
column_spec <- cols(Chromosome = col_character(),
                    Phylostratum = col_factor())

# Assemble paths for data, and loop over them reading in the files one-by-one.
data.raw.list <- control_table$Path %>% 
  file.path(opts$data_dir, .) %>% 
  map(., read_csv, na = c('', "N/A"), 
      col_types = column_spec) %>% 
  set_names(control_table$ShortName)


# Process data ------------------------------------------------------------

# Process the raw data using the functions defined in this script
data.proc.list <- map(data.raw.list, process_raw)
