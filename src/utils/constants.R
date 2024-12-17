# constants.R - Configuration and Constants for Demographic Analysis

# ---------------------------------
# 1. PATHS AND DIRECTORIES
# ---------------------------------
# Define path to preprocessed data
data_path <- "../data/preprocessed/"

# ---------------------------------
# 2. DATASET NAMES AND FILES
# ---------------------------------
# List of datasets to load (ensure filenames are consistent and correct)

files_to_load <- c(
  "allcountries.dta",
  "population_projections_t1.dta",
  "summaryindicators.dta",
  "allcountries_means5year_weightspopsurvey.dta", 
  "allcountries_means5year_noweights.dta",
  "population_projections_allcountries.dta", 
  "allcountries_means5year_weightspopsurvey_all.dta",
  "allcountries_means5year_noweights_all.dta",
  "allcountries_means5year_weightspopsurvey_countries.dta",
  "allcountries_means5year_noweights_countries.dta", 
  "allcountries_means5year_weightspopsurvey_countrygender.dta",
  "allcountries_means5year_noweights_countrygender.dta"
)

# Corresponding names for datasets
names_datas <- c(
  "ssa",
  "projections",
  "dat",
  "ssa_agg", 
  "ssa_agg_non",
  "pop_pro", 
  "subsaharan_weighted",
  "subsaharan_nonweighted",
  "countries_weighted",
  "countries_nonweighted", 
  "gen_wgt",
  "gen_nwg"
)

# ---------------------------------
# 3. CONSTANTS FOR DATA PROCESSING
# ---------------------------------

# Order of geographical regions or categories
order <- c("Sub-Saharan Africa", "India", "China", "Rest of World")

# Countries to keep for comparative analysis
keepobs = c('World', 'India', 'China', 'Sub-Saharan Africa')

# Countries for comparison
coun_comp <- c("China", "India", "Sub-Saharan Africa")

# ---------------------------------
# 4. COLOR SCHEMES
# ---------------------------------
# Define colors for visualizations
c_green = "#8eb67d"
colors_scheme = c("#9b111e", "#1f77b4",  c_green)
pal = c(
  # sub-saharan africa
  c_green,
  # india
  "#1f77b4",
  # china
  "#9b111e",
  # rest of the world
  "#E2E4E8"
  
)

# Gender-based color scheme
colors_gende =  c("Female" = "#8eb67d", "Male" = "#000C66")

colo = c(
  "black", 
  "#3f3f3f", 
  "#8eb67d"
)

# ---------------------------------
# 5. LABELS AND BREAKS FOR PLOTS
# ---------------------------------
# Define common label size and other values
sz_tot <- 6
size_share <- 6

breaks_numer = c(1990, 2020, 2050, 2100)
breaks_text = c("1990","2020","2050", "2100")

countries = c(
  "South Africa",
  "Ghana", 
  "Nigeria", 
  "Tanzania", 
  "Ethiopia",
  "Uganda",
  "Malawi",
  "Niger"
)

regions = c(
  "Average", 
  '1.- Eastern Africa', 
  '2.- Southern Africa', 
  '3.- Western Africa'
)

# Define lookup tables for mappings
country_mapping <- c(
  'eth' = 'Ethiopia', 'mlw' = 'Malawi', 'nga' = 'Nigeria',
  'tza' = 'Tanzania', 'gha' = 'Ghana', 'saf' = 'South Africa',
  'uga' = 'Uganda', 'niger' = 'Niger'
)

region_mapping <- c(
  'Ethiopia' = '1.- Eastern Africa', 'Tanzania' = '1.- Eastern Africa', 'Uganda' = '1.- Eastern Africa',
  'Ghana' = '3.- Western Africa', 'Nigeria' = '3.- Western Africa', 'Niger' = '3.- Western Africa',
  'Malawi' = '2.- Southern Africa', 'South Africa' = '2.- Southern Africa'
)

female_map <- list('female' = c('0' = 'Male', '1' = 'Female'))

var_sum = c(
  "govtexpgdp", 
  "oopexp", 
  "uhcidx",
  "socexpnohealth", 
  "lifeexpsixtytotal",
  "pensioncoverage",
  "hospbeds",
  "physicians",
  "country", 
  "cou"
)

ids = c("gender", "age_group5")


indicators <- c(
  'lifeexpsixtytotal',
  'socexpnohealth',
  'pensioncoverage'
)


indicato_labeller <- c( 
  'lifeexpsixtytotal'="**A.-** Remaining Life<br>Expectancy At age 60<br>(Years)<br>",
  'socexpnohealth'="**B.-** Social Protection<br> Expenditure on Elderly<br>(Percentage of GDP)<br>",
  'pensioncoverage'="**C.-** Pension<br>Coverage<br>(Percentage)<br>"
)

measures_labs <- c(
  "Functional\nLimitations\n(Proportion)", 
  "Illness\n&\nInjury\n(Proportion)", 
  "Stopped\nActivities\n(Proportion)", 
  "Depression\nOr Psych\nDistress\n(Proportion)",
  "Employment\nStatus\n(Proportion)",
  "Weekly\nEmployment\nStatus\n(Hours)", 
  "Unmarried\n(Proportion)", 
  "Living alone\n(Proportion)", 
  "Urban Status\n(Proportion)"
)

measures = c(
  "disability", 
  "ill_inj", 
  "ill_inj_stopactivity", 
  "depressed", 
  "work_any", 
  "work_hrs",
  "notmarried", 
  "lives_alone", 
  "urban"
  )


measures_labs_agg <- c(
  "**A.-** Functional<br>Limitations<br>",
  "**B.-** Illness & Injury<br>",
  "**C.-** Stopped Activities<br>", 
  "**D.-** Depression<br> Or Psych Distress<br>", 
  "**E.-** Employment Status<br>",
  "**F.-** Weekly Employed <br> Hours / 40<br>",
  "**G.-** Unmarried<br>",
  "**H.-** Living alone<br>", 
  "**I.-** Urban Status<br>",
  "**J.-** Health<br>Insurance<br>"
)
limitations <- c(
  "diff_seei", 
  "diff_hear",
  "diff_reme",
  "diff_walk_clim",
  "diff_self_care",
  "diff_comm"
)

limitations_labs <- c(
  "**A.-** Seeing<br>",
  "**B.-** Hearing<br>",
  "**C.-** Remembering<br>",
  "**D.-** Walking or Climbing<br>",
  "**E.-** Self Care<br>",
  "**F.-** Communicating<br>"
)

limit_labs  <- c(
  "\u00A0Seeing\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", 
  "Hearing\u00A0\u00A0\u00A0\u00A0\u00A0", 
  "\u00A0\u00A0\u00A0\u00A0Remembering",
  "Walking or \u00A0 \u00A0\u00A0\n Climbing \u00A0 \u00A0\u00A0 ",
  "Self Care\u00A0\u00A0\u00A0\u00A0\u00A0",
  "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0Communicating"
  )
bre_age = c(
  "20",
  "25",
  "30", 
  "35",
  "40", 
  "45",
  "50",
  "55",
  "60", 
  "65", 
  "70", 
  "75",
  "80"
)

lab_age = c(
  "20", 
  "", 
  "", 
  "", 
  "40",
  "", 
  "",  
  "",
  "60",
  "", 
  "", 
  "", 
  "80+"
)

labeller_health <- c(
  'uhcidx'="**A.-** Health Care<br> Service Coverage<br>(Index)<br>",
  'oopexp'="**B.-** 100 - Out of Pocket<br>Health Expenditure<br>(Percentage)<br>",
  `govtexpgdp`="**C.-** Gov. Health<br>Expenditure<br>(Percentage of GDP)<br>",
  'hospbeds'="**D.-** N° of Hospital Beds<br>Per 1,000<br>(Count)<br>",
  'physicians'="**E.-** N° of Physicians<br>Per 1,000<br>(Count)<br>"
)

var_hea = c(
  'uhcidx',
  'oopexp', 
  'govtexpgdp', 
  'hospbeds', 
  'physicians'
)

name_lab <- c(
  NA, NA, NA, NA, '+ 24 %', '+ 60 %', '+ 123 %', NA, 
  '- 8 %', '+ 20 %', '+ 90 %', NA, '- 42 %', '- 8 %', '+ 63 %', NA
  )

measurement_x <- c(
  NA, NA, NA, NA, 1421243, 1600170, 2776948, NA,
  1421243, 1600170, 2776948, NA, 1421243, 1600170, 2776948, NA
  )

lab <- c(
  '1.1 B', '0.9 B', '0.5 B', NA, NA, NA, NA, NA, 
  NA, NA, NA, NA, '0.8 B', '1.5 B', '3.4 B', NA
  )
