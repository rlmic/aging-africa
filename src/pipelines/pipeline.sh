# 0. Setup
pdoc='/Applications/RStudio.app/Contents/MacOs'
gene_date=$(date '+%y%m%d')
ssa_pat='/Users/danielapintoveizaga/Dropbox (CEGA)/Demography of Aging in SSA/data/'
ssa_dat='../../data/preprocessed/'
#reports='../outputs/reports/1-launch/survey/'
nbook='../../notebooks/'

### 1. Update content to lastest version of survey

cp "${ssa_pat}allcountries.dta" "${ssa_dat}"
cp "${ssa_pat}population_projections_t1.dta" "${ssa_dat}"
cp "${ssa_pat}summaryindicators.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_weightspopsurvey.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_noweights.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_weightspopsurvey_all.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_weightspopsurvey_countries.dta" "${ssa_dat}"
cp "${ssa_pat}population_projections_allcountries.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_noweights_all.dta" "${ssa_dat}"
cp "${ssa_pat}allcountries_means5year_noweights_countries.dta" "${ssa_dat}"
### 2. Plots Aging Paper

#echo "Aging in Africa"
#input="${nbook}plots_paper_aging.Rmd"
#output="plots_paper_aging.pdf"
#R -e "Sys.setenv(RSTUDIO_PANDOC='$pdoc'); rmarkdown::render('$input', output_file='$output')"  

