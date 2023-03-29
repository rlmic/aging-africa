# 0. Setup
pdoc='/Applications/RStudio.app/Contents/MacOs'
gene_date=$(date '+%y%m%d')
mypath='/Users/cegaadmin/Dropbox (CEGA)/'
#survey="KLPS-5 Project/Data/raw/KLPS5_C-PLUS_Mod_W1_4Analysis.dta"
#reports='../outputs/reports/1-launch/survey/'
nbook='../../notebooks/'

### 1. Update content to lastest version of survey
#cp "${mypath}${survey}" ../../data/raw/1-launch/survey/
##
### 2. Get content from the last 7 days and merge
##python3 data_pipe.py
##
### 3. Generate Outcomes
#Rscript 
##
### 4. Update reports
##echo "Generating Non-Cognitive Report"
##
#### 4.1 Non-cognitive
##input="${nbook}non-cognitive-check.Rmd"
##output="${reports}${gene_date}-non-cognitive-check.pdf"
##R -e "Sys.setenv(RSTUDIO_PANDOC='$pdoc'); rmarkdown::render('$input', output_file='$output')"  
##
#### 4.2 Occupational Complexity
##echo "Generating Occupational Complexity Report"
##input="${nbook}occ-compx-check.Rmd"
##output="${reports}${gene_date}-occ-compx-check.pdf"
##R -e "Sys.setenv(RSTUDIO_PANDOC='$pdoc'); rmarkdown::render('$input', output_file='$output')"  

## 4.3 Outcomes Occupation Complexity
echo "Occupational Complexity Outcomes"
input="${nbook}plots_poster_aging.Rmd"
output="plots_poster_aging.pdf"
R -e "Sys.setenv(RSTUDIO_PANDOC='$pdoc'); rmarkdown::render('$input', output_file='$output')"  

## ## 4.4 Pollution report
## cd ../../
## export PYTHONPATH=$PWD
## jupyter nbconvert                                   \
##     --execute --to notebook                         \
##     --inplace notebooks/contam-check.ipynb
## jupyter nbconvert                                   \
##     --output-dir outputs/reports/1-launch/survey/   \
##     --output "${gene_date}-contam-check"            \
##     --to html                                       \
##     --no-input "notebooks/contam-check.ipynb"       
## 
## 
## rm "${nbook}*.log"
