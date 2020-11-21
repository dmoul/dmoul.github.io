# regen-risks-data.R
# by Daniel Moul

# This script regenerates the dataframes need for the risk-lens-main.Rmd analysis.
# It will look for cached volume- and issue-level data frames before looking for cached pages.
# So if you really want to regenerate all dataframes, delete ./cached-data/ or any any or all 
# directories or files in that directory.
#
# If the scripts can't find a page they are looking for in ./cached-pages/ they will attempt to 
# scrape the web pages.
#
# Run this from the risks-lens directory:
# Rscript --default-packages=here,stats,utils ./scripts/regen-risks-data.R 

#library(knitr)

rmarkdown::render(here::here("risks-lens", "01-volume-issue-titles-prep.Rmd"))
rmarkdown::render(here::here("risks-lens", "02-issue-detail-prep.Rmd"))
rmarkdown::render(here::here("risks-lens", "03-title-word-analysis-prep.Rmd"))
rmarkdown::render(here::here("risks-lens", "04-body-word-analysis-prep.Rmd"))
# rmarkdown::render(here::here("risks-lens", "09-risks-lens-main.Rmd"))

# submitter analysis content not included in 09-risks-lens-main.Rmd
# rmarkdown::render(here::here("risks-lens", "05-submitter-analysis.Rmd"))
# rmarkdown::render(here::here("risks-lens", "05-submitter-analysis-bodynounphrases.Rmd"))

# content from these scripts are included in 09-risks-lens-main.Rmd, and these scripts may be out of date
# rmarkdown::render(here::here("risks-lens", "06-volume-issue-titles.Rmd"))
# rmarkdown::render(here::here("risks-lens", "07-title-word-analysis.Rmd"))
# rmarkdown::render(here::here("risks-lens", "08-body-word-analysis.Rmd"))

message("Completed")
beepr::beep(sound = 3)