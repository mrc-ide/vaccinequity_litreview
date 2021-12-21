# run all

source("A0-quality-assessment.R")
source("A-ingest-data.R")
source("B-visualisations.R")
source("C-metaanalysis.R")
rmarkdown::render("D-thematic-summary.Rmd")
