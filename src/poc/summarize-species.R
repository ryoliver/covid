if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/out/event-annotation/'
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/out/event-annotation/'
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))

message("initialize db")
db <- dbConnect(RSQLite::SQLite(), .dbPF)


ind_tb <- dbGetQuery(db, 'SELECT individual_id,taxon_canonical_name from individual') %>%
  collect()

summary <- ind_tb %>%
  group_by(taxon_canonical_name) %>%
  summarise("n_individuals" = n_distinct(individual_id))

fwrite(summary, paste0(.outPF, "species-individual-summary.csv"))
