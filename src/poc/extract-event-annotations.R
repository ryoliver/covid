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
invisible(assert_that(file.exists(.dbPF)))
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

evt_tb <- dbGetQuery(db, 'SELECT individual_id,event_id from event_clean')

evt_ghm <- fread(paste0(.datPF,"event_ghm.csv"))

message("join tables")
events <- fread(paste0(.datPF,"event_sg.csv")) %>%
  left_join(., event_gh, by = "event_id") %>%
  left_join(., evt_tb, by = "event_id") %>%
  left_join(., ind_tb, by = "individual_id") %>%
  filter(taxon_canonical_name %in% c("Puma concolor", "Anas cyanoptera", "Anas strepera","Corvus corax"))

message("write out results")
fwrite(events, paste0(.outPF, "events.csv"))