if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod_20220303.db'
  .datPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  # UPDATE VERSION!!!
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod_20220303.db'
  .datPF <- file.path(.wd,'analysis/')
}

message("start safegraph annotation")
source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)


dbListTables(db)


evt_sg <- tbl(db,'event_sg') %>%
  select(event_id,daily_count) %>%
  collect() 

evt_sg %>% 
  filter(is.na(daily_count)) %>%
  nrow()

evt_census <- tbl(db,'event_census') %>%
  select(event_id,total_population_2019,cbg_2010) %>%
  collect()

evt_census %>%
  filter(is.na(total_population_2019)) %>%
  nrow()


evt_census %>%
  filter(is.na(total_population_2019)) %>%
  distinct(cbg_2010) %>%
  nrow()