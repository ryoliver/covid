if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))


db <- dbConnect(RSQLite::SQLite(), .dbPF)


reformatted_files_daily <- list.files(paste0(.datPF,"safegraph/counties-dates-2-10-22-reformatted/daily-data"), full.names = TRUE)

# combine all data
message("reading in safegraph data...")
daily_data <- data.table::rbindlist(lapply(reformatted_files_daily, data.table::fread),use.names = TRUE) %>%
  select(cbg,date,count) %>%
  rename(daily_count = count) %>%
  mutate(cbg = as.character(cbg),
         date = as.character(date))


evt <- tbl(db,'event_cbg') %>%
  select(cbg_2010) %>%
  collect() %>%
  distinct(cbg_2010)

fwrite(evt,paste0(.datPF, "safegraph/safegraph-cbg-list.csv"))

missing_cbgs_new <- setdiff(evt_sg$cbg_2010,daily_data$cbg)
cbgs_new <- data.frame("cbg" = missing_cbgs_new)

cbgs_old <- fread(paste0(.datPF, "safegraph/safegraph-missing-cbgs.csv"))
cbgs <- rbind(cbgs_old,cbgs_new) %>%
  distinct(cbg)

fwrite(cbgs,paste0(.datPF, "safegraph/safegraph-missing-cbgs-all.csv"))


