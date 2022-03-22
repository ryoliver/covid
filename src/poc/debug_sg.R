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
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod_20220303.db'
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


#evt_sg <- tbl(db,'event_sg') %>%
#  select(event_id,cbg_2010,date_hour,daily_count) %>%
#  collect()

evt_sg <- tbl(db,'event_sg') %>%
  select(event_id,cbg_2010,timestamp,daily_count) %>%
  collect() %>%
  separate(timestamp, c("date",NA), sep = " ", remove = FALSE)


missing_cbgs <- setdiff(evt_sg$cbg_2010,daily_data$cbg)

cbgs <- data.frame("cbg" = missing_cbgs)
fwrite(cbgs,paste0(.datPF, "safegraph/safegraph-missing-cbgs.csv"))


evt_sg %>%
  filter(is.na(daily_count)) %>%
  filter(!cbg_2010 %in% missing_cbgs) %>%
  nrow()


missing_dates <- evt_sg %>%
  filter(is.na(daily_count)) %>%
  filter(!cbg_2010 %in% missing_cbgs) %>%
  distinct(cbg_2010, date)

fwrite(still_missing,paste0(.datPF, "safegraph/safegraph-missing-data.csv"))
