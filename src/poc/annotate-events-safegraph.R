#!/usr/bin/env Rscript --vanilla
#
# DESCRIPTION #
#
# This script links human mobility data with animal events for the COVID-19 Animal Movement Project
# Human mobility sourced from SafeGraph, provided by Song Gao
#
# See project documentation for details about anticipated directory structure.
# This script implements the breezy philosophy: github.com/benscarlson/breezy
#
# Major tasks fof this script:
#   * Annotate event dataset with:
#     * daily and hourly device counts
#   *write out table with decive counts

# ==== Breezy setup ====

#'
#Template
#Usage:
#script_template <taxa> <dat> <out> 
#script_template (-h | --help)
#Parameters:
#  dat: path to input csv file. 
#  out: path to output directory.
#Options:
#-h --help     Show this screen.
#-v --version     Show version.
#' -> doc

#---- Input Parameters ----#
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
  .datPF <- file.path(.wd,'analysis/')
}


suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

message("reading in event table...")
evt_df <- dbGetQuery(db,'SELECT * from event_clean') %>%
  separate(timestamp, c("date",NA), sep = " ", remove = FALSE) %>%
  mutate("date_hour" = str_trunc(timestamp,13,"right","")) %>%
  select(date_hour)

message("reading in safegraph data...")
daily_data <- fread(paste0(.datPF,"safegraph/counties-dates-2-1-22-reformatted/all_counties_cbg_day_SUM.csv")) %>%
  select(cbg,date,count) %>%
  rename(daily_count = count)

hourly_data <- fread(paste0(.datPF,"safegraph/counties-dates-2-1-22-reformatted/all_counties_cbg_hour_SUM.csv")) %>%
  select(cbg,date,count)  %>%
  mutate("date_hour" = str_trunc(timestamp,13,"right","")) %>%
  rename(hourly_count = count)

message("joining events with safegraph data...")
evt_sg <- left_join(evt_df,daily_data, by = c("CensusBlockGroup" = "cbg", "date" = "date")) %>%
  left_join(.,hourly_data, by = c("CensusBlockGroup" = "cbg", "date_hour" = "date_hour")) %>%
  select(-date,-date_hour)

message("writing out new event table...")
dbWriteTable(conn = db, name = "event_sg", value = evt_sg, append = FALSE, overwrite = T)

message("done!")


