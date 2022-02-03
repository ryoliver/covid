#!/usr/bin/env Rscript --vanilla
#
# DESCRIPTION #
#
# This script summarizes animal movement data for the COVID-19 Animal Movement Project
#
# See project documentation for details about anticipated directory structure.
# This script implements the breezy philosophy: github.com/benscarlson/breezy
#
# Major tasks of this script:
#   * summarize the following attributes:
#     * number of records
#     * number of individuals
#     * number of species
#   * at the following levels of aggregation:
#     * state
#     * county
#     * census block group

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
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .outPF <- file.path(.wd,'analysis/')
}


suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

message("reading in event table...")
evt_df <- dbGetQuery(db,'SELECT * from event_cbg') 

message("reading in individual table...")
ind_df <- dbGetQuery(db,'SELECT individual_id,taxon_canonical_name from individual')

message("joining events and individual tables...")
evt_ind <- left_join(evt_df, ind_df, by = "individual_id")

message("computing summaries...")
state_summary <- evt_ind %>%
  group_by(State) %>%
  summarise("n_records" = n_distinct(event_id),
            "n_individuals" = n_distinct(individual_id),
            "n_species" = n_distinct(taxon_canonical_name)) %>%
  mutate("level" = rep("state", nrow(.)))

county_summary <- evt_ind %>%
  group_by(CountyFIPS) %>%
  summarise("n_records" = n_distinct(event_id),
            "n_individuals" = n_distinct(individual_id),
            "n_species" = n_distinct(taxon_canonical_name)) %>%
  mutate("level" = rep("county", nrow(.)))

cbg_summary <- evt_ind %>%
  group_by(CensusBlockGroup) %>%
  summarise("n_records" = n_distinct(event_id),
            "n_individuals" = n_distinct(individual_id),
            "n_species" = n_distinct(taxon_canonical_name)) %>%
  mutate("level" = rep("cbg", nrow(.)))

summary <- rbind(state_summary,
                 county_summary,
                 cbg_summary)

message("writing out summaries...")
fwrite(summary,paste0(.outPF,"data-summary.csv"))

message("done!")