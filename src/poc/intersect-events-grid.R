#!/usr/bin/env Rscript --vanilla
#
# DESCRIPTION #
#
# This script determines the administrative units for records for the COVID-19 Animal Movement Project
# See project documentation for details about anticipated directory structure.
#
# Major tasks fof this script:
#   * Annotate event dataset with:
#     * CensusBlockGroup (12 digit FIPS code)
#     * BlockGroup (1 digit FIPS code)
#     * TractCode (6 digit FIPS)
#     * CountyFIPS (3 digit FIPS)
#     * StateFIPS (2 digit FIPS)
#     * County (character string)
#     * State (2 character code)
#   *write out csv with administrative info
#
# This script implements the breezy philosophy: github.com/benscarlson/breezy


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
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/event-annotations/')

} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/event-annotations/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
    library(sf)
  }))


#---- Initialize database ----#

invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

# read in census block group geometries
message("reading in census block group geometries...")
grid <- st_read(paste0(.datPF,"1440x456global_v2_20200527/1440x456global_20200527.shp"))

# read in event table
message("reading in event table...")

evt_sf <- dbGetQuery(db,'SELECT event_id,individual_id,lat,lon from event_clean') %>%
  st_as_sf(coords = c("lon", "lat"), crs="+proj=longlat +datum=WGS84") %>%
  st_transform(., st_crs(grid))

ind_tb <- dbGetQuery(db, 'SELECT individual_id,taxon_canonical_name from individual')


# intersect event table with census block group geometries
message("intersecting events with census block groups...")
evt_grid <- st_intersection(evt_sf,grid) 

evt_summary <- evt_grid %>%
  st_drop_geometry() %>%
  left_join(., ind_tb, by = "individual_id") %>%
  group_by(ID_1440) %>%
  summarize("n_events" = n(),
          "n_individuals" = n_distinct(individual_id),
          "n_species" = n_distinct(taxon_canonical_name))


# write out new table with annotations
message("writing out csv...")
fwrite(evt_summary, paste0(.outPF,"event-summary-1440grid.csv"))

dbDisconnect(db)

message("done!")

