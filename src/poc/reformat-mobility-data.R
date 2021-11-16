#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Template
Usage:
script_template <taxa> <dat> <out> 
script_template (-h | --help)
Parameters:
  dat: path to input csv file. 
  out: path to output directory.
Options:
-h --help     Show this screen.
-v --version     Show version.
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid/'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  .datPF <- makePath(ag$dat)
  .outPF <- makePath(ag$out)
}

#---- Initialize Environment ----#

source(rd('src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

# get list of files
files <- data.frame("name" = list.files(paste0(.wd,"data/safegraph/counties-5-18-21/"))) %>%
  # separate file name into start date, county id, and resolution
  separate(name,into = c("start_date","county_id",NA,"resolution",NA), sep = "_", remove = FALSE) %>%
  mutate("start_date" = lubridate::as_date(start_date))

# filter to daily data
files_daily <- files %>%
  filter(resolution == "day")

# get list of counties
counties <- unique(files$county_id)

# function for processing daily data
process_daily_data <- function(file_name){
  # read in file
  d <- fread(paste0(.wd,"data/safegraph/counties-5-18-21/",file[,]$name)) %>%
    # rename census block group column
    rename("cbg" = "V1") %>%
    # convert from "wide" to "long" format
    # columns are days of the week, values are device counts
    pivot_longer(!cbg, names_to = "day_of_week", values_to = "count", names_prefix = "V") %>%
    mutate("day_of_week" = as.numeric(day_of_week) - 2,
           "county_id" = rep(file[,]$county_id),
           "start_date" = rep(file[,]$start_date),
           "date" = start_date + day_of_week) %>% # find true date by adding from start date
    select(county_id,cbg,date,count)
  return(d)
}

# create reformatted files for each county
for (j in 1:length(counties)){
  
  # subset to files from single county
  county_files <- files_daily %>%
    filter(county_id == counties[j])
  
  d <- c()
  for (i in 1:nrow(county_files)){
    # start with first file
    file <- county_files[i,]
    # reformat data
    dd <- process_daily_data(file$name)
    d <- rbind(d,dd)
  }
  # write out file with all data from a single county
  fwrite(d, paste0(.outPF,"safegraph/counties-5-18-21-reformatted/daily-counties/",counties[j],"_cbg_day_SUM.csv"))
}

# combine county files into a single file
reformatted_files <- dir(paste0(.outPF,"safegraph/counties-5-18-21-reformatted/daily-counties/"), full.names = TRUE)

# combine all data
all_data <- reformatted_files %>%
  map_dfr(fread)

# write out as single file
fwrite(all_data, paste0(paste0(.outPF,"safegraph/counties-5-18-21-reformatted/all_counties_cbg_day_SUM.csv")))
