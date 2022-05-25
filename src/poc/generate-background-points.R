if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  
}

message("start generating background points...")

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(amt)
    library(DBI)
    library(RSQLite)
  }))

message("read in centroids...")

evt <- fread(paste0(.outPF,"ssf-background-pts/event-daily-centroids.csv"))

ids <- unique(e$individual_id)
  
for(i in 1:length(ids)){
  id <- ids[i]
  
  e <- evt %>%
    filter(individual_id == id) 
  
  tr <- make_track(e, lon, lat, date, 
                   individual_id = individual_id,
                   ghm = ghm,
                   sg_norm = sg_norm)
  
  ssf <- tr %>% 
    track_resample(rate = hours(24), tolerance = hours(24)) %>%
    steps_by_burst() %>%
    random_steps(n_control = 15) 
  
  fwrite(ssf, paste0(.outPF,"ssf-background-pts/centroids-background/individual-",id,".csv"))
  
  message(i)
}

dbDisconnect(db)

message("done!")
