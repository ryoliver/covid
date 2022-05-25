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

message("start generating centroids...")

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(sf)
    library(DBI)
    library(RSQLite)
  }))

grid <- st_read(paste0(.datPF,"1440x456global_v2_20200527/1440x456global_20200527.shp"))

message("connect to db...")
db <- dbConnect(RSQLite::SQLite(), .dbPF)

message("read in annotation files...")
evt_sg <- fread(paste0(.outPF,"event-annotations/event_sg.csv")) %>%
  mutate(sg_norm = safegraph_daily_count/cbg_area_m2) %>%
  select(event_id, sg_norm)

evt_ghm <- fread(paste0(.outPF,"event-annotations/event_ghm.csv"))

message("create centroids...")
evt <- dbGetQuery(db,'SELECT event_id,individual_id,lon,lat,timestamp from event_clean') %>%
  collect() %>%
  mutate("date" = as_date(timestamp),
         "year" = lubridate::year(date),
         "doy" = lubridate::yday(date)) %>%
  filter(year >= 2019) %>%
  filter(year <= 2020) %>%
  filter(doy < 170) %>%
  st_as_sf(coords = c("lon", "lat"), crs="+proj=longlat +datum=WGS84") %>%
  left_join(., evt_sg, by = "event_id") %>%
  left_join(., evt_ghm, by = "event_id") %>%
  st_transform(., crs = st_crs(grid)) %>%
  group_by(individual_id, date) %>% 
  summarize(ghm = mean(ghm, na.rm = TRUE),
            sg_norm = mean(sg_norm, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_centroid() %>%
  st_transform(., crs="+proj=longlat +datum=WGS84") 

message("extract coordinates...")
coords <- st_coordinates(evt)

evt <- cbind(evt,coords)

evt <- evt %>%
  st_drop_geometry() %>%
  rename(lon = X,
         lat = Y)

message("write out centroid shp file...")
st_write(e, paste0(.outPF,"ssf-background-pts/event-daily-centroids.csv"))
  
dbDisconnect(db)
  
message("done!")

