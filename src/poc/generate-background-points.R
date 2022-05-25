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

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(amt)
    library(DBI)
    library(RSQLite)
    library(sf)
  }))

message("connect to db...")
db <- dbConnect(RSQLite::SQLite(), .dbPF)

indtb <- tbl(db,'individual')
evttb <- tbl(db,'event_clean')

message("select test individuals...")
test <- evttb %>%
  group_by(individual_id,) %>%
  summarize(num=n()) %>%
  arrange(desc(num)) %>%
  inner_join(indtb, by = 'individual_id') %>%
  select(taxon_canonical_name, individual_id, num) %>%
  collect() %>%
  group_by(taxon_canonical_name) %>%
  slice(1) %>%
  ungroup() %>%
  sample_n(20)
  
for(i in 1:nrow(test)){
  id <- test[i,]$individual_id
  
  e <- evttb %>%
    filter(individual_id == id) %>%
    collect() %>%
    mutate("date_time" = as_datetime(timestamp),
           "date" = as_date(timestamp),
           "year" = lubridate::year(date),
           "doy" = lubridate::yday(date),
           "lat" = as.numeric(lat),
           "lon" = as.numeric(lon)) %>%
    filter(year >= 2019) %>%
    filter(year <= 2020) %>%
    filter(doy < 170) 
  
  e1 <- e %>%
    distinct(date, .keep_all = TRUE)
  
  tr1 <- make_track(e1, lon, lat, date_time, event_id = event_id)
  
  ssf1 <- tr1 %>% 
    track_resample(rate = hours(24), tolerance = hours(24)) %>%
    steps_by_burst() %>%
    random_steps(n_control = 15) %>%
    mutate("individual_id" = rep(id, nrow(.)))
  
  fwrite(ssf1, paste0(.outPF,'ssf-background-pts/individual-files/traditional-approach/individual-',id,".csv"))
  
  
  
  e2 <- e %>%
    st_as_sf(., coords = c("lon", "lat")) %>%
    filter(date == "2020-01-01") %>%
    dplyr::group_by(date) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_centroid()
  
  
  tr2 <- make_track(e2, lon, lat, date)
  
  ssf2 <- tr2 %>% 
    track_resample(rate = hours(24), tolerance = hours(24)) %>%
    steps_by_burst() %>%
    random_steps(n_control = 15) %>%
    mutate("individual_id" = rep(id, nrow(.)))
  
  fwrite(ssf2, paste0(.outPF,'ssf-background-pts/individual-files/averaging-approach/individual-',id,".csv"))
  
  message(i)
}

message("done!")
#ids <- c(1967914051,1967914129,160484522,160484522,578909800)

dbDisconnect(db)
