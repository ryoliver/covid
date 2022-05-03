if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  .datPF <- file.path(.wd,'data/example-data/')
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .datPF <- file.path(.wd,'data/example-data/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(amt)
  }))


setwd(.datPF)

files <- data.frame("name" = list.files(.datPF)) %>%
  # separate file name into start date, county id, and resolution
  separate(name,into = c("tb",NA,NA), sep = "-", remove = FALSE) 

files_evt <- files %>%
  filter(tb == "event")

files_ind <- files %>%
  filter(tb == "individual")

evttb <- data.table::rbindlist(lapply(files_evt$name, data.table::fread, colClasses = "character"))
indtb <- data.table::rbindlist(lapply(files_ind$name, data.table::fread, colClasses = "character")) %>%
  select(individual_id, taxon_canonical_name)

evt <- left_join(evttb, indtb, by = "individual_id") %>%
  mutate("date_time" = as_datetime(timestamp),
         "date" = as_date(timestamp),
         "year" = lubridate::year(date),
         "doy" = lubridate::yday(date),
         "lat" = as.numeric(lat),
         "lon" = as.numeric(lon)) %>%
  filter(year >= 2019) %>%
  filter(year <= 2020) %>%
  filter(doy < 170) %>%
  distinct(individual_id, date, .keep_all = TRUE)


ids <- unique(indtb$individual_id)
id <- ids[1]

e <- evt %>%
  filter(individual_id == id)

tr <- make_track(e, lon, lat, date_time, indvidual_id = individual_id, event_id = event_id)
ssf <- tr %>% 
  track_resample(rate = hours(24), tolerance = hours(24)) %>%
  steps_by_burst() %>%
  random_steps(n_control = 15)


