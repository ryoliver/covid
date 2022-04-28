if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid'
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
    library(sf)
    library(data.table)
    library(lubridate)
    library(leaflet)
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
         "year" = lubridate::year(date)) %>%
  filter(year >= 2019) %>%
  filter(year <= 2020)


evt_sf <- st_as_sf(evt, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84",
                   remove = FALSE)

e <- evt_sf %>%
  distinct(individual_id, date, .keep_all = TRUE)

ids <- unique(indtb$individual_id)


pal <- colorFactor(c("navy", "red"), domain = c(2019, 2020))

id <- ids[3]

leaflet() %>%
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap") %>%
  addCircleMarkers(data = subset(e, individual_id == id),
                   color = ~pal(year),
                   radius = 1)
  

p <- ggplot() +
  geom_sf(data = subset(e, individual_id == id), aes(color = date)) +
  scale_color_viridis_c(option = "magma") + 
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) 

pdf("~/Desktop/test.pdf")
ggdraw() +
  draw_plot(p)
dev.off()
