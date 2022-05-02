if(interactive()) {
  rm(list=ls())
  library(here)
  
  #.wd <- '~/Documents/Yale/projects/covid'
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
    #library(sf)
    #library(data.table)
    #library(lubridate)
    #library(leaflet)
    #library(mapview)
    #library(magick)
    #library(amt)
  }))
library(amt)
message("worked!")

if(1==2){
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



evt_sf <- st_as_sf(evt, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84",
                   remove = FALSE)


ids <- unique(indtb$individual_id)



id <- ids[1]

e <- evt %>%
  filter(individual_id == id)

tr <- make_track(e, lon, lat, date_time, indvidual_id = individual_id, event_id = event_id)
ssf <- tr %>% 
  track_resample(rate = hours(24), tolerance = hours(24)) %>%
  steps_by_burst() %>%
  random_steps(n_control = 15)




plat <- ggplot(data = subset(evt, individual_id == id)) +
  geom_point(aes(x = doy, y = lat, group = as.factor(year), color = as.factor(year))) +
  geom_line(aes(x = doy, y = lat, group = as.factor(year), color = as.factor(year))) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = "day of year", y = "latitude") 

plon <- ggplot(data = subset(evt, individual_id == id)) +
  geom_point(aes(x = doy, y = lon, group = as.factor(year), color = as.factor(year))) +
  geom_line(aes(x = doy, y = lon, group = as.factor(year), color = as.factor(year))) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = ) +
  labs(x = "day of year", y = "longitude") 

pal <- colorFactor(c("#FF6542","#86BBD8"), domain = c(2019, 2020))

m <- leaflet() %>%
  addProviderTiles(
    "OpenStreetMap",
    group = "OpenStreetMap") %>%
  addCircleMarkers(data = subset(evt_sf, individual_id == id),
                   color = ~pal(year),
                   radius = 2)

mapshot(m, file = "~/Desktop/m.jpg")

pmap <- image_read("~/Desktop/m.jpg")

p <- ggdraw() +
  draw_image(pmap, y = 0, x = 0, width = 1/3, height = 1) +
  draw_plot(plat, y = 0, x = 1/3, width = 1/3, height = 1) +
  draw_plot(plon, y = 0, x = 2/3, width = 1/3, height = 1)



pdf("~/Desktop/test.pdf", width = 12, height = 4)
ggdraw() +
  draw_plot(p)
dev.off()

p <- ggplot() +
  geom_sf(data = subset(e, individual_id == id), aes(color = as.factor(year)), alpha = 0.5) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) + 
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) 
}