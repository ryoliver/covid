if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .dbbmmPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/out/dbbmms'
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/figures/dbbmm-test/')
  
} else {
  rm(list=ls())
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .dbbmmPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/out/dbbmms'
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/figures/dbbmm-test/')
}


source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(sf)
    library(data.table)
    library(units)
    library(janitor)
    library(tidyverse)
    library(move)
    library(DBI)
    library(RSQLite)
  }))


message("pulling file list...")
files <- data.frame("file_path" = list.files(.dbbmmPF, pattern = "*.rdata",recursive = TRUE,full.names = TRUE),
                    "name" = list.files(.dbbmmPF, pattern = "*.rdata",recursive = TRUE)) %>%
  separate(name,into = c(NA,"individual_id",NA), sep = "_", remove = FALSE) %>%
  sample_n(.,100)

message("connecting to db...")
db <- dbConnect(RSQLite::SQLite(), .dbPF)

message("pulling individual table...")
indtb <- tbl(db, "individual") %>%  
  collect() %>%
  filter(individual_id %in% files$individual_id)

### read in first dBBMM
message("processing first dbbmm...")
load(files[1,]$file_path)

r <- tmp_out$`dBBMM Object`
# normalize the probs within each week
rb <- UDStack(r)
# convert to quantiles
UDr <- getVolumeUD(rb)

j <- 1

# return 1s within the 95% contour, do this for each week/layer
ud95 <- UDr[[j]]<=.95
# extract the probabilities for cells within the 95% contour
r95 <- ud95*rb[[j]]

### read in census block group geometries
message("reading cbg geometries...")
cbg <- st_read(paste0(.datPF, "safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson"))

message("transforming cbg geometries...")
cbg <- st_transform(cbg, crs(r95))


visualize_dbbmm <- function(i){
  message("processing dbbmm...")
  load(files[i,]$file_path)
  
  r <- tmp_out$`dBBMM Object`
  # normalize the probs within each week
  rb <- UDStack(r)
  # convert to quantiles
  UDr <- getVolumeUD(rb)
  
  j <- 1
  # return 1s within the 95% contour, do this for each week/layer
  ud95 <- UDr[[j]]<=.95
  # extract the probabilities for cells within the 95% contour
  r95 <- ud95*rb[[j]]
  
  r95 <- reclassify(r95, cbind(0,NA))
  r95_trim <- raster::trim(r95)
  
  r95_vect <- rasterToPolygons(r95_trim)
  
  r95_vect <- st_as_sf(r95_vect) %>%
    clean_names() %>%
    mutate(x = layer) %>%
    filter(x > 0)
  
  message("transforming cbg geometries...")
  cbgs <- st_transform(cbg, crs(r95))
  
  message("cropping cbg geometries...")
  cbgs <- st_crop(cbgs, st_bbox(r95_vect))
  
  cbgs$area <- st_area(cbgs)/1000000
  cbgs <- drop_units(cbgs)
  
  r95_vect$area <- st_area(r95_vect)/1000000
  r95_vect <- drop_units(r95_vect)
  
  
  
  id <- files[i,]$individual_id
  
  species_name <- indtb %>%
    as.data.frame() %>%
    filter(individual_id == id) %>%
    dplyr::select(taxon_canonical_name)
  
  message("plotting...")
  p <- ggplot() +
    geom_sf(data = cbgs, fill = "transparent", colour = "grey75") +
    geom_sf(data = r95_vect, fill = "transparent", colour = "red") +
    theme(legend.position = "bottom", 
          legend.key.width = unit(1.5,"cm"),
          legend.title=element_text(size=8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank()) +
    labs(title = paste0(species_name$taxon_canonical_name,", ",
                        unique(cbgs$State),", n cbgs = ", nrow(cbgs))) +
    coord_sf(datum = NA)
  
  p2 <- ggplot(cbgs, aes(x = area)) +
    geom_histogram(fill = "grey40", color = "transparent") +
    scale_x_continuous(labels = comma) +
    labs(x = "area (km2)", title = "cbg")
  
  p3 <- ggplot(r95_vect, aes(x = area)) +
    geom_histogram(fill = "grey40", color = "transparent") +
    scale_x_continuous(labels = comma) +
    labs(x = "area (km2)", title = "dBBMM")
  
  
  pdf(paste0(.outPF,id,".pdf"))
  print(
    ggdraw() +
      draw_plot(p, x = 0, y = 0.3, height = 0.7, width = 1) +
      draw_plot(p2, x = 0, y = 0, height = 0.3, width = 0.5) +
      draw_plot(p3, x = 0.5, y = 0, height = 0.3, width = 0.5))
  dev.off()
}


for (i in 1:nrow(files)){
  visualize_dbbmm(i)
  message(i)
}
message("done!")