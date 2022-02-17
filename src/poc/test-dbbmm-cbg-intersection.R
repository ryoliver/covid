if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
}


source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(sf)
    library(data.table)
  }))


load("~/Desktop/dbbmm_1003870094_2020.rdata")


r <- tmp_out$`dBBMM Object`
# normalize the probs within each week
rb <- UDStack(r)
# convert to quantiles
UDr <- getVolumeUD(rb)

j <- 1
# loop through the layers in each USstack
#for(j in 1:nlayers(UDr)){
  # return 1s within the 95% contour, do this for each week/layer
  ud95 <- UDr[[j]]<=.95
  # extract the probabilities for cells within the 95% contour
  r95 <- ud95*rb[[j]]
  # then you do something with the r95...
#} # j

cbg <- st_read("~/Documents/Yale/projects/covid/data/safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson")
r95 <- projectRaster(r95, crs = crs(cbg))

cbg$area <- st_area(cbg)

r95
cbg


extent(r95)  

plot(r95)


  