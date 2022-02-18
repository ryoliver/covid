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
    library(units)
  }))


load("~/Desktop/dbbmm_1003870095_2020.rdata")

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


cbg <- st_read("~/Documents/Yale/projects/covid/data/safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson")
r95 <- projectRaster(r95, crs = crs(cbg))

cbg$area <- st_area(cbg)/1000000
cbg <- drop_units(cbg)

cbg <- cbg %>%
  filter(!State %in% c("HI","AS", "GU", "MP", "PR", "VI"))

cbg_continental <- cbg %>%
  filter(!State %in% c("HI","AS", "GU", "MP", "PR", "VI", "AK"))

unique(cbg$State)

ggplot(cbg_continental, aes(x = area)) +
  geom_histogram(fill = "grey40", color = "transparent") +
  scale_x_continuous(trans = "log10", labels = comma) +
  labs(x = "area (km2)")

p1 <- ggplot() +
  geom_sf(data = cbg_continental, fill = "transparent", color = "grey75", lwd = 0.0001) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  coord_sf(datum = NA)

pdf("~/Desktop/test.pdf")
ggdraw() +
  draw_plot(p1)
dev.off()

max(cbg$area, na.rm = TRUE)

r95
cbg

unique(cbg$State)
extent(r95)  

plot(r95)


  