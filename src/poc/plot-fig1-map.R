if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/ucsb/repos/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')

} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(sf)
    library(cowplot)
  }))


evt_summary <- fread(paste0(.outPF,"event-summary-1440grid.csv"))

grid <- st_read(paste0(.datPF,"1440x456global_v2_20200527/1440x456global_20200527.shp")) 
#grid <- st_transform(grid, crs = "EPSG:5070")

grid <- left_join(grid, evt_summary, by = c("ID_1440"))

grid <- grid %>%
  filter(!is.na(n_events))

gadm <- st_read(paste0(.datPF,"gadm36_mol_simple/gadm36_0_noCaspian_cea_simple_20200121.shp"))
#gadm <- st_transform(gadm, crs = "EPSG:5070")


us <- gadm %>%
  filter(NAME_0 == "United States")

test <- st_intersection(grid, us)

us <- st_transform(us, crs = "EPSG:5070")
test <- st_transform(test, crs = "EPSG:5070")



bbox <- st_bbox(test)
bbox_expand <- 0.001

p1 <- ggplot() +
  geom_sf(data = us, fill = "grey75", colour = "transparent") +
  geom_sf(data = test, aes(fill = n_events), colour = "transparent") +
  scale_fill_viridis_c(option = "magma",trans = "log10") +
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  coord_sf(xlim = c(bbox[1] - abs(bbox[1]*bbox_expand), bbox[3] + abs(bbox[3]*bbox_expand)), 
           ylim = c(bbox[2] - abs(bbox[2]*bbox_expand), bbox[4] + abs(bbox[4]*bbox_expand)), 
           expand = TRUE, datum = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Animal locations (n)") 

p2 <- ggplot() +
  geom_sf(data = us, fill = "grey75", colour = "transparent") +
  geom_sf(data = test, aes(fill = n_individuals), colour = "transparent") +
  scale_fill_viridis_c(option = "magma",trans = "log10") +
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  coord_sf(xlim = c(bbox[1] - abs(bbox[1]*bbox_expand), bbox[3] + abs(bbox[3]*bbox_expand)), 
           ylim = c(bbox[2] - abs(bbox[2]*bbox_expand), bbox[4] + abs(bbox[4]*bbox_expand)), 
           expand = TRUE, datum = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Animal individuals (n)") 

p3 <- ggplot() +
  geom_sf(data = us, fill = "grey75", colour = "transparent") +
  geom_sf(data = test, aes(fill = n_species), colour = "transparent") +
  scale_fill_viridis_c(option = "magma",trans = "sqrt") +
  
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  coord_sf(xlim = c(bbox[1] - abs(bbox[1]*bbox_expand), bbox[3] + abs(bbox[3]*bbox_expand)), 
           ylim = c(bbox[2] - abs(bbox[2]*bbox_expand), bbox[4] + abs(bbox[4]*bbox_expand)), 
           expand = TRUE, datum = NA) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Species (n)") 



pdf(paste0(.wd,"/analysis/fig1-map.pdf"), width = 8, height = 10)
ggdraw() +
  draw_plot(p1) 
dev.off()

