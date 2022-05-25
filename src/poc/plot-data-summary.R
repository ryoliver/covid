#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# DESCRIPTION #
#
# This script reformats human mobility data for the COVID-19 Animal Movement Project
# See project documentation for details about anticipated directory structure.
#
# Major tasks fof this script:
#   * read in safegraph data
#   * reformat from wide to long format
#   * write out data by counties
#   * read in county files and combine into single file
#   * repeat for daily and hourly data

#
# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

#'
#Template
#Usage:
#script_template <taxa> <dat> <out> 
#script_template (-h | --help)
#Parameters:
#  dat: path to input csv file. 
#  out: path to output directory.
#Options:
#-h --help     Show this screen.
#-v --version     Show version.
#' -> doc

#---- Input Parameters ----#
if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  #ag <- docopt(doc, version = '0.1\n')
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  #.test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  #source(rd('src/funs/input_parse.r'))
  
  .datPF <- file.path(.wd,'data/safegraph/counties-dates-2-10-22/')
  .outPF <- file.path(.wd,'analysis/safegraph/counties-dates-2-10-22-reformatted/')
}


source(rd('src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

suppressWarnings(
  suppressPackageStartupMessages({
    library(sf)
    library(cowplot)
  }))

evt_summary <- fread(paste0(.outPF, "event-annotations/event-summary-1440grid.csv"))


grid <- st_read(paste0(.datPF,"1440x456global_v2_20200527/1440x456global_20200527.shp")) %>%
  left_join(.,evt_summary, by = "ID_1440") %>%
  filter(!is.na(n_events))

gadm <- st_read(paste0(.datPF,"gadm36_mol_simple/gadm36_0_noCaspian_cea_simple_20200121.shp"))

us <- gadm %>%
  filter(NAME_0 == "United States")

test <- st_intersection(grid, us)

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



h <- 1/3
pdf(paste0(.outPF,"data-summary.pdf"), width = 8, height = 12)
ggdraw() +
  draw_plot(p1, x = 0, y = 2*h, width = 1, height = h) +
  draw_plot(p2, x = 0, y = h, width = 1, height = h) +
  draw_plot(p3, x = 0, y = 0, width = 1, height = h)
dev.off()

pdf("~/Desktop/test.pdf", width = 20, height = 10)
ggdraw() +
  draw_plot(p1) 
dev.off()
