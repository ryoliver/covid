if(interactive()) {
  rm(list=ls())
  library(here)
  
  #.wd <- '~/Documents/Yale/projects/covid'
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'analysis/event-annotations/')
  .outPF <- file.path(.wd,'analysis/figures/ssf-examples/')
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .datPF <- file.path(.wd,'analysis/event-annotations/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(sf)
    library(data.table)
    library(lubridate)
    library(tidyverse)
    library(cowplot)
    library(DBI)
    library(RSQLite)
  }))

message("connect to db...")
db <- dbConnect(RSQLite::SQLite(), .dbPF)

indtb <- tbl(db,'individual')

evt <- fread(paste0(.datPF,"background_sg_ghm.csv")) %>%
  mutate("sg_norm" = safegraph_daily_count/cbg_area_m2,
         "year" = lubridate::year(t2_),
         "doy" = lubridate::yday(t2_)) %>%
  st_as_sf(coords = c("x2_", "y2_"),
           crs = "+proj=longlat +datum=WGS84",
           remove = FALSE)


ids <- unique(evt$individual_id)

for(i in 1:length(ids)){
id <- ids[i]

ind_info <- indtb %>% 
  filter(individual_id == id) %>% 
  select(taxon_canonical_name) %>% 
  collect()

e <- evt %>%
  filter(individual_id == id)

pmap <- ggplot() +
  geom_sf(data = subset(e, case_ == FALSE), color = "grey", alpha = 0.5) +
  geom_sf(data = subset(e, case_ == TRUE), aes(color = as.factor(year)), alpha = 0.5) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) + 
  
  theme(legend.position = "none", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  labs(title = ind_info$taxon_canonical_name)

plat <- ggplot(data = subset(e, case_ == TRUE)) +
  geom_point(aes(x = doy, y = y2_, group = as.factor(year), color = as.factor(year))) +
  geom_line(aes(x = doy, y = y2_, group = as.factor(year), color = as.factor(year))) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = c(0.1,0.9)) +
  labs(x = "day of year", y = "latitude") 

plon <- ggplot(data = subset(e, case_ == TRUE)) +
  geom_point(aes(x = doy, y = x2_, group = as.factor(year), color = as.factor(year))) +
  geom_line(aes(x = doy, y = x2_, group = as.factor(year), color = as.factor(year))) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "day of year", y = "longitude") 


pghm <- ggplot(data = e) +
  #geom_histogram(aes(x = ghm, y = ..density..,color = case_, fill = case_), alpha = 0.5) +
  geom_density(aes(x = ghm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = c(0.1,0.9)) +
  labs(x = "modification", y = "density", title = "all") 

pghm19 <- ggplot(data = subset(e, year == 2019)) +
  geom_density(aes(x = ghm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "modification", y = "density", title = "2019") 

pghm20 <- ggplot(data = subset(e, year == 2020)) +
  geom_density(aes(x = ghm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "modification", y = "density", title = "2020") 

psg <- ggplot(data = e) +
  #geom_histogram(aes(x = sg_norm, y = ..density..,color = case_, fill = case_), alpha = 0.5) +
  geom_density(aes(x = sg_norm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(trans = "log10", expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "mobility", y = "density", title = "all") 

psg19 <- ggplot(data = subset(e, year == 2019)) +
  geom_density(aes(x = sg_norm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(trans = "log10", expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "mobility", y = "density", title = "2019") 

psg20 <- ggplot(data = subset(e, year == 2020)) +
  geom_density(aes(x = sg_norm,color = case_, fill = case_), alpha = 0.5) +
  scale_fill_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_color_manual(values = c("#5B2A86","#9AC6C5")) +
  scale_x_continuous(trans = "log10", expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "mobility", y = "density", title = "2020") 

h <- 1/3
pdf(paste0(.outPF,"individual-",id,".pdf"), width = 12, height = 12)
print(
ggdraw() +
  draw_plot(pmap, x = 0, y = 2*h, width = 1/3, height = h) +
  draw_plot(plat, x = 1/3, y = 2*h, width = 1/3, height = h) +
  draw_plot(plon, x = 2/3, y = 2*h, width = 1/3, height = h) +
  draw_plot(pghm, x = 0, y = h, width = 1/2, height = h) +
  draw_plot(psg, x = 1/2, y = h, width = 1/2, height = h) +
  draw_plot(pghm19, x = 0, y = 0, width = 1/4, height = h) +
  draw_plot(pghm20, x = 1/4, y = 0, width = 1/4, height = h) +
  draw_plot(psg19, x = 1/2, y = 0, width = 1/4, height = h) +
  draw_plot(psg20, x = 3/4, y = 0, width = 1/4, height = h))
dev.off()
}
dbDisconnect(db)



