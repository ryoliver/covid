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
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)



suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(cowplot)
    library(lubridate)
    library(ggmap)
    library(sf)
    library(raster)
    library(rgdal)
    library(terra)
  }))



d <- fread(paste0(.outPF,"fig1-example-species.csv")) 

d <- d %>%
  mutate(year = lubridate::year(timestamp),
         sg_norm = safegraph_daily_count/cbg_area_m2) %>%
  select(taxon_canonical_name, individual_id, timestamp, lon, lat, year, sg_norm) 

d_summary <- d %>%
  group_by(individual_id) %>%
  summarize(n_years = n_distinct(year)) %>%
  filter(n_years == 2)
  

d <- d %>%
  filter(individual_id %in% d_summary$individual_id) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, remove = FALSE)


species <- d %>%
  filter(taxon_canonical_name == "Puma concolor")

ids <- unique(species$individual_id)


i <- 9
ind <- species %>%
  filter(individual_id == ids[i]) %>%
  mutate(doy = lubridate::yday(timestamp),
         md = format(as.Date(timestamp),"%m-%d")) %>%
  filter(doy < 165) %>%
  distinct(year,doy, .keep_all = TRUE)


ghm <- raster(paste0(.datPF,"gHM/gHM.tif"))

ind <- st_transform(ind, crs(ghm))
ind_buffer <- st_buffer(ind, 1000)

r <- terra::crop(ghm, ind_buffer)
r <- rasterToPolygons(r)
r <- st_as_sf(r)
r <- st_transform(r, crs = 4326)

bbox <- unname(st_bbox(r))
l <- bbox[1]
b <- bbox[2]
ri <- bbox[3]
t <- bbox[4]


basemap <- get_stamenmap(bbox = c(left = l - abs(l)*0.001, bottom = b -abs(b)*0.001,
                                  right = ri+ abs(ri)*0.001, top = t + abs(t)*0.001), 
                         maptype = "toner-lite", source = "stamen", zoom = 12)



p1 <- ggmap(basemap) +
  geom_sf(data = r, aes (fill = gHM), 
          color = "transparent", alpha = 0.5, 
          inherit.aes = FALSE) +
  geom_sf(data = ind, aes(color = as.factor(year)), 
          alpha = 0.5,
          inherit.aes = FALSE) +
  scale_fill_gradient(low = "#FEF7EB",
                      high = "#E9980C",
                      name = "Modification") +
  scale_color_manual(values = c(color19,color20),
                     guide = "none") +
  coord_sf(datum = NA) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))


p2 <- ggplot(data = ind, 
       aes(y = sg_norm, x = doy, 
           color = as.factor(year), group = as.factor(year))) +
  scale_color_manual(values = c(color19,color20)) +
  
  geom_point(lwd = 1) +
  theme_cowplot() +
  theme(legend.position= c(0.8,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                     trans = "log10") +
  labs(x  = "Day of year", y = "Mobility") 



pdf(paste0(.wd,"/analysis/figures/fig1-individual-map.pdf"), width = 8, height = 4)
ggdraw() +
  draw_plot(p1)
dev.off()

pdf(paste0(.wd,"/analysis/figures/fig1-individual-timeseries.pdf"), width = 4, height = 2.5)
ggdraw() +
  draw_plot(p2)
dev.off()
