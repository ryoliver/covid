library(tidyverse)
library(patchwork)
library(sf)
library(data.table)
library(proj4)
library(spData)
rm(list = ls())

d <- fread("~/Desktop/covid-results/final_evt_for_map.csv")
d <- d[1:10000,]

d_sf <- st_as_sf(d, coords = c("lon", "lat"), crs = "EPSG:4326")
d_sf <- st_transform(d_sf, crs = "EPSG:6933")

us <- world %>%
  filter(name_long == "United States")
us <- st_transform(us, crs = "EPSG:6933")

d_sf <- d_sf[us,]

hex <- st_make_grid(d_sf, n = c(100,100),
                    what = 'polygons',
                    square = FALSE,
                    flat_topped = TRUE) %>%
  st_sf() %>%
  rowid_to_column('hex_id')

d_sf_hex <- st_join(d_sf, hex, join=st_within) %>%
  st_set_geometry(NULL) %>%
  count(name = "events", hex_id)

hex <- hex %>%
  left_join(d_sf_hex, by = 'hex_id') %>%
  replace(is.na(.), 0) %>%
  filter(events > 0)

plot(hex['events'])

us_map <- ggplot(us) +
  geom_sf() 

us_map +
  geom_sf(data = hex, aes(fill = events)) +
  coord_sf(datum = NA, crs = st_crs("EPSG:5070")) +
  scale_fill_viridis_c(option = "magma",trans = "log10") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.5,"cm"),
        legend.title=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Animal locations (n)") 

