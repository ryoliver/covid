library(tidyverse)
library(patchwork)
library(sf)
library(data.table)
library(proj4)
library(spData)
rm(list = ls())

d <- fread("~/Desktop/covid-results/final_evt_for_map.csv")

d_sf <- st_as_sf(d, coords = c("lon", "lat"), crs = "EPSG:4326")
d_sf <- st_transform(d_sf, crs = "EPSG:6933")

us <- world %>%
  filter(name_long == "United States")
us <- st_transform(us, crs = "EPSG:6933")

d_sf <- d_sf[us,]

species_list <- unique(d_sf$taxon_canonical_name)


map_species <- function(species){
  d_sf_species <- d_sf %>%
    filter(taxon_canonical_name == species)
  
  print(1)
  
  hex <- st_make_grid(d_sf_species, n = c(100,100),
                      what = 'polygons',
                      square = FALSE,
                      flat_topped = TRUE) %>%
    st_sf() %>%
    rowid_to_column('hex_id')
  
  print(2)
  
  d_sf_hex <- st_join(d_sf_species, hex, join=st_within) %>%
    st_set_geometry(NULL) %>%
    count(name = "events", hex_id)
  
  print(3)
  
  hex <- hex %>%
    left_join(d_sf_hex, by = 'hex_id') %>%
    replace(is.na(.), 0) %>%
    filter(events > 0)
  
  print(4)
  
  us_map <- ggplot(us) +
    geom_sf(fill = "grey75", color = "grey75") 
  
  p <- us_map +
    geom_sf(data = hex, aes(fill = events), color = "transparent") +
    coord_sf(
      datum = NA, crs = st_crs("EPSG:5070")) +
    scale_fill_viridis_c(option = "magma",trans = "log10") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank()) +
    theme(legend.position = "bottom", 
          legend.key.width = unit(1,"cm"),
          legend.key.height = unit(0.5,"cm"),
          legend.title=element_text(size=7),
          legend.text = element_text(size=7),
          legend.margin=margin(0,0,0,0),
          #legend.box.margin=margin(-10,-10,-10,-10),
          axis.title = element_blank()) +
    guides(fill = guide_colorbar(title.position = "top")) +
    labs(fill = "Animal locations (n)") 
  
  
  ggsave(p, file = paste0("~/Desktop/species-maps/",species,".pdf"), width = 4, height = 5)
}

for(i in 14:length(species_list)){
  species <- species_list[i]
  print(i)
  map_species(species)
}
