library(tidyverse)
library(patchwork)
library(sf)
library(data.table)
library(proj4)
library(spData)
rm(list = ls())

# species list + taxonomy
species_list <- fread("src/species_list.csv")
species_list$taxa[species_list$scientific_name== "Puma concolor"] <- "mammals"


d <- fread("~/Desktop/covid-results/final_evt_for_map.csv")

d_sf <- st_as_sf(d, coords = c("lon", "lat"), crs = "EPSG:4326")
d_sf <- st_transform(d_sf, crs = "EPSG:6933")

us <- world %>%
  filter(name_long == "United States")
us <- st_transform(us, crs = "EPSG:6933")

d_sf <- d_sf[us,]

d_species_joined <- d_sf %>%
  st_set_geometry(NULL) %>%
  left_join(., species_list, by = c("taxon_canonical_name" = "scientific_name"))

locations_summary <- d_species_joined %>%
  group_by(taxa) %>%
  summarise(n_locations = n())

species_summary <- species_list %>%
  group_by(taxa) %>%
  summarise(n_species = n())
  

p1 <- ggplot(data = locations_summary) +
  geom_bar(aes(y = taxa, x = n_locations), 
           stat = "identity",
           fill = "grey45") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y  = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  scale_y_discrete(expand = expansion(mult = c(0.5, 0.5))) +  
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)),
                     #labels = scales::comma,
                     breaks = seq(0, 8000000, by = 2000000),
                     labels = c("0", "2M", "4M", "6M", "8M")) +
  labs(x = "Locations (n)")

p2 <- ggplot(data = species_summary) +
  geom_bar(aes(y = taxa, x = n_species), 
           stat = "identity",
           fill = "grey45") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y  = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  scale_y_discrete(expand = expansion(mult = c(0.5, 0.5))) +  
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)),
                     labels = scales::comma) +
  labs(x = "Species (n)")


p_inset <- p1 + p2
ggsave(p_inset, file = "~/Desktop/figure1-inset.pdf", width = 1.8, height = 0.9)

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

bbox <- st_bbox(st_transform(hex, crs = "EPSG:5070"))
bbox_expand <- 0.1


us_map <- ggplot(us) +
  geom_sf(fill = "grey75", color = "grey75") 

p <- us_map +
  geom_sf(data = hex, aes(fill = events), color = "transparent") +
  coord_sf(
    xlim = c(bbox[1] - abs(bbox[1]*bbox_expand), bbox[3] + abs(bbox[3]*bbox_expand)), 
    ylim = c(bbox[2] - abs(bbox[2]*bbox_expand), bbox[4] + abs(bbox[4]*bbox_expand)), 
           datum = NA, crs = st_crs("EPSG:5070")) +
  scale_fill_viridis_c(option = "magma",trans = "log10") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  theme(legend.position = "bottom", 
        legend.key.width = unit(.7,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title=element_text(size=7),
        legend.text = element_text(size=7),
        legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-10,-10,-10,-10),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = "Animal locations (n)") 

ggsave(p, file = "~/Desktop/figure1.pdf", width = 4, height = 5)

