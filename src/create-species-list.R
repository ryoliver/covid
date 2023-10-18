library(data.table)
library(tidyverse)

rm(list = ls())


# species results
area_ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-10-17.csv") %>%
  select(species)

area_sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-10-17.csv") %>%
  mutate(response = rep("area_sg", nrow(.))) %>%
  select(species)

niche_ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-10-17.csv") %>%
  mutate(response = rep("niche_ghm", nrow(.))) %>%
  select(species)

niche_sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-10-17.csv") %>%
  mutate(response = rep("niche_sg", nrow(.))) %>%
  select(species)

species_list <- data.frame(scientific_name = unique(c(area_ghm$species,
                                     area_sg$species,
                                     niche_ghm$species,
                                     niche_sg$species))) %>%
  mutate(common_name = rep(NA, n()),
         taxa = rep(NA, n())) %>%
  mutate(common_name = case_when(scientific_name == "Alces alces" ~ "Moose",
                                 scientific_name == "Anas acuta" ~ "Northern pintail",
                                 scientific_name == "Anas americana" ~ "American wigeon",
                                 scientific_name == "Anas clypeata" ~ "Northern shoveler",
                                 scientific_name == "Anas crecca" ~ "Common teal",
                                 scientific_name == "Anas cyanoptera" ~ "Cinnamon teal",
                                 scientific_name == "Anas discors" ~ "Blue-winged teal",
                                 scientific_name == "Anas platyrhynchos" ~ "Mallard",
                                 scientific_name == "Anas strepera" ~ "Gadwall",
                                 scientific_name == "Anser albifrons" ~ "GWF goose",
                                 scientific_name == "Anser caerulescens" ~ "Snow goose",
                                 scientific_name == "Antilocapra americana" ~ "Pronghorn",
                                 scientific_name == "Aquila chrysaetos" ~ "Golden eagle",
                                 scientific_name == "Ardea alba" ~ "Great egret",
                                 scientific_name == "Canis latrans" ~ "Coyote",
                                 scientific_name == "Cervus elaphus" ~ "Elk",
                                 scientific_name == "Chen rossii" ~ "Ross's goose",
                                 scientific_name == "Circus cyaneus" ~ "Northern harrier",
                                 scientific_name == "Corvus corax" ~ "Common raven",
                                 scientific_name == "Grus canadensis" ~ "Sandhill crane",
                                 scientific_name == "Haliaeetus leucocephalus" ~ "Golden eagle",
                                 scientific_name == "Lynx rufus" ~ "Bobcat",
                                 scientific_name == "Odocoileus hemionus" ~ "Mule deer",
                                 scientific_name == "Odocoileus virginianus" ~ "White-tailed deer",
                                 scientific_name == "Ovis canadensis" ~ "Big-horned sheep",
                                 scientific_name == "Puma concolor" ~ "Cougar",
                                 scientific_name == "Ursus americanus" ~ "Black bear",
                                 scientific_name == "Ursus arctos" ~ "Brown bear")) %>%
  mutate(taxa = case_when(scientific_name %in% c("Anas acuta",
                                                 "Anas americana",
                                                 "Anas clypeata",
                                                 "Anas crecca",
                                                 "Anas cyanoptera",
                                                 "Anas platyrhynchos",
                                                 "Anas strepera",
                                                 "Anser albifrons",
                                                 "Anser caerulescens",
                                                 "Aquila chrysaetos",
                                                 "Ardea alba",
                                                 "Aquila chrysaetos",
                                                 "Chen rossii",
                                                 "Circus cyaneus",
                                                 "Corvus corax",
                                                 "Haliaeetus leucocephalus",
                                                 "Rallus longirostris",
                                                 "Anas discors",
                                                 "Grus canadensis") ~ "birds",
                          scientific_name %in% c("Alces alces",
                                                 "Antilocapra americana",
                                                 "Canis latrans",
                                                 "Cervus elaphus",
                                                 "Lynx rufus",
                                                 "Odocoileus hemionus",
                                                 "Odocoileus virginianus",
                                                 "Ovis canadensis",
                                                 "Ursus americanus",
                                                 "Ursus arctos") ~ "mammals",
                          scientific_name %in% c("Puma concolor") ~ "cougar")) 

fwrite(species_list,"src/species_list.csv")
  
