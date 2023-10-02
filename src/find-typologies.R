library(data.table)
library(tidyverse)

rm(list = ls())



area_ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-09-26.csv") %>%
  select(species, Estimate, LCL, HCL, sig_code) %>%
  mutate("ghm_sig" = ifelse(sig_code != "ns_add", 1, 0)) %>%
  distinct(species, ghm_sig)

area_sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-09-26.csv") %>%
  select(species, Estimate, LCL, HCL, sig_code) %>%
  mutate("sg_sig" = ifelse(sig_code != "ns_add", 1, 0)) %>%
  distinct(species, sg_sig)

area <- left_join(area_ghm, area_sg, by = "species") %>%
  mutate(area_sig = rowSums(across(c(ghm_sig, sg_sig)))) %>%
  mutate(area_sig = ifelse(area_sig > 0, 1, 0)) %>%
  select(-ghm_sig, -sg_sig)

niche_ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-09-27.csv") %>%
  select(species, Estimate, LCL, HCL, sig_code) %>%
  mutate("ghm_sig" = ifelse(sig_code != "ns_add", 1, 0)) %>%
  distinct(species, ghm_sig)

niche_sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-09-27.csv") %>%
  select(species, Estimate, LCL, HCL, sig_code) %>%
  mutate("sg_sig" = ifelse(sig_code != "ns_add", 1, 0)) %>%
  distinct(species, sg_sig)

niche <- left_join(niche_ghm, niche_sg, by = "species") %>%
  mutate(niche_sig = rowSums(across(c(ghm_sig, sg_sig)))) %>%
  mutate(niche_sig = ifelse(niche_sig > 0, 1, 0)) %>%
  select(-ghm_sig, -sg_sig)

## NOTE: common names are hardcoded 
species_name <- data.frame(scientific_name = sort(intersect(area_ghm$species, niche_ghm$species)),
                           common_name = c("Moose",
                                           "Northern pintail",
                                           "American wigeon",
                                           "Northern shoveler",
                                           "Common teal",
                                           "Cinnamon teal",
                                           "Mallard",
                                           "Gadwall",
                                           "GWF goose",
                                           "Snow goose",
                                           "Pronghorn",
                                           "Golden eagle",
                                           "Great egret",
                                           "Coyote",
                                           "Elk",
                                           "Ross's goose",
                                           "Hen harrier",
                                           "Common raven",
                                           "Bald eagle",
                                           "Bobcat",
                                           "Mule deer",
                                           "White-tailed deer",
                                           "Cougar",
                                           "Clapper rail",
                                           "Black bear",
                                           "Grizzly bear"))

summary <- left_join(niche, area, by = "species") %>%
  replace(is.na(.),0) %>%
  mutate(typology_1 = ifelse(area_sig == 1 & niche_sig == 0, 1, 0),
         typology_2 = ifelse(area_sig == 0 & niche_sig == 1, 1, 0),
         typology_3 = ifelse(area_sig == 1 & niche_sig == 1, 1, 0),
         typology_4 = ifelse(area_sig == 0 & niche_sig == 0, 1, 0)) %>%
  mutate(test = rowSums(across(c(typology_1, typology_2, typology_3, typology_4)))) %>%
  left_join(., species_name, by = c("species" = "scientific_name"))

print("typology 1")  
summary %>%
  filter(typology_1 == 1) %>%
  select(common_name)

print("typology 2")  
summary %>%
  filter(typology_2 == 1) %>%
  select(common_name)

print("typology 3")  
summary %>%
  filter(typology_3 == 1) %>%
  select(common_name)

print("typology 4")  
summary %>%
  filter(typology_4 == 1) %>%
  select(common_name)
