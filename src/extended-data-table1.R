library(data.table)
library(tidyverse)
library(ggplot2)

rm(list = ls())


# species results
area <- fread("~/Desktop/covid-results/area_ghm_effects_2024-02-15.csv") %>%
  distinct(species, n_weeks, n_ind_yrs, n_ind)

niche <- fread("~/Desktop/covid-results/niche_ghm_effects_2024-02-15.csv") %>%
  distinct(species, n_weeks, n_ind_yrs, n_ind)

species_list <- fread("src/species_list.csv")

area <- left_join(area, species_list, by = c("species" = "scientific_name")) %>%
  select(taxa, common_name, species, n_weeks, n_ind_yrs, n_ind) %>%
  arrange(taxa, common_name) 

niche <- left_join(niche, species_list, by = c("species" = "scientific_name")) %>%
  select(taxa, common_name, species, n_weeks, n_ind_yrs, n_ind) %>%
  arrange(taxa, common_name) 

combined <- left_join(niche, area, by = c("taxa", "species", "common_name")) %>%
  rename("Taxa" = taxa,
         "Common name" = common_name,
         "Scientific name" = species) 


fwrite(combined, "~/Desktop/sample_size_2024-02-15.csv")
