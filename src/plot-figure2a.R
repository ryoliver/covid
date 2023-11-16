library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)
library(extrafont)
library(ggimage)
library(magick)
library(ggh4x)

rm(list = ls())

area_icon <- "~/Desktop/covid-results/images/area-size.png"
niche_icon <- "~/Desktop/covid-results/images/niche-size.png"

mammal_icon <-  "~/Desktop/covid-results/images/carnivore.png"
bird_icon <- "~/Desktop/covid-results/images/corvid.png"

# species results
area_ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-10-17.csv") %>%
  mutate(response = rep("area_ghm", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

area_sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-10-17.csv") %>%
  mutate(response = rep("area_sg", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

niche_ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-10-17.csv") %>%
  mutate(response = rep("niche_ghm", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

niche_sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-10-17.csv") %>%
  mutate(response = rep("niche_sg", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

# species list + taxonomy
species_list <- fread("src/species_list.csv")
species_list$taxa[species_list$scientific_name== "Puma concolor"] <- "cougar"




results <- rbind(area_ghm, area_sg, niche_ghm, niche_sg) %>%
  left_join(., species_list, by = c("species" = "scientific_name")) 

area_no_models <- data.frame(species = setdiff(niche_sg$species, area_sg$species)) %>%
  left_join(., species_list, by = c("species" = "scientific_name")) %>%
  select(species, taxa) %>%
  mutate(Estimate = rep(NA, n()))


mobility_order <- results %>%
  filter(response == "area_sg") %>%
  select(species, taxa, Estimate) %>%
  rbind(.,area_no_models) %>%
  group_by(taxa) %>%
  arrange(Estimate, .by_group = TRUE) %>%
  distinct(species) %>%
  mutate("order" = seq(1:n())+1) %>%
  ungroup() %>%
  select(species, order)


results <- left_join(results, mobility_order, by = "species") 

results$response <- factor(results$response,
                           levels = c("area_sg", "niche_sg", "area_ghm", "niche_ghm"))
results$taxa <- factor(results$taxa,
                       levels = c("cougar","mammals","birds"))


x_label <- "Effect size"

p <- ggplot(results) +
  ggh4x::facet_grid2(taxa~response, scales = "free", independent = "x", space = "free") +
  geom_segment(
    aes(x = LCL, y = reorder(common_name, -order), 
        xend = HCL,yend = reorder(common_name, -order),
        group = sig_code,
        color = sig_code),
    size = 2.5,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(common_name, -order), 
                 color = sig_code,
                 group = sig_code), 
             size = 2) +
  scale_color_manual(name ="model structure",
                     values = c("#F98177","#8895BF","#aeb6bf","#6BB0B3")) +
  xlab(x_label) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#4a4e4d", fill=NA, size=1),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 7),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "black") +
  theme(
        strip.text = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.2, "lines"))

ggsave(p, file = "~/Desktop/fig2a.pdf", width = 170, height = 90, units = "mm")

