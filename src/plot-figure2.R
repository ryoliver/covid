library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)
library(extrafont)
library(ggimage)
library(magick)

rm(list = ls())

area_icon <- "~/Desktop/covid-results/images/area-size.png"
niche_icon <- "~/Desktop/covid-results/images/niche-size.png"

mammal_icon <-  "~/Desktop/covid-results/images/carnivore.png"
bird_icon <- "~/Desktop/covid-results/images/corvid.png"

area_ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-09-26.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

area_sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-09-26.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

niche_ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-09-27.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

niche_sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-09-27.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

species_name <- data.frame(scientific_name = sort(intersect(area_ghm$species, niche_ghm$species)),
                           common_name = c("Moose",
                                           "Northern pintail",
                                           "American wigeon",
                                           "Northern shoveler",
                                           "Common teal",
                                           "Cinnamon teal",
                                           "Mallard",
                                           "Gadwall",
                                           "Greater white-fronted goose",
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
                                           "Grizzly bear")) %>%
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
                                                 "Rallus longirostris") ~ "birds",
                          scientific_name %in% c("Alces alces",
                                                 "Antilocapra americana",
                                                 "Canis latrans",
                                                 "Cervus elaphus",
                                                 "Lynx rufus",
                                                 "Odocoileus hemionus",
                                                 "Odocoileus virginianus",
                                                 "Ovis canadensis",
                                                 "Puma concolor",
                                                 "Ursus americanus",
                                                 "Ursus arctos") ~ "mammals")) 
  

area_ghm <- area_ghm %>%
  filter(species %in% species_name$scientific_name) %>%
  mutate(response = rep("area_ghm", nrow(.)))

area_sg <- area_sg %>%
  filter(species %in% species_name$scientific_name) %>%
  mutate(response = rep("area_sg", nrow(.)))

niche_ghm <- niche_ghm %>%
  filter(species %in% species_name$scientific_name) %>%
  mutate(response = rep("niche_ghm", nrow(.)))

niche_sg <- niche_sg %>%
  filter(species %in% species_name$scientific_name) %>%
  mutate(response = rep("niche_sg", nrow(.)))

results <- rbind(area_ghm, area_sg, niche_ghm, niche_sg) %>%
  left_join(., species_name, by = c("species" = "scientific_name")) %>%
  filter(common_name != "Cougar")

results$response <- factor(results$response,
                           levels = c("area_sg", "area_ghm", "niche_sg", "niche_ghm"))
results$taxa <- factor(results$taxa,
                       levels = c("mammals", "birds"))

x_label <- "Effect size"

ggplot(results) +
  facet_grid(rows = vars(taxa), cols = vars(response), scales = "free_y", space = "free") +
  geom_segment(
    aes(x = LCL, y = common_name, 
        xend = HCL,yend = common_name,
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = common_name, 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="model structure",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  xlab(x_label) +
  theme_minimal() +
  theme(
    #panel.grid.major.y = element_blank(),
     #   panel.grid.minor.y = element_blank(),
      #  panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "#4a4e4d", fill=NA, size=1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "black")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  theme(strip.text.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"))

+
  scale_y_discrete(labels=c("Mean response"=expression(bold("Mean response")), parse=TRUE)) +
  


