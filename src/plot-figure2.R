library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)
library(extrafont)
library(ggimage)
library(magick)
library(cowplot)
library(grid)
library(gridExtra)

rm(list = ls())

area_icon <- "~/Desktop/covid-results/images/area-size.png"
niche_icon <- "~/Desktop/covid-results/images/niche-size.png"

mammal_icon <-  "~/Desktop/covid-results/images/carnivore.png"
bird_icon <- "~/Desktop/covid-results/images/corvid.png"


species_names <- data.frame("scientific_name" = c("Alces alces",
                                                  "Antilocapra americana",
                                                  "Cervus elaphus",
                                                  "Odocoileus hemionus",
                                                  "Odocoileus virginianus",
                                                  "Ovis canadensis",
                                                  "Canis latrans",
                                                  "Canis lupus",
                                                  "Lynx rufus",
                                                  "Puma concolor",
                                                  "Ursus americanus",
                                                  "Ursus arctos",
                                                  "Anser caerulescens",
                                                  "Ardea alba",
                                                  "Aquila chrysaetos",
                                                  "Corvus corax",
                                                  "Haliaeetus leucocephalus"),
                            "common_name" = c("Moose",
                                              "Pronghorn",
                                              "Elk",
                                              "Mule deer",
                                              "White-tailed deer",
                                              "Bighorn sheep",
                                              "Coyote",
                                              "Wolf",
                                              "Bobcat",
                                              "Cougar",
                                              "Black bear",
                                              "Grizzly bear",
                                              "Snow goose",
                                              "Great egret",
                                              "Golden eagle",
                                              "Common raven",
                                              "Bald eagle")) %>%
  mutate(taxa = case_when(scientific_name %in% c("Anser caerulescens",
                                                 "Ardea alba",
                                                 "Aquila chrysaetos",
                                                 "Corvus corax",
                                                 "Haliaeetus leucocephalus") ~ "birds",
                          scientific_name %in% c("Alces alces",
                                                 "Antilocapra americana",
                                                 "Cervus elaphus",
                                                 "Odocoileus hemionus",
                                                 "Odocoileus virginianus",
                                                 "Ovis canadensis",
                                                 "Canis latrans",
                                                 "Canis lupus",
                                                 "Lynx rufus",
                                                 "Puma concolor",
                                                 "Ursus americanus",
                                                 "Ursus arctos") ~ "mammals")) 

ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-05-02.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)
sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-05-02.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

space_use <- rbind(ghm, sg)

space_use <- space_use %>%
  left_join(.,species_names, by = c("species" = "scientific_name")) %>%
  filter(!species %in% c("Anser caerulescens")) 

area_meta_ghm <- fread("~/Desktop/covid-results/area_meta_ghm.csv") %>%
  filter(Parameter != "sigma") %>%
  mutate(species = rep(NA, nrow(.)),
         common_name = rep("Mean response", nrow(.)),
         taxa = rep(NA, nrow(.)),
         sig_code = rep(NA, nrow(.)),
         Estimate = Median,
         LCL = CI_low,
         HCL = CI_high,
         driver = "Human modification") %>%
  mutate(taxa = case_when(Parameter == "b_classbird" ~ "birds",
                          Parameter == "b_classmammal" ~ "mammals")) %>%
  mutate(species = taxa) %>%
  select(species, common_name, taxa, sig_code, Estimate, LCL, HCL, driver)

area_meta_sg <- fread("~/Desktop/covid-results/area_meta_sg.csv") %>%
  filter(Parameter != "sigma") %>%
  mutate(species = rep(NA, nrow(.)),
         common_name = rep("Mean response", nrow(.)),
         taxa = rep(NA, nrow(.)),
         sig_code = rep(NA, nrow(.)),
         Estimate = Median,
         LCL = CI_low,
         HCL = CI_high,
         driver = "Human mobility") %>%
  mutate(taxa = case_when(Parameter == "b_classbird" ~ "birds",
                          Parameter == "b_classmammal" ~ "mammals")) %>%
  mutate(species = taxa) %>%
  select(species, common_name, taxa, sig_code, Estimate, LCL, HCL, driver)

space_use <- rbind(space_use, area_meta_ghm, area_meta_sg)

space_use$sig_code <- factor(space_use$sig_code, levels = c("low_int","high_int","sig_add", "ns_add"))
space_use$taxa <- factor(space_use$taxa, levels = c("mammals","birds"))
space_use$driver <- factor(space_use$driver, levels = c("Human modification","Human mobility"))

ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-05-11.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)
sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-05-11.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

niche <- rbind(ghm, sg)


niche <- niche %>%
  left_join(.,species_names, by = c("species" = "scientific_name")) %>%
  filter(!species %in% c("Numenius americanus","Anser caerulescens","Sus scrofa","Grus canadensis"))

niche_meta_ghm <- fread("~/Desktop/covid-results/niche_meta_ghm.csv") %>%
  filter(Parameter != "sigma") %>%
  mutate(species = rep(NA, nrow(.)),
         common_name = rep("Mean response", nrow(.)),
         taxa = rep(NA, nrow(.)),
         sig_code = rep(NA, nrow(.)),
         Estimate = Median,
         LCL = CI_low,
         HCL = CI_high,
         driver = "Human modification") %>%
  mutate(taxa = case_when(Parameter == "b_classbird" ~ "birds",
                          Parameter == "b_classmammal" ~ "mammals")) %>%
  mutate(species = taxa) %>%
  select(species, common_name, taxa, sig_code, Estimate, LCL, HCL, driver)

niche_meta_sg <- fread("~/Desktop/covid-results/niche_meta_sg.csv") %>%
  filter(Parameter != "sigma") %>%
  mutate(species = rep(NA, nrow(.)),
         common_name = rep("Mean response", nrow(.)),
         taxa = rep(NA, nrow(.)),
         sig_code = rep(NA, nrow(.)),
         Estimate = Median,
         LCL = CI_low,
         HCL = CI_high,
         driver = "Human mobility") %>%
  mutate(taxa = case_when(Parameter == "b_classbird" ~ "birds",
                          Parameter == "b_classmammal" ~ "mammals")) %>%
  mutate(species = taxa) %>%
  select(species, common_name, taxa, sig_code, Estimate, LCL, HCL, driver)

niche <- rbind(niche, niche_meta_ghm, niche_meta_sg)

niche$sig_code <- factor(niche$sig_code, levels = c("low_int","high_int","sig_add", "ns_add"))
niche$taxa <- factor(niche$taxa, levels = c("mammals","birds"))
niche$driver <- factor(niche$driver, levels = c("Human modification","Human mobility"))



mobility_order <- space_use %>%
  filter(driver == "Human mobility") %>%
  group_by(taxa) %>%
  arrange(Estimate, .by_group = TRUE) %>%
  distinct(species) %>%
  mutate("order" = seq(1:n())+1) %>%
  ungroup() %>%
  select(species, order)

mobility_order[mobility_order$species %in% c("birds","mammals"),]$order <- 1

modification_order <- space_use %>%
  filter(driver == "Human modification") %>%
  group_by(taxa) %>%
  arrange(Estimate, .by_group = TRUE) %>%
  distinct(species) %>%
  mutate("order" = seq(1:n())+1) %>%
  ungroup() %>%
  select(species, order)

modification_order[modification_order$species %in% c("birds","mammals"),]$order <- 1


space_use_mobility <- space_use %>%
  filter(driver == "Human mobility") %>%
  filter(species != "Puma concolor") %>%
  left_join(.,mobility_order, by = "species")

space_use_modification <- space_use %>%
  filter(driver == "Human modification") %>%
  left_join(.,modification_order, by = "species")

niche_mobility <- niche %>%
  filter(driver == "Human mobility") %>%
  filter(!species %in% c("Puma concolor")) %>%
  left_join(.,mobility_order, by = "species")

niche_modification <- niche %>%
  filter(driver == "Human modification") %>%
  left_join(.,modification_order, by = "species")

x_label <- "Effect size"

su <- rbind(space_use_mobility, space_use_modification)
min_val <- min(su$LCL)
max_val <- max(su$HCL)

p1 <- ggplot(space_use_mobility) +
  geom_segment(
    aes(x = LCL, y = reorder(common_name, -order), 
        xend = HCL,yend = reorder(common_name, -order),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(common_name, -order), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="model structure",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  scale_y_discrete(labels=c("Mean response"=expression(bold("Mean response")), parse=TRUE)) +
  xlab(x_label) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines")) +
  ggtitle("Effect of human mobility") +
  labs(tag = bquote(bold("a")))

p3 <- ggplot(space_use_modification) +
  geom_segment(
    aes(x = LCL, y = reorder(common_name, -order), 
        xend = HCL,yend = reorder(common_name, -order),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(common_name, -order), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="model structure",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  scale_y_discrete(labels=c("Mean response"=expression(bold("Mean response")), parse=TRUE)) +
  xlab(x_label) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines")) +
  ggtitle("Effect of human modification") +
  labs(tag = bquote(bold("c")))

n <- rbind(niche_mobility, niche_modification)
min_val <- min(n$LCL)
max_val <- max(n$HCL)


p2 <- ggplot(niche_mobility) +
  geom_segment(
    aes(x = LCL, y = reorder(common_name, -order), 
        xend = HCL,yend = reorder(common_name, -order),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(common_name, -order), 
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
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines")) +
  theme(strip.text.y = element_text(angle = 270, 
                                    hjust = 0,
                                    face = "bold",
                                    size = 9,
                                    margin = margin(0,0,0,0, "cm")),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.background = element_rect(fill = "#DDE0E4",
                                        color = "transparent")) +
  ggtitle(" ") +
  labs(tag = bquote(bold("b")))


p4 <- ggplot(niche_modification) +
  geom_segment(
    aes(x = LCL, y = reorder(common_name, -order), 
        xend = HCL,yend = reorder(common_name, -order),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(common_name, -order), 
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
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_text(angle = 270, 
                                    hjust = 0,
                                    face = "bold",
                                    size = 9,
                                    margin = margin(0,0,0,0, "cm")),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.background = element_rect(fill = "#DDE0E4",
                                        color = "transparent"))  +
  ggtitle(" ") +
  labs(tag = bquote(bold("d")))

puma_space_use_mobility <- space_use %>%
  filter(species == "Puma concolor") %>%
  filter(driver == "Human mobility")

puma_niche_mobility <- niche %>%
  filter(species == "Puma concolor") %>%
  filter(driver == "Human mobility")




puma1 <- ggplot(puma_space_use_mobility) +
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
  scale_color_manual(values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7.5),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") 


puma2 <- ggplot(puma_niche_mobility) +
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
  scale_color_manual(values = "#6BB0B3") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 0.38)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7.5),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") 


p <- (p1 + p2)/(puma1 + puma2)/(p3 + p4) +
  plot_layout(heights = c(9,1,9)) 


ggsave(p, file = "~/Desktop/figure2.png", width = 8, height = 7)



