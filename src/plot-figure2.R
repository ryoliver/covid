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


ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-05-02.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)
sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-05-02.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

puma_ghm_area <- ghm %>%
  filter(species == "Puma concolor")
puma_sg_area <- sg %>%
  filter(species == "Puma concolor")

sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-05-02.csv") %>%
  filter(species != "Puma concolor") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

space_use <- rbind(ghm, sg)

space_use <- space_use %>%
  filter(!species %in% c("Anser caerulescens")) %>%
  mutate(taxa = case_when(species %in% c("Anser caerulescens",
                                         "Ardea alba",
                                         "Haliaeetus leucocephalus",
                                         "Aquila chrysaetos",
                                         "Corvus corax",
                                         "Haliaeetus leucocephalus") ~ "birds",
                          species %in% c("Alces alces",
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

space_use$sig_code <- factor(space_use$sig_code, levels = c("low_int","high_int","sig_add", "ns_add"))
space_use$taxa <- factor(space_use$taxa, levels = c("mammals","birds"))
space_use$driver <- factor(space_use$driver, levels = c("Human modification","Human mobility"))


x_label <- "Effect size"

min_val <- min(space_use$LCL)
max_val <- max(space_use$HCL)


p1 <- ggplot(subset(space_use, driver == "Human modification")) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="Effect type",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.3,0.6),
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10,
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_blank(),
        panel.spacing.y = unit(1, "lines")) +
  ggtitle("Effect of human modification") +
  labs(tag = expression(bold("c")), y = "Species", x = x_label)


p2 <- ggplot(subset(space_use, driver == "Human mobility")) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
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
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10,
                                    face = "bold"),
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
  labs(tag = expression(bold("a")), y = "Species")

ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-05-11.csv") %>%
  mutate("driver" = rep("Human modification", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)
sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-05-11.csv") %>%
  mutate("driver" = rep("Human mobility", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, driver)

puma_ghm_niche <- ghm %>%
  filter(species == "Puma concolor")
puma_sg_niche <- sg %>%
  filter(species == "Puma concolor")

niche <- rbind(ghm, sg)


niche <- niche %>%
  filter(!species %in% c("Numenius americanus","Anser caerulescens","Sus scrofa")) %>%
  mutate(taxa = case_when(species %in% c("Anser caerulescens",
                                         "Ardea alba",
                                         "Grus canadensis",
                                         "Haliaeetus leucocephalus",
                                         "Aquila chrysaetos",
                                         "Corvus corax",
                                         "Haliaeetus leucocephalus") ~ "birds",
                          species %in% c("Alces alces",
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

niche$sig_code <- factor(niche$sig_code, levels = c("low_int","high_int","sig_add", "ns_add"))
niche$taxa <- factor(niche$taxa, levels = c("mammals","birds"))
niche$driver <- factor(niche$driver, levels = c("Human modification","Human mobility"))

min_val <- min(niche$LCL)
max_val <- max(niche$HCL)


p3 <- ggplot(subset(niche, driver == "Human modification")) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="model structure",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
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
                                    size = 10),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.background = element_rect(fill = "#DDE0E4",
                                        color = "transparent"))  +
  labs(tag = expression(bold("d")), x = x_label)

p4 <- ggplot(subset(niche, driver == "Human mobility")) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
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
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  coord_cartesian(xlim = c(min_val, max_val)) +
  facet_grid(rows = vars(taxa), scales = "free_y", space = "free") +
  theme(strip.text.y = element_text(angle = 270, 
                                    hjust = 0,
                                    face = "bold",
                                    size = 10),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.background = element_rect(fill = "#DDE0E4",
                                        color = "transparent"))  +
  labs(tag = expression(bold("b")))


puma_plot <- ggplot(puma_sg_area) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(name ="Effect type",
                     values = c("#F98177","#8895BF","#6BB0B3","#aeb6bf"),
                     labels = c("interaction, low estimate",
                                "interaction, high estimate",
                                "additive",
                                "not significant")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold",
                                  size = 9),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  labs(title = "Cougar")

p <- plot_spacer()/(p2 + p4)/(p1 + p3) +
  plot_layout(heights = c(1,6,6)) +
  inset_element(puma_plot, left = 0, bottom = 1.7, right = 0.2, top = 2)

p <- ggdraw() +
  draw_plot(p, width = 1)+
  draw_image(image_read(area_icon), x = 0.18, y = 0.86, height = 0.15, width = 0.15) +
  draw_image(image_read(niche_icon), x = 0.65, y = 0.86, height = 0.15, width = 0.15) +
  draw_image(image_read(mammal_icon), x = 0.94, y = 0.828, height = 0.04, width = 0.04) +
  draw_image(image_read(bird_icon), x = 0.94, y = 0.598, height = 0.03, width = 0.03) +
  draw_image(image_read(mammal_icon), x = 0.94, y = 0.392, height = 0.04, width = 0.04) +
  draw_image(image_read(bird_icon), x = 0.94, y = 0.16, height = 0.03, width = 0.03)


ggsave(p, file = "~/Desktop/test.png", height = 6.5, width = 7)

puma <- rbind(puma_ghm_area, puma_sg_area)


puma_sg_area_plot <- ggplot(puma_sg_area) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
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
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  ggtitle("Effect of human mobility")

puma_ghm_area_plot <- ggplot(puma_ghm_area) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
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
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") +
  ggtitle("Effect of human modification")


puma_sg_niche_plot <- ggplot(puma_sg_niche) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(values = c("#6BB0B3")) +
  xlab(x_label) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") 

puma_ghm_niche_plot <- ggplot(puma_ghm_niche) +
  geom_segment(
    aes(x = LCL, y = reorder(species, -Estimate), 
        xend = HCL,yend = reorder(species, -Estimate),
        group = sig_code,
        color = sig_code),
    size = 3,
    alpha = 0.3,
    lineend = "round") +
  geom_point(aes(x = Estimate, y = reorder(species, -Estimate), 
                 color = sig_code,
                 group = sig_code), 
             size = 2.5) +
  scale_color_manual(values = c("#6BB0B3")) +
  xlab(x_label) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 9,
                                    face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Arial", color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "#4a4e4d") 

p_puma <- (puma_sg_area_plot + puma_sg_niche_plot)/(puma_ghm_area_plot + puma_ghm_niche_plot)

ggsave(p_puma, file = "~/Desktop/test_puma.png", height = 2.3, width = 4)

