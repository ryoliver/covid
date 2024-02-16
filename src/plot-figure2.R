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


# species results
area_ghm <- fread("~/Desktop/covid-results/area_ghm_effects_2023-11-20.csv") %>%
  mutate(response = rep("area_ghm", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

area_sg <- fread("~/Desktop/covid-results/area_sg_effects_2023-11-20.csv") %>%
  mutate(response = rep("area_sg", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

niche_ghm <- fread("~/Desktop/covid-results/niche_ghm_effects_2023-11-20.csv") %>%
  mutate(response = rep("niche_ghm", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

niche_sg <- fread("~/Desktop/covid-results/niche_sg_effects_2023-11-20.csv") %>%
  mutate(response = rep("niche_sg", nrow(.))) %>%
  select(species, Estimate, LCL, HCL, sig_code, response)

# species list + taxonomy
species_list <- fread("src/species_list.csv")
species_list$taxa[species_list$scientific_name== "Puma concolor"] <- "cougar"

species_list$taxa[species_list$scientific_name== "Numenius americanus"] <- "curlew"




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
                       levels = c("cougar","mammals","curlew","birds"))


x_label <- "Effect size"

sig_species <- results %>%
  filter(sig_code != "ns_add") %>%
  distinct(species) %>%
  left_join(., species_list, by = c("species" = "scientific_name"))

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
                     values = c("#fcaf58ff","#9a7aa0ff","#aeb6bf","#79B473")) +
  scale_y_discrete(labels=c( "Moose"=expression(bold(Moose)),
                             "Pronghorn"=expression(bold(Pronghorn)),
                             "Great egret"=expression(bold("Great egret")),
                             "Coyote"=expression(bold(Coyote)),
                             "Common raven"=expression(bold("Common raven")),
                             "Bobcat"=expression(bold(Bobcat)),
                             "Long-billed curlew"=expression(bold("Long-billed curlew")),
                             "Mule deer"=expression(bold("Mule deer")),
                             "Bighorn sheep"=expression(bold("Bighorn sheep")),
                             "Cougar"=expression(bold(Cougar)),
                             "Brown bear"=expression(bold("Brown bear")),
                             "Northern harrier"=expression(bold("Northern harrier")),
                             "White-tailed deer"=expression(bold("White-tailed deer")),
                             "Cinnamon teal"=expression(bold("Cinnamon teal")),
                             "Mallard"=expression(bold(Mallard)),
                             "Snow goose"=expression(bold("Snow goose")),
                             "Elk"=expression(bold(Elk)),
                             "Northern shoveler"=expression(bold("Northern shoveler")),
                             "Gadwall"=expression(bold(Gadwall)),
                             parse=TRUE)) +
  
  xlab(x_label) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "#aeb6bf", fill=NA, size=1),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 7),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "black") +
  theme(
        #axis.text.y = element_text(face = ifelse(results$common_name %in% sig_species$common_name, "bold", "italic")),
        strip.text = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.2, "lines"))

ggsave(p, file = "~/Desktop/fig2a.pdf", width = 175, height = 110, units = "mm")

results_clean <- results %>%
  mutate(driver = ifelse(response %in% c("area_sg", "niche_sg"), "mobility", "modification"),
         response = case_when(response == "area_sg" ~ "area size",
                              response == "area_ghm" ~ "area size",
                              response == "niche_sg" ~ "niche size",
                              response == "niche_ghm" ~ "niche size")) %>%
  filter(!sig_code %in% c("low_int", "ns_add"))

# species that show responses in both area AND niche size
species_both_responses <- results_clean %>%
  group_by(driver, species) %>%
  summarise(n_responses = n()) %>%
  filter(n_responses > 1)

# count number of species that show responses in both area AND niche size per driver
summarize_species_both_responses <- species_both_responses %>%
  group_by(driver) %>%
  summarise(n_species = n()) %>%
  mutate(response = rep("both", n()))

# count number of species that respond in either area OR niche size
species_single_response <- results_clean %>%
  filter(common_name != "Snow goose") %>%
  group_by(driver, species) %>%
  summarise(n_responses = n()) %>%
  filter(n_responses == 1) 

summarize_species_single_response <- results_clean %>%
  left_join(., species_single_response) %>% 
  filter(!is.na(n_responses)) %>%
  group_by(response, driver) %>%
  summarise(n_species = n())

# combine summaries
summarize_species_drivers <- rbind(summarize_species_both_responses, summarize_species_single_response) %>%
  arrange(driver) %>%
  mutate(driver = case_when(driver == "mobility" ~ "human mobility",
                            driver == "modification" ~ "landscape modification"))

summarize_species_drivers$driver <- factor(summarize_species_drivers$driver,
                                           levels = c("landscape modification","human mobility"))

summarize_species_drivers$response <- factor(summarize_species_drivers$response,
       levels = c("niche size", "both","area size"))

total_species_drivers <- summarize_species_drivers %>%
  group_by(driver) %>%
  summarise("n_total" = sum(n_species))

summarize_species_drivers <- summarize_species_drivers %>%
  left_join(., total_species_drivers, by = "driver") %>%
  mutate("percent_species" = (n_species/n_total)*100)

p_drivers <- ggplot(summarize_species_drivers, aes(fill=response, y=driver, x=percent_species)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#878188", "#B9B5BA","#DCDCDD")) +
  
  scale_y_discrete(labels = c("human mobility" = "human\nmobility",
                              "landscape modification" = "landscape\nmodification")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    legend.text = element_text(size = 7),
    axis.text = element_text(size = 7),
    #axis.text.y = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 7),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = "Species (%)")

# species that show drivers in both area AND niche size
species_both_drivers <- results_clean %>%
  group_by(response, species) %>%
  summarise(n_drivers = n()) %>%
  filter(n_drivers > 1)

# count number of species that show drivers in both area AND niche size per driver
summarize_species_both_drivers <- species_both_drivers %>%
  group_by(response) %>%
  summarise(n_species = n()) %>%
  mutate(driver = rep("both", n()))

# count number of species that respond in either area OR niche size
species_single_driver <- results_clean %>%
  group_by(response, species) %>%
  summarise(n_drivers = n()) %>%
  filter(n_drivers == 1) 

summarize_species_single_driver <- results_clean %>%
  left_join(., species_single_driver) %>% 
  filter(!is.na(n_drivers)) %>%
  group_by(response, driver) %>%
  summarise(n_species = n())

# combine summaries
summarize_species_responses <- rbind(summarize_species_both_drivers, summarize_species_single_driver) %>%
  arrange(response) %>%
  mutate(driver = case_when(driver == "mobility" ~ "human mobility",
                            driver == "modification" ~ "landscape modification",
                            driver == "both" ~ "both"))

summarize_species_responses$driver <- factor(summarize_species_responses$driver,
                                             levels = c("landscape modification","both","human mobility"))

summarize_species_responses$response <- factor(summarize_species_responses$response,
                                               levels = c("niche size", "area size"))

total_species_responses <- summarize_species_responses %>%
  group_by(response) %>%
  summarise("n_total" = sum(n_species))

summarize_species_responses <- summarize_species_responses %>%
  left_join(., total_species_responses, by = "response") %>%
  mutate("percent_species" = (n_species/n_total)*100)

p_responses <- ggplot(summarize_species_responses, aes(fill=driver, y=response, x=percent_species)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_manual(values = c("#556677", "#93A3B4","#C3CCD5")) +
  scale_fill_manual(values = c("#878188", "#B9B5BA","#DCDCDD")) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(size = 7),
    axis.text = element_text(size = 7),
    #axis.text.y = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 7),
    axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = "Species (%)")

p_all <- p_responses + p_drivers

ggsave(p_all, file = "~/Desktop/fig2b.pdf", width = 170, height = 30, units = "mm")

summarize_species_responses

summarize_species_drivers

