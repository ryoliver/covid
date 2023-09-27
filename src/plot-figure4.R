library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
library(ggbeeswarm)

rm(list = ls())

pred_dat <- fread("~/Desktop/covid-results/area_change_prediction_2023-09-27.csv")

spl <- unique(pred_dat$species)

diff_out <- list()
for(i in 1:length(spl)){
  sp_dat <- pred_dat %>% 
    filter(species == spl[i])
  
  est_low <- sp_dat %>% 
    filter(ghm_case == "low" & sg_case == "low") %>% 
    pull(est_unscaled_exp)
  
  est_high <- sp_dat %>% 
    filter(ghm_case == "high" & sg_case == "high") %>% 
    pull(est_unscaled_exp)
  
  diff <- est_low-est_high
  
  tmp_out <- tibble(species = spl[i],
                    est_low = est_low,
                    est_high = est_high,
                    diff = diff,
                    model = sp_dat$model[1],
                    tot_sig = sp_dat$tot_sig[1])
  
  diff_out[[i]] <- tmp_out
}

area_diff_df <- do.call("rbind", diff_out) %>% 
  mutate(diff_km = diff/1000000,
         prop = est_high/est_low,
         perc_num = -round((1-prop)*100, 0),
         percent_change = ((est_high-est_low)/est_low)*100) %>% 
  filter(tot_sig == "sig") %>% 
  mutate(direct = case_when(diff_km > 0 ~ "p",
                            diff_km < 0 ~ "n"),
         group = rep("a",nrow(.))) %>%
  mutate(taxa = case_when(species %in% c("Anas acuta",
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
                          species %in% c("Alces alces",
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

area_mean_diff <- area_diff_df %>% 
  summarize(mdiff = mean(diff_km)) %>% 
  pull(mdiff) %>% 
  round(1)




pred_dat <- fread("~/Desktop/covid-results/niche_change_prediction_2023-09-27.csv")

spl <- unique(pred_dat$species)

diff_out <- list()
for(i in 1:length(spl)){
  sp_dat <- pred_dat %>% 
    filter(species == spl[i])
  
  est_low <- sp_dat %>% 
    filter(ghm_case == "low" & sg_case == "low") %>% 
    pull(est_unscaled_exp)
  
  est_high <- sp_dat %>% 
    filter(ghm_case == "high" & sg_case == "high") %>% 
    pull(est_unscaled_exp)
  
  diff <- est_low-est_high
  
  tmp_out <- tibble(species = spl[i],
                    est_low = est_low,
                    est_high = est_high,
                    diff = diff,
                    model = sp_dat$model[1],
                    tot_sig = sp_dat$tot_sig[1])
  
  diff_out[[i]] <- tmp_out
}

niche_diff_df <- do.call("rbind", diff_out) %>% 
  mutate(percent_change = ((est_high-est_low)/est_low)*100) %>% 
  filter(tot_sig == "sig") %>% 
  mutate(direct = case_when(diff > 0 ~ "p",
                            diff < 0 ~ "n"),
         group = rep("a",nrow(.))) %>%
  mutate(taxa = case_when(species %in% c("Anas acuta",
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
                          species %in% c("Alces alces",
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


niche_mean_diff <- niche_diff_df %>% 
  summarize(mdiff = mean(percent_change, na.rm = TRUE)) %>% 
  pull(mdiff) %>% 
  round(1)

area_diff_df <- area_diff_df %>%
  filter(species != "Haliaeetus leucocephalus")

max_val <- max(c(min(area_diff_df$diff_km, na.rm = TRUE), max(area_diff_df$diff_km, na.rm = TRUE)))


p1 <- ggplot(area_diff_df, 
             aes(x = -diff_km, y = group, color = taxa)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_vline(aes(xintercept = -area_mean_diff), linetype = "dotted", color = "#4a4e4d") +
  geom_beeswarm(cex = 8, alpha = 0.7, size = 1) +
  geom_point(aes(x = -area_mean_diff), size = 3, shape = 22, 
             fill = "#ABB0AE", color = "#4A4E4D") +
  scale_color_manual(values = c("#FF9B54","#A7D3A6")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = bquote(bold('Change in area size'~(km^2))))

max_val <- max(c(min(niche_diff_df$percent_change, na.rm = TRUE), max(niche_diff_df$percent_change, na.rm = TRUE)))

p2 <- ggplot(niche_diff_df, aes(x = percent_change, y = group, color = taxa)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_vline(aes(xintercept = niche_mean_diff), linetype = "dotted", color = "#4a4e4d") +
  geom_beeswarm(cex = 8, alpha = 0.7, size = 1) +
  geom_point(aes(x = niche_mean_diff), size = 3, shape = 22, 
             fill = "#ABB0AE", color = "#4A4E4D") +
  scale_color_manual(values = c("#FF9B54","#A7D3A6")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = bquote('Change in niche size (%)'))

p <- p1/p2

ggsave(p, file = "~/Desktop/figure4.png", width = 3.5, height = 2)

p1 <- ggplot(area_diff_df, aes(x = -diff_km, fill = taxa)) +
  geom_dotplot(binwidth = 0.5,stackdir = "center", alpha = 0.7) +
  scale_fill_manual(values = c("#FF9B54","#A7D3A6")) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.title.x = element_text(size = 6, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote(bold('Change in area size'~(km^2))))

p <- p1/p2

ggsave(p, file = "~/Desktop/figure4.png", width = 3.5, height = 2)


