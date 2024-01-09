library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)

rm(list = ls())
species_list <- fread("src/species_list.csv")

pred_dat <- fread("~/Desktop/covid-results/area_change_prediction_2023-11-20.csv")

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
  mutate(diff_km = -diff/1000000,
         prop = est_high/est_low,
         perc_num = -round((1-prop)*100, 0),
         percent_change = ((est_high-est_low)/est_low)*100) %>% 
  filter(tot_sig == "sig") %>% 
  mutate(direct = case_when(diff_km > 0 ~ "p",
                            diff_km < 0 ~ "n"),
         group = rep("a",nrow(.))) %>%
  left_join(., species_list, by = c("species" = "scientific_name"))


pred_dat <- fread("~/Desktop/covid-results/niche_change_prediction_2023-11-20.csv")

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
  left_join(., species_list, by = c("species" = "scientific_name"))



area_diff <- area_diff_df %>%
  select(species, common_name, taxa, percent_change) %>%
  mutate(bin = cut(percent_change, 
                   breaks = c(-100000,-10000,-1000,-100,-10,-1,-0.1,0,0.1,1,10,100,1000,10000,1000000),
                   labels = c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5))) %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  arrange(taxa) %>%
  mutate(y = seq(1:n()))  %>%
  ungroup() %>%
  mutate(x = as.numeric(as.character(bin))) 


niche_diff <- niche_diff_df %>%
  select(species, common_name, taxa, percent_change) %>%
  mutate(bin = cut(percent_change, 
                   breaks = c(-100000,-10000,-1000,-100,-10,-1,-0.1,0,0.1,1,10,100,1000,10000,1000000),
                   labels = c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5))) %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  arrange(taxa) %>%
  mutate(y = seq(1:n()))  %>%
  ungroup() %>%
  mutate(x = as.numeric(as.character(bin))) 


max_val <- max(c(abs(min(area_diff$x)), max(area_diff$x),abs(min(niche_diff$x)), max(niche_diff$x)))


p1 <- ggplot(data = area_diff) +
  geom_point(aes(x = x, y = y, color = taxa), size = 2) +
  scale_fill_manual(values = c("#FF9B54","#A7D3A6")) +
  scale_color_manual(values = c("#FF9B54","#A7D3A6")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "black") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    legend.position = "none",
    legend.title = element_blank(),
    #axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  scale_y_continuous(breaks = seq(0,10, by = 2), expand = expansion(mult = c(0.2, 0.2))) +  
  scale_x_continuous(breaks = seq(-6,6, by = 1),
                     labels = c("-10K", "-1K", "-100", "-10", "-1", "-0.1", "0",
                                "0.1","1", "10", "100", "1K", "10K")) +
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = 'Change in area size (%)')


p2 <- ggplot(data = niche_diff) +
  geom_point(aes(x = x, y = y, color = taxa), size = 2) +
  scale_fill_manual(values = c("#FF9B54","#A7D3A6")) +
  scale_color_manual(values = c("#FF9B54","#A7D3A6")) +
  geom_vline(aes(xintercept = 0), linetype = "solid", size = 0.5, alpha = 0.8, color = "black") +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_line(colour = "#4a4e4d", linewidth =0.3, linetype='solid'),
    legend.position = "none",
    legend.title = element_blank(),
    #axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    axis.ticks.x = element_line(color = "#4a4e4d")) +
  scale_y_continuous(breaks = seq(0,10, by = 2), expand = expansion(mult = c(0.2, 0.2))) +  
  scale_x_continuous(breaks = seq(-6,6, by = 1),
                     labels = c("-10K", "-1K", "-100", "-10", "-1", "-0.1", "0",
                                "0.1","1", "10", "100", "1K", "10K")) +
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = 'Change in niche size (%)')

p <- p1/p2 +
  plot_layout(heights = c(1.2,1))
ggsave(p, file = "~/Desktop/figure4.pdf", height = 55, width = 90, units = "mm")

area_diff_df_mammals <- area_diff_df %>%
  filter(taxa == "mammals")
  

median(area_diff_df_mammals$percent_change)
range(area_diff_df_mammals$diff_km)

area_diff_df_birds <- area_diff_df %>%
  filter(taxa == "birds") 

median(area_diff_df_birds$percent_change)
range(area_diff_df_birds$diff_km)


median(niche_diff_df$percent_change)
mean(niche_diff_df$percent_change)
range(niche_diff_df$percent_change)

niche_diff_df_mammals <- niche_diff_df %>%
  filter(taxa == "mammals")

median(niche_diff_df_mammals$percent_change)
mean(niche_diff_df_mammals$percent_change)
range(niche_diff_df_mammals$percent_change)

niche_diff_df_birds <- niche_diff_df %>%
  filter(taxa == "birds") 

median(niche_diff_df_birds$percent_change)
mean(niche_diff_df_birds$percent_change)
range(niche_diff_df_birds$percent_change)

