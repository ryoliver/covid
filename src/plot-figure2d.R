library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)


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
  mutate(diff_km = -diff/1000000,
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
                                         "Ursus arctos") ~ "mammals")) %>%
  filter(!is.na(taxa))

area_diff <- area_diff_df %>%
  mutate(bin = cut(diff_km, 
                   breaks = seq(-30, 30, by = 2),
                   labels = seq(-29, 29, by = 2))) %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  arrange(taxa) %>%
  mutate(y = seq(1:n()))  %>%
  ungroup() %>%
  mutate(x = as.numeric(as.character(bin)))

niche_diff <- niche_diff_df %>%
  mutate(bin = cut(percent_change, 
                   breaks = seq(-200, 200, by = 10),
                   labels = seq(-195, 195, by = 10))) %>%
  filter(!is.na(bin)) %>%
  group_by(bin) %>%
  arrange(taxa) %>%
  mutate(y = seq(1:n()))  %>%
  ungroup() %>%
  mutate(x = as.numeric(as.character(bin)))

max_val <- max(c(abs(min(area_diff$x, na.rm = TRUE)), max(area_diff$x, na.rm = TRUE)))

p1 <- ggplot(data = area_diff) +
  geom_point(aes(x = x, y = y, color = taxa), size = 1.2) +
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
  scale_y_continuous(breaks = seq(0,10, by = 4), expand = expansion(mult = c(0.05, 0.05))) +  
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = bquote('Change in area size'~(km^2)))

max_val <- max(c(abs(min(niche_diff$x, na.rm = TRUE)), max(niche_diff$x, na.rm = TRUE)))

p2 <- ggplot(data = niche_diff) +
  geom_point(aes(x = x, y = y, color = taxa), size = 1.2) +
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
  scale_y_continuous(breaks = seq(0,10, by = 4), expand = expansion(mult = c(0.15, 0.15))) +  
  coord_cartesian(xlim = c(-max_val,max_val)) +
  labs(x = bquote('Change in niche size (%)'))


p <- p1/p2 +
  plot_layout(heights = c(2.5, 1))
ggsave(p, file = "~/Desktop/figure2d.pdf", width = 2.5, height = 2)
