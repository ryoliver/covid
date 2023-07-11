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


pred_dat <- read_csv("~/Desktop/covid-results/area_change_prediction_2023-06-23.csv")

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
         group = rep("a",nrow(.)))

area_mean_diff <- area_diff_df %>% 
  summarize(mdiff = mean(diff_km)) %>% 
  pull(mdiff) %>% 
  round(1)




pred_dat <- read_csv("~/Desktop/covid-results/niche_change_prediction_2023-07-11.csv")

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
         group = rep("a",nrow(.)))

niche_mean_diff <- niche_diff_df %>% 
  summarize(mdiff = mean(percent_change, na.rm = TRUE)) %>% 
  pull(mdiff) %>% 
  round(1)

p1 <- ggplot(area_diff_df, aes(x = -diff_km, y = group, size = abs(percent_change))) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(color = "#759FBD",cex = 4, alpha = 0.7) +
  geom_point(aes(x = -area_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  scale_size_continuous(trans = "log10") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote(bold('Change in area size'~(km^2))))


p2 <- ggplot(niche_diff_df, aes(x = percent_change, y = group, size = abs(percent_change))) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(color = "#759FBD",cex = 4, alpha = 0.7) +
  geom_point(aes(x = niche_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  scale_size_continuous(trans = "log10", limits = c(1,16000), breaks = c(1,10,100,1000,10000)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 9),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote('Change in niche size (%)'),
       size = "percent change")

p <- p1/p2

ggsave(p, file = "~/Desktop/test.png", width = 5, height = 4)


p1 <- ggplot(area_diff_df, aes(x = -diff_km, y = group)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(color = "#759FBD",cex = 4, alpha = 0.7, size = 3) +
  geom_point(aes(x = -area_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote(bold('Change in area size'~(km^2))))


p2 <- ggplot(niche_diff_df, aes(x = percent_change, y = group)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(color = "#759FBD",cex = 4, alpha = 0.7, size = 3) +
  geom_point(aes(x = niche_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 9),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote('Change in niche size (%)'),
       size = "percent change")

p <- p1/p2

ggsave(p, file = "~/Desktop/test2.png", width = 5, height = 4)

p1 <- ggplot(area_diff_df, aes(x = -diff_km, y = group)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(aes(fill = abs(percent_change)), cex = 7, alpha = 0.7, shape = 21, size = 3) +
  geom_point(aes(x = -area_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  scale_fill_viridis_c(trans = "log10", option = "magma", limits = c(1,16000), breaks = c(1,10,100,1000,10000)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote(bold('Change in area size'~(km^2))))


p2 <- ggplot(niche_diff_df, aes(x = percent_change, y = group)) +
  geom_vline(aes(xintercept = 0), linetype = "solid", alpha = 0.5, color = "#4a4e4d") +
  geom_beeswarm(aes(fill = abs(percent_change)), cex = 7, alpha = 0.7, shape = 21, size =3) +
  geom_point(aes(x = niche_mean_diff), size = 3, shape = 22, 
             fill = "#CED3B1", color = "#B5BD89") +
  scale_fill_viridis_c(trans = "log10", option = "magma", limits = c(1,16000), breaks = c(1,10,100,1000,10000)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.key.width=unit(1,"cm"),
        legend.key.height =unit(0.3,"cm"),
        legend.title = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_text(size = 9, 
                                    face = "bold"),
        axis.ticks.x = element_line(color = "#4a4e4d")) +
  labs(x = bquote('Change in niche size (%)'),
       fill = "percent change") +
  guides(fill = guide_legend(title.position = "top"))


p <- p1/p2

ggsave(p, file = "~/Desktop/test3.png", width = 5, height = 4)


