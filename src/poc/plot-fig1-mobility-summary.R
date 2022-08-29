if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/ucsb/repos/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(cowplot)
    library(lubridate)
  }))


d <- fread(paste0(.outPF,"events.csv"))

d <- d %>%
  filter(!is.na(safegraph_daily_count)) %>%
  filter(!is.na(ghm)) %>%
  mutate(year = lubridate::year(timestamp),
         sg_norm = safegraph_daily_count/cbg_area_m2) %>%
  select(individual_id, timestamp, year, sg_norm, ghm) %>%
  distinct(individual_id, timestamp, .keep_all = TRUE)




p1 <- ggplot(data = subset(d, year == 2019),
             aes(y = sg_norm, x = ghm)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_cowplot() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), 
                     limits = c(NA, 0.004),
                     labels = comma) +
  labs(x = "modification", y = "mobility")


p2 <- ggplot(data = subset(d, year == 2020),
             aes(y = sg_norm, x = ghm)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_cowplot() +
  theme(legend.position = c(0.1, 0.8),
        legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), 
                     limits = c(NA, 0.004),
                     labels = comma) +
  labs(x = "modification", y = "mobility")

i <- 1/2
pdf("~/Desktop/test.pdf", width = 12, height = 5)
ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = i, height = 1) +
  draw_plot(p2, x = i, y = 0, width = i, height = 1)

dev.off()