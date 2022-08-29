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
  
  .dbPF <- '/gpfs/loomis/pi/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(cowplot)
  }))

d <- fread(paste0(.outPF,"dbbmm_size.csv")) %>%
  mutate(sg_norm = sg/cbg_area)

ggplot(data = subset(d, sg_norm < 0.0000002))+
  geom_histogram(aes(x = sg_norm, y = ..density.., 
                     group = as.factor(year), fill = as.factor(year)),
                 alpha = 0.5, position = "identity") +
  theme_cowplot() +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(y = "Density") 

ggplot(data = subset(d, sg_norm < 0.0000002))+
  geom_density(aes(x = sg_norm, y = ..density.., 
                     group = as.factor(year), color = as.factor(year)),
                 alpha = 0.5, position = "identity") +
  theme_cowplot() +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(y = "Density") 
