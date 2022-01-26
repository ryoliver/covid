#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Template
Usage:
script_template <taxa> <dat> <out> 
script_template (-h | --help)
Parameters:
  dat: path to input csv file. 
  out: path to output directory.
Options:
-h --help     Show this screen.
-v --version     Show version.
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid/'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'analysis/')
  .outPF <- file.path(.wd,'analysis/figures/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  .datPF <- makePath(ag$dat)
  .outPF <- makePath(ag$out)
}

#---- Initialize Environment ----#

source(rd('src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

daily_dat <- fread(paste0(.datPF,"safegraph/counties-dates-1-20-22-reformatted/all_counties_cbg_day_SUM.csv"))


p <- ggplot(data = daily_dat) +
  stat_summary(aes(x = date,y = count, group = county_id), geom = "point", fun = mean) +
  stat_summary(aes(x = date,y = count, group = county_id), geom = "line", fun = mean) +
  stat_summary(aes(x = date,y = count, group = county_id), geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.1) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.y  = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  labs(x = "", y = "device counts") 

p <- ggplot(data = subset(daily_dat, county_id == 1001)) +
  geom_line(aes(x = date,y = count, group = cbg)) +
  geom_point(aes(x = date,y = count, group = cbg)) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9),
        axis.text.y  = element_text(size = 8),
        axis.text.x = element_text(size = 8)) +
  labs(x = "", y = "device counts") 

pdf("~/Desktop/test.pdf")
ggdraw() +
  draw_plot(p)
dev.off()