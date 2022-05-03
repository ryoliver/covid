if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/Yale/projects/covid'
  .test <- TRUE
  rd <- here::here
  .datPF <- file.path(.wd,'analysis/event-annotations/')
  .outPF <- file.path(.wd,'analysis/figures/')
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .datPF <- file.path(.wd,'data/example-data/')
  .outPF <- file.path(.wd,'analysis/figures/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(lubridate)
    library(cowplot)
  }))

sg <- fread(paste0(.datPF,"event_sg.csv")) %>%
  mutate("year" = lubridate::year(timestamp))
ghm <- fread(paste0(.datPF,"event_ghm.csv"))

evt <- left_join(ghm, sg, by = "event_id") %>%
  select(event_id, year, ghm, safegraph_daily_count, cbg_area_m2) %>%
  mutate(sg = safegraph_daily_count/cbg_area_m2) %>%
  filter(!is.na(sg))


p <- ggplot(data = evt) +
  geom_point(aes(x = ghm, y = sg, group = as.factor(year), color = as.factor(year)), alpha = 0.5) +
  scale_color_manual(values = c("#FF6542","#86BBD8")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  theme_cowplot() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = ) +
  labs(x = "human modification (0-1)", y = "human mobility") +
  facet_wrap(~ as.factor(year))


pdf(paste0(.outPF,"sg-ghm.pdf"),width = 10, height = 4)
ggdraw() +
  draw_plot(p)
dev.off()




