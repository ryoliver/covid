if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'src/poc/')
  .outPF <- file.path(.wd,"src/workflow/")
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .datPF <- file.path(.wd,'src/poc/')
  .outPF <- file.path(.wd,"src/workflow/")
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
  }))



files <- list.files(paste0(.wd,'/analysis/ssf-background-pts/individual-files'),full.names = TRUE)

n_total <- length(files)


n <- 50
n_events <- floor(n_total/n)

start_ix <- seq(from = 1, to = n*n_events, by = n_events)
end_ix <- seq(from = n_events, to = n_total, by = n_events)


joblist <- data.frame("string" = rep(paste0(" module load miniconda; conda activate move; Rscript ",.datPF,"annotate-background-sg-ghm.R "), times = n),
                      "arg1" = start_ix,
                      "arg2" = end_ix,
                      "arg3" = seq(from = 1, to = n, by = 1))

write.table(joblist,paste0(.outPF,"annotation-joblist.txt"),col.names = FALSE,row.names = FALSE,quote = FALSE)
