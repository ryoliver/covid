if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .outPF <- file.path(.wd,'data/example-data/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .outPF <- file.path(.wd,'data/example-data/')
}

message("start safegraph annotation")
source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

stdtb <- tbl(db,'study')
indtb <- tbl(db,'individual')
evttb <- tbl(db,'event')


ids <- c(1967914051,1967914129,160484522,160484522)

select_individual <- function(id){
  e <- evttb %>%
    filter(individual_id == id)
  
  fwrite(e, paste0(.datPF,"event-data-",id,".csv"))
  
  i <- indtb %>%
    filter(individual_id == id)
  
  fwrite(i, paste0(.datPF,"individual-data-",id,".csv"))
}

for(i in 1:length(ids)){
  select_individual(ids[i])
}
