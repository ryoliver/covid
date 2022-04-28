#---- Input Parameters ----#
if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'analysis/')
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  .datPF <- file.path(.wd,'analysis/')
}

message("start safegraph data test")
source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
  }))


reformatted_files_daily <- list.files(paste0(.datPF,"safegraph/counties-dates-2-10-22-reformatted/daily-data"), full.names = TRUE)

old <- data.table::rbindlist(lapply(reformatted_files_daily, data.table::fread, colClasses = "character"),use.names = TRUE) %>%
  select(cbg,date,count) 


list <- fread(paste0(.datPF,"safegraph-summary/census-block-group-list.csv"), colClasses = "character")

missing <- data.frame(CensusBlockGroup = setdiff(list$CensusBlockGroup,old$cbg))

fwrite(missing, paste0(.datPF,"safegraph-summary/missing-cbgs.csv"))

if (1 ==2){
reformatted_files_daily <- list.files(paste0(.datPF,"safegraph/counties-dates-4-26-22-reformatted/daily-data"), full.names = TRUE)

new <- data.table::rbindlist(lapply(reformatted_files_daily, data.table::fread, colClasses = "character"),use.names = TRUE) %>%
  select(cbg,date,count) 

message("nrow old:")
nrow(old)
message("nrow new:")
nrow(new)

message("n cbg old:")
n_distinct(old$cbg)
message("n cbg new")
n_distinct(new$cbg)

old_summary <- old %>%
  group_by(cbg) %>%
  summarize("n_old" = n_distinct(date))

new_summary <- new %>%
  group_by(cbg) %>%
  summarize("n_new" = n_distinct(date))

summary <- left_join(old_summary, new_summary, by = "cbg") %>%
  mutate(diff = n_old - n_new)

message("sum:")
sum(summary$diff)


length(setdiff(list$CensusBlockGroup,new$cbg))

}