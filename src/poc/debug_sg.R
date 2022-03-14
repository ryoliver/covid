if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .test <- TRUE
  rd <- here::here
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .dbPF <- '/gpfs/loomis/project/jetz/sy522/covid-19_movement/processed_data/mosey_mod.db'
  .datPF <- file.path(.wd,'data/')
}

source(file.path(.wd,'/src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(data.table)
  }))


db <- dbConnect(RSQLite::SQLite(), .dbPF)


evt_df <- dbGetQuery(db,'SELECT * from event_cbg') %>%
  separate(timestamp, c("date",NA), sep = " ", remove = FALSE) %>%
  mutate("date_hour" = str_trunc(timestamp,13,"right","")) %>%
  collect()


daily_data <- fread(paste0(.wd,"/analysis/safegraph/counties-dates-2-10-22-reformatted/all_counties_cbg_day_SUM.csv")) %>%
  select(cbg,date,count) %>%
  rename(daily_count = count) %>%
  mutate(cbg = as.character(cbg),
         date = as.character(date))


evt_df_simple <- evt_df %>%
  select(cbg_2010, date) %>%
  rename(cbg = cbg_2010)

daily_data_simple <- daily_data %>%
  select(cbg,date)


diff <- setdiff(evt_df_simple,daily_data_simple)


daily_data %>%
  filter(cbg == "010359604001")

diff_cbgs <- setdiff(diff$cbg, daily_data$cbg)
nondiff_cbgs <- intersect(diff$cbg, daily_data$cbg)



evt_sg <- left_join(evt_df,daily_data, by = c("cbg_2010" = "cbg", "date" = "date")) 

evt_sg_filter <- evt_sg %>%
  filter(cbg_2010 %in% nondiff_cbgs)


evt_sg_filter %>%
  filter(is.na(daily_count)) %>%
  distinct


daily_data_distinct <- daily_data %>%
  distinct(cbg, date, .keep_all = TRUE) 

evt_sg_distinct <- left_join(evt_df,daily_data_distinct, by = c("cbg_2010" = "cbg", "date" = "date")) 



evt_sg %>%
  filter(is.na(daily_count)) %>%
  nrow()

evt_sg_distinct %>%
  filter(is.na(daily_count)) %>%
  nrow()

missing_cbgs





cbg <- tbl(db, "event_cbg") %>%  collect()

sg <- tbl(db, "event_sg") %>%  collect()

census <- tbl(db, "event_census") %>%  collect()



daily_data <- fread(paste0(.wd,"/analysis/safegraph/counties-dates-2-10-22-reformatted/all_counties_cbg_day_SUM.csv")) %>%
  select(cbg,date,count) %>%
  rename(daily_count = count) %>%
  mutate(cbg = as.character(cbg),
         date = as.character(date))

evt_df %>%
  filter(is.na(cbg_2010)) %>%
  nrow()

evt_df %>%
  filter(is.na(date)) %>%
  nrow()

daily_data %>%
  filter(is.na(cbg)) %>%
  nrow()

daily_data %>%
  filter(is.na(date)) %>%
  nrow()

non_unique <- daily_data %>%
  count(cbg,date) %>%
  filter(n > 1)

daily_data_distinct <- daily_data %>%
  distinct(cbg, date, .keep_all = TRUE) 


daily_data %>%
  filter(cbg == "100010401001") %>%
  filter(date == "2019-02-05")
  



evt_sg_new <- left_join(evt_df,daily_data_distinct, by = c("cbg_2010" = "cbg", "date" = "date")) 

evt_sg_new %>%
  filter(is.na(daily_count)) %>%
  nrow()

missing <- setdiff(evt_df$cbg_2010,daily_data$cbg)

test <- evt_sg_new %>%
  filter(!cbg_2010 %in% missing)

test %>%
  filter(is.na(daily_count)) %>%
  nrow()


missing_dates <- setdiff(test$date, daily_data$date)

test2 <- test %>%
  filter(!date %in% missing_dates)


test2 %>%
  filter(is.na(daily_count)) %>%
  nrow()


test2 %>% 
  distinct(cbg_2010,date, .keep_all = TRUE) %>%
  filter(is.na(daily_count)) %>%
  select(event_id, timestamp, date, cbg_2010, daily_count) %>%
  nrow()

daily_data %>%
  filter(cbg == "410379601002") %>%
  filter(date == "2019-03-06")


cbg %>%
  nrow()

sg %>%
  nrow()

census %>%
  nrow()

census %>%
  nrow()

nrow(census) - nrow(cbg)
nrow(sg) - nrow(cbg)


colnames(sg)
colnames(census)

cbg %>%
  filter(is.na(cbg_2010)) %>%
  nrow()

sg %>%
  filter(is.na(cbg_2010)) %>%
  nrow()

sg %>%
  filter(is.na(daily_count)) %>%
  nrow()

census %>%
  filter(is.na(cbg_2010)) %>%
  nrow()

census %>%
  filter(is.na(total_population_2019)) %>%
  nrow()

missing_sg <- sg %>%
  filter(is.na(daily_count)) 

missing_census <- census %>%
  filter(is.na(total_population_2019)) 

missing_sg %>%
  filter(is.na(timestamp)) %>%
  nrow()

evt_df %>%
  filter(is.na(date)) %>%
  nrow()


daily_data %>%
  filter(is.na(cbg)) %>%
  nrow()

evt_sg_new %>%
  filter(is.na(daily_count)) %>%
  nrow()


