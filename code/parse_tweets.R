
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# input is monthly folder

library(tidyverse)
library(furrr)
library(tweetWrangleR)

# month_folder <- args[1]
month_folder <- "/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/corona_data/"

# patterns_folder <- args[2]
patterns_folder <- "/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/patterns/"

dir.create(file.path(month_folder, "processed_files/rds_files/"), recursive = T, showWarnings = F)
dir.create(file.path(month_folder, "processed_files/csv_files/"), recursive=T, showWarnings = F)

month_folder_output_rds <- paste0(month_folder, "processed_files/rds_files/")
month_folder_output_csv <- paste0(month_folder, "processed_files/csv_files/")

month_folder_output_rds
month_folder_output_csv

file_paths <- list.files(month_folder, pattern = "*collection.json", recursive=T)
file_paths

plan(multiprocess(workers = 4)) # run on 20 cores
options(future.globals.maxSize= 20000*1024^2)

pattern_asian <- jsonlite::read_json( paste0(patterns_folder,"pattern_asian.json")) %>%
  paste0(collapse = "|") %>% 
  stringr::str_to_lower()

pattern_asian

pattern_jewish <- jsonlite::read_json(paste0(patterns_folder,"pattern_jewish.json"))%>% 
  paste0(collapse = "|")%>% 
  stringr::str_to_lower()

pattern_jewish 

pattern_lgbt <- jsonlite::read_json(paste0(patterns_folder,"pattern_lgbt.json"))%>%
  paste0(collapse = "|")%>% 
  stringr::str_to_lower()
pattern_lgbt


pattern_muslim <- jsonlite::read_json(paste0(patterns_folder,"pattern_muslim.json"))%>% 
  paste0(collapse = "|")%>% 
  stringr::str_to_lower()
pattern_muslim


paste0(month_folder, file_paths[1]) # example file path

parse_filter <- function(json_path, export_as_rds=FALSE, export_as_csv=TRUE){
  
  json <- rtweet::parse_stream(paste0(month_folder, json_path)) %>% 
    select( -c( place_url, place_type, bbox_coords, symbols, display_text_width, urls_url, urls_t.co, media_url, media_t.co, media_expanded_url, ext_media_t.co,  profile_banner_url,	profile_background_url,	profile_image_url,profile_url,	profile_expanded_url, protected, account_lang, ext_media_expanded_url,	ext_media_type, coords_coords, status_url)) %>%
    filter(lang=="en") %>% 
    select(text, status_id, everything()) %>% 
    mutate(
      pattern_muslim_bool= ifelse(stringr::str_detect(text, regex(pattern_muslim, ignore_case = TRUE)), TRUE, FALSE),
      pattern_muslim_match = stringr::str_match_all(text, regex(pattern_muslim, ignore_case = TRUE))
    ) %>% 
    mutate(
      pattern_jewish_bool= ifelse(stringr::str_detect(text, regex(pattern_jewish, ignore_case = TRUE)), TRUE, FALSE),
      pattern_jewish_match = stringr::str_match_all(text, regex(pattern_jewish, ignore_case = TRUE))
    ) %>% 
    mutate(
      pattern_lgbt_bool= ifelse(stringr::str_detect(text, regex(pattern_lgbt, ignore_case = TRUE)), TRUE, FALSE),
      pattern_lgbt_match = stringr::str_match_all(text, regex(pattern_lgbt, ignore_case = TRUE))
    ) %>% 
    mutate(
      pattern_asian_bool= ifelse(stringr::str_detect(text, regex(pattern_asian, ignore_case = TRUE)), TRUE, FALSE),
      pattern_asian_match = stringr::str_match_all(text, regex(pattern_asian, ignore_case = TRUE))
    ) %>% 
    mutate(hourly_nrow=(nrow(.))) %>%
    mutate(day_tweet=lubridate::as_date(created_at)) %>%
    mutate(month_tweet=as.integer(lubridate::month(created_at))) %>% 
    mutate(year_tweet=as.integer(lubridate::year(created_at))) %>% 
    mutate(is_reply=ifelse(is.na(reply_to_status_id),FALSE, TRUE)) %>% 
    mutate_at(vars(ends_with("verified")), ~as.logical(.))

  if (export_as_rds == TRUE) {
    rds_path= paste0(stringr::str_sub(paste0(month_folder_output_rds, json_path), end = -6), ".rds")
    print(paste("exporting parsed chunk as RDS to: ", rds_path))
    readr::write_rds(json, path = rds_path)
  }
  else {
    return(json)
  }
  
  if (export_as_csv ==TRUE){
    csv_path = paste0(stringr::str_sub(paste0(month_folder_output_csv, json_path), end = -6), ".csv")
    print(paste("exporting parsed chunk as csv to: ", rds_path))
    json %>%
      rtweet::save_as_csv(csv_path)
  }
  else {
    return(json)
  }
}

print("without rds")
tictoc::tic()
future_map(file_paths[1:10], parse_filter, export_as_csv=TRUE, export_as_rds=TRUE)
tictoc::toc()

# sys info
Sys.info()

# session info
sessionInfo()