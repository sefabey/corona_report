library(tidyverse)
library(xts)
# old <- fs::dir_ls("/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/corona_data/processed_files/csv_files/olde", type = "file") %>% 
#   map_df(rtweet::read_twitter_csv)
# 
# new <- fs::dir_ls("/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/corona_data/processed_files/csv_files/new", type = "file") %>% 
#   map_df(rtweet::read_twitter_csv)
# 
# 
# 
# data <- read_csv("/Users/sefaozalp/Downloads/corona_march_2020_hourly_files_aggregated_brief.csv")




data


# setdiff(names(new), names(old))

data %>% 
  filter_at(vars(contains("bool")), any_vars(.==TRUE)) 
data %>% 
  select(contains("bool"))

data <- rtweet::read_twitter_csv("/Users/sefaozalp/Downloads/2020-04-30_hour2100_corona_collection_nd.csv")


to_xts <- data %>% 
  # filter_at(vars(contains("bool")), any_vars(.==TRUE)) %>%
  rtweet::ts_data(by="hours") %>% 
  rename(english_lang_tweet_count= n) %>% 
  left_join( data %>% 
              filter(pattern_asian_bool, ) %>% 
              rtweet::ts_data(by="hours") %>% 
              rename(asian_tweet_count= n),
             by = "time") %>% 
  left_join( data %>% 
               filter(pattern_jewish_bool, ) %>% 
               rtweet::ts_data(by="hours") %>% 
               rename(jewish_tweet_count= n),
             by = "time") %>% 
  left_join( data %>% 
               filter(pattern_lgbt_bool, ) %>% 
               rtweet::ts_data(by="hours") %>% 
               rename(lgbt_tweet_count= n),
             by = "time") %>% 
  left_join( data %>% 
               filter(pattern_muslim_bool, ) %>% 
               rtweet::ts_data(by="hours") %>% 
               rename(lgbt_muslim_count= n),
             by = "time") %>% 
  mutate_if(is.integer,  ~replace_na(., 0)) 

a <- as.xts(to_xts[-1], order.by = (to_xts$time))

a$total_tweet_count= as.integer(a$total_tweet_count)
a

  


library(dygraphs)
library(tidyverse)

april <- read_rds("/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/corona_data/timeseries/april_2020_aggregated_timeseries_xts.rds")

april <- april[-1]

march <- read_rds("/Users/sefaozalp/Documents/Work/cyberhate/corona/corona_report/data/corona_data/timeseries/march_2020_aggregated_timeseries_xts.rds")

merged_data <- zoo::rbind.zoo(march,april) 

merged_data %>% 
  # head(100) %>% 
  dygraph(main = "Tweets Matching The Identity Pattern") %>% 
  dyRangeSelector() %>% 
  dyOptions(drawPoints = TRUE, pointSize = 1) %>% 
  dyRangeSelector(height = 20) %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)


merged_data[,-c(1,2)] %>% 
  # head(100) %>% 
  dygraph(main = "Tweets Matching The Identity Pattern") %>% 
  dyRangeSelector() %>% 
  dyOptions(drawPoints = TRUE, pointSize = 1) %>% 
  dyRangeSelector(height = 20) %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)

