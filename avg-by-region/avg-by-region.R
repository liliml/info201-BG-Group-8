## import library
# library(tidyverse)

## clean data
## run once on setup
# food <- read_delim("my_data/wfp_market_food_prices.csv")
# clean_food <- food %>% 
#   mutate(adm0_id = NULL, adm1_id = NULL, mkt_id = NULL, 
#          cm_id = NULL, cur_id = NULL, pt_id = NULL, um_id = NULL, 
#          mp_commoditysource = NULL) %>% 
#   rename(country = adm0_name, region = adm1_name, market = mkt_name,
#          commodity = cm_name, currency = cur_name, market_type = pt_name,
#          unit = um_name, month = mp_month, year = mp_year, price = mp_price)
# write_delim(clean_food, "my_data/clean_data.csv")
# food <- clean_food

## variable information
# country
# region
# market
# commodity
# market_type
# unit
# month
# year
# price

## data exploration

# what are the different countries in this data set?
food %>% 
  pull(country) %>% 
  unique()

# how many regions are there data for in each country?
food %>% 
  select(country, region) %>% 
  group_by(country) %>% 
  summarize(regions = n_distinct(region))