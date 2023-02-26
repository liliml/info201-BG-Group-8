## Setting up data
library(tidyverse)
library(stringr)

streaming <- read_delim("data/streaming-platform-data.csv")
View(streaming)

## Question: 
## Typical rating of movies on each streaming platform 
## visualized with a histogram

## Problem: For some reason, the values are not distinct in the rating
## According to n_distinct there is only 1 unique value
## The ratings are not a numeric string? Would have to convert it somehow
streaming %>% 
  summarize(unique_ratings = n_distinct("Rotten Tomatoes"))

## Check what type of variable it is
typeof(streaming$`Rotten Tomatoes`)

## Converting to numeric
streaming$modified_ratings <- as.numeric(
  str_remove(streaming$`Rotten Tomatoes`, "/100"))

## Checking if it did it right
streaming %>% 
  arrange(desc(modified_ratings)) %>% 
  select(Title,"Rotten Tomatoes", modified_ratings)

## Making plots
streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Netflix == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count")

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Hulu == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count")

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Prime Video` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count")

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Disney+` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count")
