## Setting up data
library(tidyverse)
library(stringr)

streaming <- read_delim("data/streaming-platform-data.csv")

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
## THESE ARE THE MAIN HISTOGRAMS FOR THE SHINY APP
streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Netflix == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count", aes(fill = ..x..))+
  scale_fill_gradient(low="red",high="green")+
  labs(title="Netflix",
       x = "Rotten Tomatoes Rating",
       y = 'Movie count')

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Hulu == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count", aes(fill = ..x..))+
  scale_fill_gradient(low="red",high="green")+
  labs(title="Hulu",
       x = "Rotten Tomatoes Rating",
       y = 'Movie count')

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Prime Video` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count", aes(fill = ..x..))+
  scale_fill_gradient(low="red",high="green")+
  labs(title="Prime Video",
       x = "Rotten Tomatoes Rating",
       y = 'Movie count')

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Disney+` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_histogram(stat = "count", aes(fill = ..x..))+
  scale_fill_gradient(low="red",high="green")+
  labs(title="Disney+",
       x = "Rotten Tomatoes Rating",
       y = 'Movie count')


## Everything under this is extra stuff for possible other interactive elements

## Making accompanying boxplot to display with plot
## These would be toggled on and off and would display in one area on top of
## each other for easy comparison of medians, range, outliers, etc.
streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Netflix == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_boxplot(fill = "red")
  
streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Hulu == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_boxplot(fill = "green")

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Prime Video` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_boxplot(fill = "blue")

streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Disney+` == 1) %>% 
  ggplot(aes(x = modified_ratings))+
  geom_boxplot(fill = "darkblue")

## Variables
netflix_data <- streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Netflix == 1) %>% 
  select(modified_ratings)

hulu_data <- streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(Hulu == 1) %>% 
  select(modified_ratings)

primevideo_data <- streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Prime Video` == 1) %>% 
  select(modified_ratings)

disneyplus_data <- streaming %>% 
  filter(!is.na(modified_ratings)) %>% 
  filter(`Disney+` == 1) %>% 
  select(modified_ratings)

## Plot that depicts all, but idk how to make it so it could appear
## and not appear with a toggle like function for shiny
ggplot() +
  geom_boxplot(data = netflix_data, aes(x = modified_ratings), width=0.2, fill="red")+
  geom_boxplot(data = hulu_data, aes(x = modified_ratings),fill="green",
               width=0.2,
               position = position_nudge(y=0.2))+
  geom_boxplot(data = primevideo_data, aes(x = modified_ratings),fill="blue",
               width=0.2,
               position = position_nudge(y=0.4))+
  geom_boxplot(data = disneyplus_data, aes(x = modified_ratings),fill="darkblue",
               width=0.2,
               position = position_nudge(y=0.6))+
  labs(title="Streaming Services boxplots",
       subtitle = "Looking at the movie catalog in each service and their overall Rotten Tomatoes ratings.",
       x = "Rotten Tomatoes rating")

               