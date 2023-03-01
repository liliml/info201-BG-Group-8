## Setting up data
library(tidyverse)
library(stringr)


streaming <- read_delim("data/streaming-platform-data.csv")

## Question: 
## Typical rating of movies on each streaming platform 
## visualized with a histogram

## Plot description and analysis:
# These histograms contain data of each streaming service's movie catalog in 2021 to early 2022.
# The histogram's bins on the x axis represent the Rotten Tomatoes rating given to each movie.
# The height of each bin corresponds to how many movies have gotten that rating.
# The color scale on the bins are another way to depict the ratings. Green corresponds to higher ratings while red are lower ratings.

# Comparing the four streaming service histograms, all streaming services have a wide variety and mostly follow a normal distribution shape.
# All histograms all have some outlier data points where there are a handful of very low rated movies in their catalogs.
# This is one area where start up companies making a streaming service that is competitive with the current market could improve on.
# Netflix seems to have the widest range of ratings in their movie catalog, while Hulu seem to have the most narrow.
# An explanation for this could be that the size of Netflix's catalog is much larger than Hulu, almost having 1500 more movies.
# Hulu appears to have the median with the highest rating as it's center is around the 60/100 Rotten Tomatoes rating. 
# The service with the lowest median rating is Prime Video, with it's histogram looking to be skewed to the right. 
# Prime Video also seems to have the lowest percentage of movies in its catalog that are highly rated as it's bins are the least green shaded and lean more towards red/yellow.
# There is a trend with larger movie catalogs and greater diversity of ratings, but moreso leaning towards the lower ratings spectrum while the smaller catalogs lean more towards the higher spectrum even with a more narrow ratings range.
# It is possible that Hulu and Disney+ are more selective with what movies are on their catalog, hence why there are less but are of higher Rotten Tomatoes ratings.
# For start up companies, it's important to recognize the balance between having a large and diverse catalog of movies and a smaller scope that are of higher quality.

## Table that shows how many movies are in each streaming service
Services <- c(names(streaming[7:10]))
Total_Movies <- c(sum(streaming[,7] == 1), sum(streaming[,8] == 1), sum(streaming[,9] == 1), sum(streaming[,10] == 1))

streaming_total_movies <- data.frame(Services,Total_Movies)
streaming_total_movies %>% 
  knitr::kable()

## Table Description:
# This table shows how many movies were available on each streaming service in 2021 to early 2022.

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

               