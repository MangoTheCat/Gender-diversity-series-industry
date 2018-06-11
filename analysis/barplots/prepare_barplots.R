### Data frame manipulation before plot

library(magrittr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(dplyr)

# Import our movies dataset
percentages_movies <- read.csv("percentages_movies.csv") 
percentages_movies <- percentages_movies[ , -1]

# Change colnames for movie and serie dataframes
colnames(percentages_movies) <- c("year", "directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")
colnames(percentages) <- c("year", "directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")

# From wide to long dataframes
# Movies dataframe
percentages_movies_long <- percentages_movies %>%
  gather(key = category, value = percentage, -year)
# Series dataframe
percentages_long <- percentages %>%
  gather(key = category, value = percentage, -year)

# Add a column to these dataframes: movie or film ?
percentages_movies_long$industry <- rep("Film industry", 88)
percentages_long$industry <- rep("Series industry", 96)

# Combine these 2 long dataframes
percentages_movies_series <- bind_rows(percentages_long, percentages_movies_long)
