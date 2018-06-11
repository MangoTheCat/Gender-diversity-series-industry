### Evolution plot

library(magrittr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)

# year as date
percentages_movies_series_ymd <- percentages_movies_series %>%
  subset(year != 2018)
percentages_movies_series_ymd$year <- ymd(percentages_movies_series_ymd$year, truncated = 2L) 

# Data visualisation
evolution <- ggplot(percentages_movies_series_ymd, aes(x = year,
                                                       y = percentage,
                                                       group = category,
                                                       colour = category)) +
  geom_line(size = 2) +
  facet_wrap(~industry) +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) + # center the title
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_manual(values = brewer.pal(8, "Set1")) +
  labs(title = "Percentages of women from 2007 to 2017\n Film industry VS serie industry",
       x = "",
       y = "Percentages")