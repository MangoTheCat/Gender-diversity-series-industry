### gather all our Percentages datasets into one dataset

library(dplyr)

percentages <- bind_rows(percentages_2018, percentages_2017, percentages_2016, 
                         percentages_2015, percentages_2014, percentages_2013,
                         percentages_2012, percentages_2011, percentages_2010,
                         percentages_2009, percentages_2008, percentages_2007)
