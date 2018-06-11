### Create database

library("xml2")
library("XML")
library("rvest")
library("magrittr")

### Web scraping
### Database creation

# IMDb 100 Most Popular TV Shows -------------

url <- "https://www.imdb.com/chart/tvmeter?sort=us,desc&mode=simple&page=1"
page <- read_html(url)

serie_nodes <- html_nodes(page, '.titleColumn') %>%
  as_list()

serie_name <- c()
serie_link <- c()
serie_year <- c()
for (i in seq_along(serie_nodes)){
  serie_name <- c(serie_name, serie_nodes[[i]]$a[[1]])
  serie_link <- c(serie_link, attr(serie_nodes[[i]]$a, "href"))
  serie_year <- c(serie_year, serie_nodes[[i]]$span[[1]])
}

serie_link <- paste0("http://www.imdb.com",serie_link)
serie_year <- gsub("[()]", "", serie_year)


serie_episodelist <- sapply(strsplit(serie_link, split='?', fixed=TRUE),
                            function(x) (x[1])) %>%
  paste0("episodes?ref_=tt_eps_yr_mr")


# Create dataframe ----------------------------------------------
top_series <- data.frame(serie_name, serie_year, serie_episodelist, stringsAsFactors = FALSE)


# Serie_year is the date of 1rst release but we need the years of release of all the episodes
# I did not manage to gather it with web scraping.
# I added it manually

