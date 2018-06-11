### Create series list

library("xml2")
library("XML")
library("rvest")
library("magrittr")


# Serie list creation with the crew splitted by category ------------------

series_list <- list()

# FOCUS ON EACH SERIE -----------------------------------------------------------------
for (r in seq_len(nrow(top_series))) { 
  
  serie_name <- top_series[r, "serie_name"]
  print(serie_name)
  
  list_serieyear <- as.list(strsplit(top_series[r, "serie_year"], split = ", ")[[1]]) # list of release years for each serie
  
  link_episodelist_peryear <- list() # list of links IMDb where we find all the episodes per year of release
  episodes_list_peryear <- list()
  
  # FOCUS ON EACH YEAR OF REALEASE FOR THIS SERIE -------------------------------------
  for (u in seq_along(list_serieyear)){ 
    
    year <- list_serieyear[[u]]
    print(year)
    
    link_episodelist_yeari <- strsplit(top_series[r, "serie_episodelist"], split='?', fixed=TRUE)[[1]][1] %>%
      paste0("?year=", year, collapse = "")
    link_episodelist_peryear[[u]] <- link_episodelist_yeari
    
    # FOCUS ON EACH EPISODE FOR THIS YEAR OF RELEASE ----------------------------------
    for (l in seq_along(link_episodelist_peryear)){ 
      
      page <- read_html(link_episodelist_peryear[[l]]) 
      episodes_nodes <- html_nodes(page, '.info') %>%
        as_list()
      
      episode_name <- c()
      episode_link <- c()
      
      for (t in seq_along(episodes_nodes)){
        episode_name <- c(episode_name, episodes_nodes[[t]]$strong$a[[1]])
        episode_link <- c(episode_link, attr(episodes_nodes[[t]]$strong$a, "href"))
      }
      
      episode_link <- paste0("http://www.imdb.com",episode_link)
      episode_link <- sapply(strsplit(episode_link, split='?', fixed=TRUE), 
                             function(x) (x[1])) %>%
        paste0("fullcredits?ref_=tt_ql_1")
      
      episode_name <- sapply(episode_name, 
                             function(x) (gsub(pattern = "\\#", replacement = "", x)))  %>% # some names = "Episode #1.1", delete #
        as.character()
      
      # GATHER THE NAME OF THE EPISODE, ITS YEAR OF RELEASE AND ITS FULL CREW LINK ----
      episodes_details_peryear <- data.frame(year = year,
                                             episode_name = episode_name,
                                             episode_link = episode_link,
                                             stringsAsFactors = FALSE)
    }
    
    # FOCUS ON EACH FULL CREW LINK ----------------------------------------------------
    for (e in seq_len(nrow(episodes_details_peryear))){

      print(episodes_details_peryear[e, "episode_link"])
      
      episode_page <- read_html(episodes_details_peryear[e, "episode_link"])
      episode_name <- episodes_details_peryear[e, "episode_name"]
      
      # GATHER ALL THE CREW NAMES FOR THIS EPISODE --------------------------------------------
      allcrew <- html_nodes(episode_page, '.name , .dataHeaderWithBorder')
      episode_allcrew <- html_text(allcrew)
      episode_allcrew <- gsub("[\n]", "", episode_allcrew)
      episode_allcrew <- trimws(episode_allcrew) #Remove white spaces 
      
      # SPLIT ALL THE CREW NAMES BY CATEGORY ------------------------------------------------
      categories <- html_nodes(episode_page, '.dataHeaderWithBorder')
      episode_categories <- html_text(categories)
      episode_categories <- gsub("[\n]", "", episode_categories) 
      episode_categories <- trimws(episode_categories) #Remove white spaces
      
      ## DIRECTORS --------------------------------------------------------------------------
      episode_directors <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Directed by", episode_allcrew[i])){
          j <- 1
          k <- i+1
          while (! grepl("Writing Credits", episode_allcrew[k]) && ! grepl("Cast", episode_allcrew[k])){ # 1 episode with no writing credits
            episode_directors <- c(episode_directors, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      if (length(episode_directors) == 0){
        episode_directors <- c("")
      }
      
      
      ## WRITERS --------------------------------------------------------------------------
      episode_writers <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Writing Credits", episode_allcrew[i])){
          j <- 1
          while (! grepl("Writing Credits", episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+2], episode_allcrew[k])){ #j+2 --> problem with grepl(cast)
            episode_writers <- c(episode_writers, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      if (length(episode_writers) == 0){
        episode_writers <- c("")
      } else {
        episode_writers <- head(episode_writers, -1) #Remove the last element (cast)
      }
      
      
      ## PRODUCERS --------------------------------------------------------------------------
      episode_producers <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Produced by", episode_allcrew[i])){
          if (grepl ("Produced by", episode_categories[length(episode_categories)])){ # Sometimes producers is last category
            first <- i+1
            for (p in first:length(episode_allcrew)) {
              episode_producers <- c(episode_producers, episode_allcrew[p])
            }
          } else {
            j <- 1
            while (! grepl(episode_allcrew[i], episode_categories[j])){
              j <- j+1
            }
            k <- i+1
            while (! grepl(episode_categories[j+1], episode_allcrew[k])){
              episode_producers <- c(episode_producers, episode_allcrew[k])
              k <- k+1
            }
          }
        }
      }
      if (length(episode_producers) == 0){
        episode_producers <- c("")
      }
      
      ## SOUND DEPT --------------------------------------------------------------------------
      episode_sound <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Sound Department", episode_allcrew[i])){
          if (grepl ("Sound Department", episode_categories[length(episode_categories)])){ # Sometimes sound dept is last category
            first <- i+1
            for (p in first:length(episode_allcrew)) {
              episode_sound <- c(episode_sound, episode_allcrew[p])
            }
          } else {
            j <- 1
            while (! grepl(episode_allcrew[i], episode_categories[j])){
              j <- j+1
            }
            k <- i+1
            while (! grepl(episode_categories[j+1], episode_allcrew[k])){
              episode_sound <- c(episode_sound, episode_allcrew[k])
              k <- k+1
            }
          }
        }
      }
      if (length(episode_sound) == 0){
        episode_sound <- c("")
      }
      
      ## MUSIC DEPT --------------------------------------------------------------------------
      episode_music <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Music by", episode_allcrew[i])){
          j <- 1
          while (! grepl(episode_allcrew[i], episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+1], episode_allcrew[k])){
            episode_music <- c(episode_music, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Music Department", episode_allcrew[i])){
          if (grepl ("Music Department", episode_categories[length(episode_categories)])){ # Sometimes music dept is last category
            first <- i+1
            for (p in first:length(episode_allcrew)) {
              episode_music <- c(episode_music, episode_allcrew[p])
            }
          } else {
            j <- 1
            while (! grepl(episode_allcrew[i], episode_categories[j])){
              j <- j+1
            }
            k <- i+1
            while (! grepl(episode_categories[j+1], episode_allcrew[k])){
              episode_music <- c(episode_music, episode_allcrew[k])
              k <- k+1
            }
          }
        }
      }
      if (length(episode_music) == 0){
        episode_music <- c("")
      }
      
      ## ART DEPT --------------------------------------------------------------------------
      episode_art <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Art Direction by", episode_allcrew[i])){
          j <- 1
          while (! grepl(episode_allcrew[i], episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+1], episode_allcrew[k])){
            episode_art <- c(episode_art, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Art Department", episode_allcrew[i])){
          if (grepl ("Art Department", episode_categories[length(episode_categories)])){ # Sometimes art dept is last category
            first <- i+1
            for (p in first:length(episode_allcrew)) {
              episode_art <- c(episode_art, episode_allcrew[p])
            }
          } else {
            j <- 1
            while (! grepl(episode_allcrew[i], episode_categories[j])){
              j <- j+1
            }
            k <- i+1
            while (! grepl(episode_categories[j+1], episode_allcrew[k])){
              episode_art <- c(episode_art, episode_allcrew[k])
              k <- k+1
            }
          }
        }
      }
      if (length(episode_art) == 0){
        episode_art <- c("")
      }
      
      ## MAKEUP DEPT --------------------------------------------------------------------------
      episode_makeup <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Makeup Department", episode_allcrew[i])){
          j <- 1
          while (! grepl(episode_allcrew[i], episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+1], episode_allcrew[k])){
            episode_makeup <- c(episode_makeup, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      if (length(episode_makeup) == 0){
        episode_makeup <- c("")
      }
      
      
      ## COSTUME DEPT --------------------------------------------------------------------------
      episode_costume <- c()
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Costume Design by", episode_allcrew[i])){
          j <- 1
          while (! grepl(episode_allcrew[i], episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+1], episode_allcrew[k])){
            episode_costume <- c(episode_costume, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      for (i in 1:(length(episode_allcrew)-1)){
        if (grepl("Costume and Wardrobe Department", episode_allcrew[i])){
          j <- 1
          while (! grepl(episode_allcrew[i], episode_categories[j])){
            j <- j+1
          }
          k <- i+1
          while (! grepl(episode_categories[j+1], episode_allcrew[k])){
            episode_costume <- c(episode_costume, episode_allcrew[k])
            k <- k+1
          }
        }
      }
      if (length(episode_costume) == 0){
        episode_costume <- c("")
      }
      
      
      ## EPISODE_INFO CONTAINS THE EPISODE CREW NAMES ORDERED BY CATEGORY -------------------
      episode_info <- list()
      episode_info$directors <- episode_directors
      episode_info$writers <- episode_writers
      episode_info$producers <- episode_producers
      episode_info$sound <- episode_sound
      episode_info$music <- episode_music
      episode_info$art <- episode_art
      episode_info$makeup <- episode_makeup
      episode_info$costume <- episode_costume
      
      ## EPISODES_LIST_PER_YEAR GATHERS THE INFORMATION FOR EVERY EPISODE OF THE SERIE-------
      ## SPLITTED BY YEAR OF RELEASE --------------------------------------------------------
      episodes_list_peryear[[year]][[episode_name]] <- episode_info
    }
    
    ## SERIES_LIST GATHERS THE INFORMATION FOR EVERY YEAR AND EVERY SERIE -------------------
    series_list[[serie_name]] <- episodes_list_peryear
  } 
}

