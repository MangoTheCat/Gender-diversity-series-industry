### Percentage of women in categories

number_male_directors <- 0
number_female_directors <- 0
number_male_writers <- 0
number_female_writers <- 0
number_male_producers <- 0
number_female_producers <- 0
number_male_sound <- 0
number_female_sound <- 0
number_male_music <- 0
number_female_music <- 0
number_male_art <- 0
number_female_art <- 0
number_male_makeup <- 0
number_female_makeup <- 0
number_male_costume <- 0
number_female_costume <- 0


for (s in seq_along(series_list)){
  if ("2014" %in% names(series_list[[s]])){
    
    # Directors --------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_directors <- as.integer(series_list[[s]]$`2014`[[i]]$directors_gender$male)
      serie_f_directors <- as.integer(series_list[[s]]$`2014`[[i]]$directors_gender$female)
      
      number_male_directors <- number_male_directors + serie_m_directors
      number_female_directors <- number_female_directors + serie_f_directors
    }
    
    # Writers ----------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_writers <- as.integer(series_list[[s]]$`2014`[[i]]$writers_gender$male)
      serie_f_writers <- as.integer(series_list[[s]]$`2014`[[i]]$writers_gender$female)
      
      number_male_writers <- number_male_writers + serie_m_writers
      number_female_writers <- number_female_writers + serie_f_writers
    }
    
    # Producers --------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_producers <- as.integer(series_list[[s]]$`2014`[[i]]$producers_gender$male)
      serie_f_producers <- as.integer(series_list[[s]]$`2014`[[i]]$producers_gender$female)
      
      number_male_producers <- number_male_producers + serie_m_producers
      number_female_producers <- number_female_producers + serie_f_producers
    }
    
    # Sound teams -------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_sound <- as.integer(series_list[[s]]$`2014`[[i]]$sound_gender$male)
      serie_f_sound <- as.integer(series_list[[s]]$`2014`[[i]]$sound_gender$female)
      
      number_male_sound <- number_male_sound + serie_m_sound
      number_female_sound <- number_female_sound + serie_f_sound
    }
    
    # Music teams --------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_music <- as.integer(series_list[[s]]$`2014`[[i]]$music_gender$male)
      serie_f_music <- as.integer(series_list[[s]]$`2014`[[i]]$music_gender$female)
      
      number_male_music <- number_male_music + serie_m_music
      number_female_music <- number_female_music + serie_f_music
    }
    
    # Art teams ----------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_art <- as.integer(series_list[[s]]$`2014`[[i]]$art_gender$male)
      serie_f_art <- as.integer(series_list[[s]]$`2014`[[i]]$art_gender$female)
      
      number_male_art <- number_male_art + serie_m_art
      number_female_art <- number_female_art + serie_f_art
    }
    
    # Makeup teams -------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_makeup <- as.integer(series_list[[s]]$`2014`[[i]]$makeup_gender$male)
      serie_f_makeup <- as.integer(series_list[[s]]$`2014`[[i]]$makeup_gender$female)
      
      number_male_makeup <- number_male_makeup + serie_m_makeup
      number_female_makeup <- number_female_makeup + serie_f_makeup
    }
    
    # Costume teams ------------------------------------------------------------
    
    for (i in seq_along(series_list[[s]]$`2014`)){
      serie_m_costume <- as.integer(series_list[[s]]$`2014`[[i]]$costume_gender$male)
      serie_f_costume <- as.integer(series_list[[s]]$`2014`[[i]]$costume_gender$female)
      
      number_male_costume <- number_male_costume + serie_m_costume
      number_female_costume <- number_female_costume + serie_f_costume
    }
  }
} 

percentage_women_directors <- 100 * number_female_directors / (number_male_directors+number_female_directors) 
percentage_women_writers <- 100 * number_female_writers / (number_male_writers+number_female_writers) 
percentage_women_producers <- 100 * number_female_producers / (number_male_producers+number_female_producers) 
percentage_women_sound <- 100 * number_female_sound / (number_male_sound+number_female_sound) 
percentage_women_music <- 100 * number_female_music / (number_male_music+number_female_music) 
percentage_women_art <- 100 * number_female_art / (number_male_art+number_female_art) 
percentage_women_makeup <- 100 * number_female_makeup / (number_male_makeup+number_female_makeup) 
percentage_women_costume <- 100 * number_female_costume / (number_male_costume+number_female_costume) 

# Creating a dataset percentages_2014  ----------------------------------------

percentages_2014 <- data.frame(year = 2014, 
                               women_directors = percentage_women_directors, 
                               women_writers = percentage_women_writers, 
                               women_producers = percentage_women_producers, 
                               women_sound = percentage_women_sound,
                               women_music = percentage_women_music,
                               women_art = percentage_women_art,
                               women_makeup = percentage_women_makeup,
                               women_costume = percentage_women_costume, 
                               stringsAsFactors = FALSE)



