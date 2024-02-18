library(tidyverse)

lyrics <- readRDS("rawdata/category_sample_lyrics.RDS")


lyrics_unique <- distinct(lyrics)

test_sample <- readRDS("rawdata/category_songs.RDS")
final <- test_sample %>% 
     left_join(lyrics_unique, by = c("track.artist", "track.name"))%>% 
     drop_na(sentiment) %>% 
     mutate(track.duration_min = track.duration_ms/60000,
            words_per_minute = n_words/track.duration_min) %>% 
     mutate(release_year = year(as_date(track.album.release_date))) %>%
     select(track.name, track.artist, category, track.duration_min, words_per_minute, sentiment, n_words, lyrics) 

saveRDS(final, "cleandata/example_data_lyrics_with_year.RDS")

