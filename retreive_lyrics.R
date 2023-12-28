readRenviron(".Renviron")
library(tidyverse)
library(tidytext)
library(tidymodels)
# the CRAN version of the package is not working properly
#remotes::install_github("https://github.com/giovanni-cutri/geniusr")
library(geniusr)
library(tokenizers)




category_songs <- readRDS("rawdata/small_sample.RDS") %>% 
  distinct()
category_songs %>% 
  summarise(n = n(), .by = category)
test_sample <- slice_sample(category_songs, n = 100, by = category ) %>% 
  select(track.name, track.artist, category, track.duration_ms)

library(foreach)

lyrics <- foreach(i = 1:nrow(test_sample), .combine = "rbind", .errorhandling = "remove") %do% {
  

  
  song_tmp <- test_sample[i,]
  
  lyrics_tmp <- get_lyrics_search(artist_name = song_tmp$track.artist,
                                  song_title = song_tmp$track.name) %>% 
    select(line) %>% 
    reduce(line) %>% 
    paste0(collapse = "\n ")
  
  
  tokens <- tibble(tokens = tokenize_words(lyrics_tmp, simplify = TRUE)) %>% 
    nest()
  
  n_words <- unnest(tokens) %>% 
    nrow()
  
  sentiment <- tokens %>% unnest(data)  %>% 
    rename(word = tokens) %>% 
    inner_join(filter(get_sentiments("nrc"),  sentiment %in% c("positive", "negative")),
               by = "word", relationship =  'many-to-many') %>% 
    group_by(sentiment) %>% 
    count() %>% 
    pivot_wider(names_from = sentiment, values_from = n) %>% 
    mutate(total = negative + positive,
           sentiment = positive/total)
  
  output <- tibble(track.artist=song_tmp$track.artist,
                   track.name = song_tmp$track.name,
                   lyrics = lyrics_tmp,
                   tokens = tokens$data,
                   n_words =  n_words) %>% 
    cbind(sentiment)
  print(i/nrow(test_sample))
  return(output)
}


saveRDS(lyrics, "rawdata/sample_lyrics.RDS")
lyrics_unique <- distinct(lyrics)

final <- test_sample %>% 
  left_join(lyrics_unique, by = c("track.artist", "track.name"))%>% 
  drop_na(sentiment) %>% 
  mutate(track.duration_min = track.duration_ms/60000,
         words_per_minute = n_words/track.duration_min)

saveRDS(final, "rawdata/example_lyrics.RDS")



rap <- final %>% 
  filter(category == "Hip-Hop")

others <- final %>% 
  filter(category %in% c("Rock", "Metal", "Pop", "Indie")) %>% 
  slice_sample(n = 50)

saveRDS(rbind(rap, others), "cleandata/rap_genre_example.RDS")




