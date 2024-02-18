readRenviron(".Renviron")
library(tidyverse)
library(tidytext)
library(tidymodels)
# the CRAN version of the package is not working properly
#remotes::install_github("https://github.com/giovanni-cutri/geniusr")
library(geniusr)
library(tokenizers)




category_songs <- readRDS("rawdata/category_songs.RDS") %>% 
  distinct()
category_songs %>% 
  summarise(n = n(), .by = category)
test_sample <- category_songs %>% 
  select(track.name, track.artist, category, track.duration_ms)

test_sample_songs <-
  test_sample %>% 
  distinct(track.name, track.artist)
library(foreach)

cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)

lyrics <- foreach(i = 1:nrow(test_sample_songs), .combine = "rbind", 
                  .errorhandling = "remove",
                  .packages = c("tidyverse", "geniusr", "tokenizers", "tidytext")) %dopar% {
  

  
  song_tmp <- test_sample_songs[i,]
  
  lyrics_tmp <- get_lyrics_search(artist_name = song_tmp$track.artist,
                                  song_title = song_tmp$track.name) %>% 
    select(line) %>% 
    reduce(line) %>% 
    paste0(collapse = "\n ")
  
  
  tokens <- tibble(tokens = tokenize_words(lyrics_tmp, simplify = TRUE)) %>% 
    nest()
  
  n_words <- unnest(tokens, cols = c(data)) %>% 
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
  Sys.sleep(1)
  return(output)
}


saveRDS(lyrics, "rawdata/category_sample_lyrics.RDS")

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

saveRDS(final, "cleandata/example_data_lyrics.RDS")





