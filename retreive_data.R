library(tidyverse)
library(spotifyr)
library(foreach)
readRenviron(".Renviron")

auth_object <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])

categories <- get_categories()

cats <- c("Pop", "Hip-Hop",  "Metal", "Indie", "Rock", "Dance/Electronic", "Alternative", "Rock", "Party")
category_ids <- categories %>% 
  filter(name %in% cats) %>% 
  pull(id)



category_playlists <- foreach(id = category_ids, .combine = "rbind") %do% {
  
  tmp <- get_category_playlists(category_id = id, limit = 20, country = "DE", include_meta_info = FALSE)
  
  tmp$category_id = id
  tmp
}


playlists <- categories %>% 
  select(id, name) %>% 
  rename(category = name,
         category_id = id) %>% 
  right_join(category_playlists, by = "category_id") %>% 
  filter(tracks.total <= 100)


playlist_ids <- playlists %>% 
  pull(id)

category_songs <- foreach(i = playlist_ids, .combine = "rbind", .errorhandling = "remove") %do% {
  
  out <- get_playlist_tracks(playlist_id = i)
  title <- playlists %>% 
    filter(id == i) %>% 
    distinct(category) %>% 
    pull()
  
  
  
  out <- out %>%
    filter(!is.na(track.id)) %>%
    # separate out the df column artists
    unnest(cols = 'track.artists') %>%
    group_by(track.id) %>%
    mutate(row_number = 1:n(),
           track.artist = name,
           track.artist.id = id) %>%
    ungroup() %>%
    filter(row_number == 1) %>%
    select(track.id, track.name, track.artist, track.artist.id, track.popularity, track.album.id, track.album.name, track.album.release_date, track.duration_ms, track.explicit) %>% 
    mutate(playlist.id = i,
           category = title)
  
  Sys.sleep(1)
  out
  
}



