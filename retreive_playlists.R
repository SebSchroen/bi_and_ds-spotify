library(tidyverse)
library(spotifyr)
library(foreach)
readRenviron(".Renviron")

auth_object <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])

offset <- seq(51, 1000, 50)
offset <- c(0, offset)
playlists_1 <- foreach(i = offset, .combine = "rbind",.errorhandling = "remove",
                     .packages = c("tidyverse", "spotifyr")) %do% {

    search_spotify("meine playlist", type = "playlist", limit = 50, offset = i, market = "DE")

                     }

playlists_2 <- foreach(i = offset, .combine = "rbind",.errorhandling = "remove",
                       .packages = c("tidyverse", "spotifyr")) %do% {
                         
                         search_spotify("meine lieder", type = "playlist", limit = 50, offset = i, market = "DE")
                         
                       }

playlists <- rbind(playlists_1, playlists_2) %>% 
  filter(owner.id != "spotify") %>% 
  arrange(tracks.total)

hist(playlists$tracks.total, breaks = 100)

playlists
playlist_ids <- playlists %>% 
  pull(id)

cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)

user_playlist_songs <- foreach(i = playlist_ids, .combine = "rbind", .errorhandling = "remove",
                          .packages = c("tidyverse", "spotifyr")) %dopar% {
                            
                            out <- get_playlist_tracks(playlist_id = i)

                            
                            
                            
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
                              mutate(playlist.id = i)
                            
                            Sys.sleep(1)
                            out
                            
                          }
library(tidyverse)

saveRDS(user_playlist_songs, "cleandata/user_playlist_songs.RDS")
user_playlist_songs <- readRDS("cleandata/user_playlist_songs.RDS") %>% 
  mutate(track.album.release_date = as_date(track.album.release_date)) %>% 
  filter(date <= "2018-12-31" & date >= "1985-01-01") %>% 
  select(track.id, track.name, track.artist, track.artist.id, track.playlist.id, track.album.release_date) 


user_playlist_songs %>% 
  saveRDS("cleandata/example_data_association_rule.RDS")



small_playlists <- user_playlist_songs %>% 
  group_by(playlist.id) %>% 
  summarise(n = n())  %>% 
  filter(n  <= 20) %>% 
  pull(playlist.id)


timeline <- user_playlist_songs %>%  
  mutate(date = floor_date(date, "month")) %>% 
  drop_na(date) %>% 
  group_by(date) %>%  
  summarise(n = n())%>%
  filter(date >= "1945-01-01")


  ggplot(., aes(x = date, y = n)) + geom_line()
  
user_playlist_songs %>% 

  distinct(track.artist, track.id, ) %>% 
  group_by(track.artist) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(50)%>% 
  ggplot(aes(x = reorder(track.artist, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Artists in my Playlists",
       x = "Artist",
       y = "Number of Songs") +
  theme_minimal()

  
  library(arules)

  playlist_artists <- user_playlist_songs %>% 
  #  filter(playlist.id %in% small_playlists) %>% 
    filter(date <= "2018-12-31" & date >= "1985-01-01") %>% 
    mutate(song = paste(track.artist, track.name, sep = "-")) %>% 
    distinct(track.artist, playlist.id) %>% 
    group_by(track.artist) %>% 
    mutate(count = n()) %>% 
 #   filter(count >= 5) %>% 
    select(-count) %>% 
    ungroup()

  
  
  trans <- transactions(playlist_artists, cols = c("playlist.id", "track.artist"), format = "long")
  itemLabels(trans)
  dim(trans)
  
  
  rules <- apriori(trans, 
                   parameter = list(supp=0.01, conf=0.5, maxlen=3,
                                    target= "rules"))
  inspect(rules, by = "lift", control = list(n = 10))
  library(arulesViz)
  plot(rules, method = "grouped", limit = 25)
  plot(rules, method = "graph", limit = 10)
  
  
  
  playlist_songs <- user_playlist_songs %>% 
    #  filter(playlist.id %in% small_playlists) %>% 
    filter(date <= "2018-12-31" & date >= "1985-01-01") %>% 
    mutate(song = paste(track.artist, track.name, sep = "-")) %>% 
    distinct(track.artist, track.name, song, playlist.id, track.popularity) %>% 
    arrange(playlist.id, track.artist, desc(track.popularity)) %>% 
    group_by(playlist.id, track.artist) %>% 
    mutate(id = 1:n()) %>% 
    filter(id == 1)

    trans <- transactions(playlist_songs, cols = c("playlist.id", "song"), format = "long")
  itemLabels(trans)
  dim(trans)
  
  
  rules <- apriori(trans, 
                   parameter = list(supp=0.005, conf=0.5, maxlen=2,
                                    target= "rules"))
  inspect(rules, by = "lift", control = list(n = 10))
  library(arulesViz)
  plot(rules, method = "grouped", limit = 25)
  plot(rules, method = "graph", limit = 10)  
  