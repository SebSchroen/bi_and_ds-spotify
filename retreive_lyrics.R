install.packages("geniusr")
readRenviron(".Renviron")
library(tidyverse)
library(tidytext)
# the CRAN version of the package is not working properly
remotes::install_github("https://github.com/giovanni-cutri/geniusr")
library(geniusr)


lyrics <- get_lyrics_search(artist_name = "Foo Fighters",
                  song_title = "Everlong")



