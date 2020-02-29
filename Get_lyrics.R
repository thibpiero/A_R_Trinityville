rm(list=ls())

library(tidyverse)
library(rvest)

# Create vector of title

title <- c("Initialisation", "megatron", "dehors-dans-la-night", "hillz","plug","menu-principal", "piranha-baby",
           "trinityville","mieux-vaut-pas-regarder-part-1","vamonos", "akanizer","burning-man","il-etait-une-fois-sous-leau",
           "longue-vie","mieux-vaut-pas-regarder-part-2","de-batard","tentative-de-reconnexion","poizon", "nakre","million-flowerz",
           "manuel-dutilisation","logiciel-triste")

# List

url<- list()
webpage <- list()
song_lyrics <- list()
lyric_data <- list()

# Create list of url 
# Create list of webpage
# Scrqpe the section
# Convert the data to text 

for (i in 1:length(title)) {
  url[[i]] <- paste0("https://genius.com/Laylow-", title[i] ,"-lyrics" )
  webpage[[i]] <- read_html(url[[i]])
  song_lyrics[[i]]  <- html_nodes(webpage[[i]] ,'p , .header_with_cover_art-primary_info-title')
  lyric_data[[i]] <- html_text(song_lyrics[[i]])
  
}

Song_names <- sapply(lyric_data, "[[", 1)
Lyrics <- sapply(lyric_data, "[[", 2)

Trynityville <- data.frame(Song = Song_names, lyrics = Lyrics)




