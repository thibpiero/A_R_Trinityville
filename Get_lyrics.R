rm(list=ls())

library(tidyverse)
library(rvest)
library(tm)
library(devtools)
library(BRRR)

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
skrrrahh("bigshaq") # Big Shaq tells you when the loop is over.

Song_names <- sapply(lyric_data, "[[", 1)
Lyrics <- sapply(lyric_data, "[[", 2)

Trinityville <- data.frame(Song = Song_names, lyrics = Lyrics)


##Cleaning the lyrics
# As.Character
Trinityville$lyrics <- as.character(Trinityville$lyrics)
# Remove some words, name of artist etc... 
# Keep in mind to think about the featuring song to improve the analysis
Trinityville$lyrics <-gsub("Intro|Couplet 2|Couplet 1|Pont|Outro|Laylow|Jok'Air|Couplet 3|Couplet unique|Alpha Wann|Lomepal|Pré-refrain|Wit.|S.Pri Noir","", Trinityville$lyrics)
# Remove special characters
Trinityville$lyrics <-gsub("[^[:alnum:]]"," ", Trinityville$lyrics)
# Remove words with 1 or 2 characters
Trinityville$lyrics <-gsub(" *\\b[[:alpha:]]{1,2}\\b *"," ", Trinityville$lyrics)



# Analysis of the number of word by title
corpus = VCorpus(VectorSource(Trinityville$lyrics))
tdm = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
tdm
