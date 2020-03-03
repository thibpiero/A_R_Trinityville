rm(list=ls())

library(tidyverse)
library(rvest)
library(tm)
library(devtools)
library(BRRR)
library(tidytext)
library(syuzhet)
library(wordcloud)
library(wordcloud2)

# Create vector of title

title <- c("Initialisation", "megatron", "dehors-dans-la-night", "hillz","plug","menu-principal", "piranha-baby",
           "trinityville","mieux-vaut-pas-regarder-part-1","vamonos", "akanizer","burning-man","il-etait-une-fois-sous-leau",
           "longue-vie","mieux-vaut-pas-regarder-part-2","de-batard","tentative-de-reconnexion","poizon", "nakre","million-flowerz",
           "manuel-dutilisation","logiciel-triste")

# List

url<- list()
webpage <- list()
song_lyrics <- list()
song_producers <- list()
lyric_data <- list()
producers_data <- list()


# Create list of url 
# Create list of webpage
# Scrqpe the section
# Convert the data to text 


for (i in 1:length(title)) {
  url[[i]] <- paste0("https://genius.com/Laylow-", title[i] ,"-lyrics" )
  webpage[[i]] <- read_html(url[[i]])
  song_lyrics[[i]]  <- html_nodes(webpage[[i]] ,'p , .header_with_cover_art-primary_info-title')
  song_producers[[i]] <- html_nodes(webpage[[i]] ,'.metadata_unit , .metadata_unit-label , .drop-target-attached-center')

  lyric_data[[i]] <- html_text(song_lyrics[[i]])
  producers_data[[i]] <- html_text(song_producers[[i]])
  
}
skrrrahh("bigshaq") # Big Shaq tells you when the loop is over.

Song_names <- sapply(lyric_data, "[[", 1)
Lyrics <- sapply(lyric_data, "[[", 2)
lyric_data[[2]][14]
Producers <- sapply(lyric_data, "[[", 2)

Trinityville <- data.frame(Song = Song_names, lyrics = Lyrics)

#write.csv(Trinityville, "/Users/francois/Desktop/RTHIB/A_R_Trinityville/Lyrics.csv")

##Cleaning the lyrics
# As.Character
Trinityville$lyrics <- as.character(Trinityville$lyrics)
# Remove some words, name of artist etc... 
# Keep in mind to think about the featuring song to improve the analysis
Trinityville$lyrics <-gsub("Intro|Couplet 2|Refrain|Couplet 1|Pont|Outro|Laylow|Jok'Air|Couplet 3|Couplet unique|Alpha Wann|Lomepal|Pré-refrain|Wit.|S.Pri Noir","", Trinityville$lyrics)
# Remove special characters
Trinityville$lyrics <-gsub("[^[:alnum:]]"," ", Trinityville$lyrics)
# Remove words with 1,2,3 or 4 characters
Trinityville$lyrics <-gsub(" *\\b[[:alpha:]]{1,3}\\b *"," ", Trinityville$lyrics)

# Remove some indesirable word 
Trinityville$lyrics <-gsub("dans| suis|tout |pour|mais|comme|quand|plus|faire |fait|sais | même |vais | veut| veux| avec| vaut | étais
                           |leur | mets | quoi| parce ","", Trinityville$lyrics)
# Remove number 
Trinityville$lyrics <- gsub('[[:digit:]]+', '', Trinityville$lyrics)



# Analysis of the number of word by title

corpus = VCorpus(VectorSource(Trinityville$lyrics))
tdm = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
Word_by_title <- as.data.frame(tdm)
Word_by_title$word <- row.names(Word_by_title)

colnames(Word_by_title) <- Trinityville$Song
# Analysis of the number of word in the entire album

tdm.m = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
term.freq <-rowSums(tdm.m)
Word_trinity <-data.frame(word=names(term.freq),
                     frequency=term.freq)
Word_trinity <-Word_trinity[order(Word_trinity[,2], decreasing=T),]; Word_trinity[1:50,]
Word_trinity$word <-factor(Word_trinity$word,
                      levels=unique(as.character(Word_trinity$word)))


Word_by_title[c("Initialisation", )]



## Code a word cloud

# Word cloud album
wordcloud(words = Word_trinity$word, freq = Word_trinity$frequency, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))

wordcloud2(data=Word_trinity, size=1.6, color='random-dark')

wordcloud2(data=Word_trinity, size = 0.7, shape = 'diamond')
wordcloud2(Word_trinity, size = 0.5, minRotation = -pi/2, maxRotation = -pi/2)


# Word cloud by title


wordcloud(words = Word_by_title[,"word"], freq = Word_by_title[,"Initialisation"], min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))


