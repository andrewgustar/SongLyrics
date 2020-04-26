#Investigations for https://www.musichistorystats.com/song-lyrics-8-parts-of-speech-tagging/

library(tidyverse)
library(tidytext)       #used here for the stop_words data 
library(koRpus)
library(koRpus.lang.en) #english language pack
library(pbapply)        #'apply' functions with a progress bar

#read in subset of lyrics data with just songs by main artists
lyrics <- readRDS("lyrics_mainArtists.RDS")

#do POS tagging
#
#This can take a long time, depending on the size of the data and the speed of your machine.
#You also need to install TreeTagger on your machine
#
set.kRp.env(TT.cmd="c:/TreeTagger/bin/tag-english.bat", lang="en")   #set path of treetagger executable

textTag <- pblapply(lyrics$lyrics, treetag, format="obj", stopwords = stop_words$word) 

#example as used in article
treetag("Somewhere over the rainbow, way up high, there's a place that I heard of, once in a lullaby.", format = "obj")@TT.res

#extract tagged lyrics into a dataframe
lyricsTag <- do.call(rbind, pblapply(seq_along(textTag), function(i) cbind(Ref=i, textTag[[i]]@TT.res)))

#delete unused columns
lyricsTag$stem <- NULL 
lyricsTag$doc_id <- NULL
lyricsTag$desc <- NULL

#extract other statistics from output of treetagger
lyrStats <- as.data.frame(t(pbsapply(textTag, function(x) unlist(x@desc[c(3:6,8,11:14)]))))
lyrStats <- lyrStats %>% 
  mutate(Ref=row_number())

#calculate readability scores - Flesch reading ease score 0-100 (as used in MS Word)
#
#this can also take a while in some cases
#actually not very useful for song lyrics due to inconsistency of punctuation, but have used it elsewhere
#
textReadability <- pbsapply(textTag, function(x) flesch(x, quiet=TRUE)@Flesch$RE) 

textReadability <- data.frame(Ref=lyrics$Ref, Readability=textReadability, stringsAsFactors = FALSE)

#merge data into lyrics dataframe by reference number
lyrics <- lyrics %>% 
  left_join(lyrStats) %>% 
  left_join(textReadability)

#check word classes and tag types used
#see https://www.laurenceanthony.net/software/tagant/resources/treetagger_tagset.pdf
wordtypes <- unique(lyricsTag$wclass)
tagtypes <- unique(lyricsTag$tag)
tagtypes <- droplevels(tagtypes)

#check for words that can have more than one tag depending on context
multitypes <- lyricsTag %>% 
  group_by(lemma) %>% 
  summarise(notypes = length(unique(wclass)),
            tags = paste(unique(wclass), collapse = " ")) %>% 
  filter(notypes > 1) %>% 
  arrange(desc(notypes))

#distribution of tags
as.data.frame(table(lyricsTag$wclass, lyricsTag$tag)) %>% 
  filter(Freq>0) %>% 
  arrange(Var1, Var2)

#top ten words for each tag class
typesdist <- lyricsTag %>% 
  count(wclass, lemma) %>% 
  group_by(wclass) %>% 
  top_n(10, n) %>% 
  arrange(wclass, desc(n))

#top ten words for each tag type
tagsdist <- lyricsTag %>% 
  count(tag, lemma) %>% 
  group_by(tag) %>% 
  top_n(10,n) %>% 
  arrange(tag,desc(n))

#top adjective, noun and verb for each artist
artistAdjVerb <- lyricsTag %>% 
  mutate(subtag = substr(tag, 1, 2)) %>% 
  left_join(lyrics %>% select(Ref, artist, decade)) %>% 
  count(artist, subtag, lemma) %>% 
  filter(subtag %in% c("JJ", "VV", "NN")) %>% 
  group_by(artist, subtag) %>% 
  top_n(1,n) %>% 
  arrange(artist, subtag)

#top adjective, noun and verb by decade
decadeAdjVerb <- lyricsTag %>% 
  mutate(subtag = substr(tag, 1, 2)) %>% 
  left_join(lyrics %>% select(Ref, decade)) %>% 
  count(decade, subtag, lemma) %>% 
  filter(subtag %in% c("JJ", "VV", "NN")) %>% 
  group_by(decade, subtag) %>% 
  top_n(1,n) %>% 
  arrange(decade, subtag)
