library(tidyverse)
library(tidytext)
library(extrafont)
library(scales)
library(koRpus)
library(koRpus.lang.en)
library(pbapply)

rm(list = ls())

lyrics <- readRDS("LyricWiki.RDS")

lyrics <- lyrics %>%
  distinct() %>% #remove duplicates
  filter(!is.na(lyrics),
         nchar(lyrics) > 15,
         language == "English",
         between(year, 1950, 2019),#require valid year
         !str_detect(lyrics,"Unfortunately, we are not licensed to display")) %>% 
  select(-songby, -language) %>%
  mutate(
    decade = 10 * (year %/% 10),
    compress = map_int(lyrics, ~ length(memCompress(., type = "gzip"))) /
      nchar(lyrics),
    Ref=row_number()
  )

arts <- c(#====
  "Ella Fitzgerald",
  "Johnny Cash",
  "Bob Dylan",
  "Van Morrison",
  "Marty Robbins",
  "Frank Sinatra",
  "Elvis Presley",
  "Elton John",
  "Cliff Richard",
  "Dolly Parton",
  "Diana Ross",
  "Status Quo",
  "Louis Armstrong",
  "Nat King Cole",
  "The Hollies",
  "David Bowie",
  "Prince",
  "The Beach Boys",
  "The Beatles",
  "The Supremes",
  "Barry Manilow",
  "The Kinks",
  "Aretha Franklin",
  "Sparks",
  "The Rolling Stones",
  "Rod Stewart",
  "The Monkees",
  "Depeche Mode",
  "ABBA",
  "Stevie Wonder",
  "Bruce Springsteen",
  "Jethro Tull",
  "Pet Shop Boys",
  "Bee Gees",
  "Billy Joel",
  "Tom Jones",
  "Grateful Dead",
  "Snoop Dogg",
  "Motörhead",
  "Busta Rhymes",
  "The Bangles",
  "Blondie",
  "Madness",
  "Jim Reeves"
)

lyrArts <- lyrics %>% 
  filter(artist %in% arts) %>% 
  mutate(Ref=row_number())

saveRDS(lyrArts, "")

Sys.time()
set.kRp.env(TT.cmd="c:/TreeTagger/bin/tag-english.bat",lang="en")
textTag <- pblapply(lyrArts$lyrics,treetag,format="obj",stopwords = stop_words$word)
Sys.time() #takes about a minute for 20 lyrics - 53h for full set, 1h for top artists

treetag("Somewhere over the rainbow, way up high, there's a place that I heard of, once in a lullaby.", format = "obj")@TT.res

lyricsTag <- do.call(rbind,pblapply(seq_along(textTag),function(i) cbind(Ref=i,textTag[[i]]@TT.res)))

lyricsTag$stem <- NULL #column not used
lyricsTag$doc_id <- NULL #column not used
lyricsTag$desc <- NULL #column not used

saveRDS(lyricsTag, "lyricsTopTagged.RDS")
lyricsTag <- readRDS("lyricsTopTagged.RDS")

#extract other statistics...
lyrStats <- as.data.frame(t(pbsapply(textTag,function(x) unlist(x@desc[c(3:6,8,11:14)]))))
lyrStats <- lyrStats %>% 
  mutate(Ref=row_number())

saveRDS(lyrStats,"lyrTagStats.RDS")
lyrStats <- readRDS("lyrTagStats.RDS")

#readability scores... (this takes about 7s for 20)
Sys.time()
textReadability <- pbsapply(textTag, function(x) flesch(x, quiet=TRUE)@Flesch$RE) #Flesch reading ease score 0-100 (as used in MS Word)
Sys.time()

textReadability <- data.frame(Ref=lyrArts$Ref,Readability=textReadability,stringsAsFactors = FALSE)

saveRDS(textReadability,"textReadability.RDS")
textReadability <- readRDS("textReadability.RDS")

#merge data into texts
lyrArts <- lyrArts %>% 
  left_join(lyrStats) %>% 
  left_join(textReadability)

saveRDS(lyrArts,"lyrTopExtra.RDS")
lyrArts <- readRDS("lyrTopExtra.RDS")

lyricsTag %>% filter(Ref==938) %>% head(23) #over the rainbow
lyricsTag %>% filter(tag=="FW") %>% distinct(token)

wordtypes <- unique(lyricsTag$wclass)
wordtypes

multitypes <- lyricsTag %>% 
  group_by(lemma) %>% 
  summarise(notypes = length(unique(wclass)),
            tags = paste(unique(wclass), collapse = " ")) %>% 
  filter(notypes > 1) %>% 
  arrange(desc(notypes))

as.data.frame(table(lyricsTag$wclass,lyricsTag$tag)) %>% 
  filter(Freq>0) %>% 
  arrange(Var1, Var2)

typesdist <- lyricsTag %>% 
  count(wclass, lemma) %>% 
  group_by(wclass) %>% 
  top_n(10,n) %>% 
  arrange(wclass,desc(n))

tagtypes <- unique(lyricsTag$tag)
tagtypes <- droplevels(tagtypes)
tagtypes
#see https://www.laurenceanthony.net/software/tagant/resources/treetagger_tagset.pdf

#adjectives JJ, comparatives JJR, superlatives JJS
#modal verbs MD

tagsdist <- lyricsTag %>% 
  count(tag, lemma) %>% 
  group_by(tag) %>% 
  top_n(10,n) %>% 
  arrange(tag,desc(n))

artistAdjVerb <- lyricsTag %>% 
  mutate(subtag = substr(tag,1,2)) %>% 
  left_join(lyrArts %>% select(Ref, artist, decade)) %>% 
  count(artist, subtag, lemma) %>% 
  filter(subtag %in% c("JJ", "VV", "NN")) %>% 
  group_by(artist, subtag) %>% 
  top_n(1,n) %>% 
  arrange(artist, subtag)

decadeAdjVerb <- lyricsTag %>% 
  mutate(subtag = substr(tag,1,2)) %>% 
  left_join(lyrArts %>% select(Ref, decade)) %>% 
  count(decade, subtag, lemma) %>% 
  filter(subtag %in% c("JJ", "VV", "NN")) %>% 
  group_by(decade, subtag) %>% 
  top_n(1,n) %>% 
  arrange(decade, subtag)
