#Word Raster diagrams for https://www.musichistorystats.com/song-lyrics-3-repetition-and-compression/

library(tidyverse)
library(tidytext)  #for unnest_tokens 
library(extrafont) #optional for formatting chart

#read in subset of lyrics data with just songs by main artists
lyrics <- readRDS("lyrics_mainArtists.RDS")

#add GZIP compression ratios
lyrics <- lyrics %>% 
  mutate(compress = map_int(lyrics, ~ length(memCompress(., type = "gzip"))) / nchar(lyrics))

#find an interesting song - modify as appropriate!
song <- lyrics %>% 
  filter(str_detect(artist,"Elton"),
         str_detect(title,"Yellow")) %>% 
  sample_n(1)

#split into words
words <- song %>% unnest_tokens(word, lyrics)

#create word raster diagram
outer(words$word, words$word, "==") %>%                    #create logical matrix of repeated words
  as_tibble(.name_repair = "unique") %>%                   #convert to tibble
  mutate(Y = row_number()) %>%                             #add y axis data (x is the columns)
  pivot_longer(-Y, names_to = "X", values_to = "same") %>% #convert to long format for ggplot
  mutate(X = as.integer(str_remove_all(X, "\\."))) %>%     #restore x axis to numeric
  ggplot(aes(x = X, y = Y, fill = same)) +
  geom_raster() +
  theme_minimal(base_family = "Cambria") +                 #base_family optional
  scale_x_continuous(name = "word") +
  scale_y_continuous(name = "word") +
  scale_fill_manual(values = c("lightyellow", "blue"), guide = FALSE) +
  coord_equal() +                                          #force to be square
  ggtitle(paste(song$title[1], "by", song$artist[1]),
          subtitle = paste("Unique words",
                           length(unique(words$word)),
                           ":",
                           round(100 * length(unique(words$word)) / nrow(words)),
                           "% of total words\nGZIP Compression :",
                           round(100 * song$compress), "%"))

