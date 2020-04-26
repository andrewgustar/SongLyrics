#To reproduce chart like the one at https://www.musichistorystats.com/song-lyrics-3-repetition-and-compression/
#although that one was done with the full dataset, so slightly different

library(tidyverse) 
library(tidytext)
library(extrafont) #optional for formatting chart

#read in subset of lyrics data with just songs by main artists
lyrics <- readRDS("lyrics_mainArtists.RDS")

repeatedlyricwords <- lyrics %>%
  unnest_tokens(word, lyrics) %>%                    #split into words
  group_by(artist, title) %>%
  mutate(repeated = (word == lag(word))) %>%         #identify repeated words
  ungroup() %>%
  filter(!is.na(repeated) & repeated) %>%            #filter to repeated words
  group_by(word, decade) %>%
  count() %>%
  filter(n > 5) %>%                                  #only those apeparing at least 5 times a decade
  group_by(decade) %>%
  mutate(rank = rank(-n, ties.method = "random"),    #rank order (1 = most common)
         relprop = n/max(n)) %>%                     #frequency as proportion of most common
  arrange(rank, decade) %>%
  filter(rank < 25)                                  #just consider the top 25 by decade

repeatedlyricwords %>% 
  ungroup() %>% 
  mutate(decade = paste0(decade,"s")) %>%
  group_by(word) %>% 
  mutate(bestpos=min(rank),                          #top position reached
         rank=pmin(rank,11)) %>%                     #set lower ranks to 11
  ungroup() %>% 
  filter(bestpos<11) %>%                             #ignore any that never reached top ten
  ggplot(aes(x=decade, y=rank, colour=word, group=word, label=word)) +
  geom_line(size=1) +
  geom_label(aes(size=relprop), family = "Cambria", fill="grey95") +
  scale_color_discrete(guide = FALSE) +
  scale_y_reverse(breaks=c(1:20), limits=c(10,1), minor_breaks = NULL) +
  scale_size_continuous(guide = FALSE) +
  theme_light(base_family = "Cambria") +
  ggtitle("Top 10 repeated song lyrics by decade",
          subtitle = "Text size reflects frequency relative to most common word")
