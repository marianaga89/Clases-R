library(billboard)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

hot_100 <- wiki_hot_100s
head(hot_100)

#artistas mas veces en los 100

artistas_100 <- hot_100 %>% 
  group_by(artist) %>%
  summarise(veces = n())

#artistas mas veces en el top 10

artistas_top_10 <- hot_100 %>% 
  filter(no >= 10) %>% 
  group_by(artist) %>% 
  summarise(veces = n())

#con mas mas 10 

top_15_artistas <- artistas_100 %>% 
  top_n(15) %>% 
  arrange(veces) %>% 
  mutate(artist = factor(artist, levels = unique(artist)))

library(ggplot2)

ggplot(top_15_artistas, aes(x = artist, y = veces, fill = artist)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none")

top_1_artistas <- wiki_hot_100s %>% 
  filter(no == 1) %>% 
  group_by(artist) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top_10_artistas <- wiki_hot_100s %>% 
  filter(no <= 10) %>% 
  group_by(artist) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top_15_artistas <- wiki_hot_100s %>% 
  filter(no <= 15) %>% 
  group_by(artist) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
