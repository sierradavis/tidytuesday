library(showtext)
font_add_google(name = "Sansita Swashed")
showtext_auto()

library(tidytext)
library(textdata)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggimage)
library(ggbump)

episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv")
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")


tidy_dialogue <- dialogue %>% 
  unnest_tokens(word, dialogue) %>% 
  na.omit()

sentiment_dict<- get_sentiments("nrc") %>% 
  filter(!sentiment %in% c("positive", "negative"))


sentiment_by_writer<-tidy_dialogue %>%
  inner_join(sentiment_dict) %>%
  inner_join(episodes) %>% 
  filter(written_by == "The Duffer Brothers") %>% 
  group_by(season,sentiment) %>% 
summarise(total_each_sentiment=n())%>%
 arrange(season,desc(total_each_sentiment)) %>% 
  mutate(rank = row_number())
 

sentiment_labels <- sentiment_by_writer %>% 
  filter(season == 4, rank <= 8) %>% 
  pull(sentiment)

final_plot <-sentiment_by_writer %>% 
ggplot(aes(season, rank, col = sentiment)) +
  geom_bump(size = 3) +
  geom_point(shape = 21, size = 3, stroke = 1, fill = "black") +
  geom_text(
    data = sentiment_by_writer %>% filter(season == 1, sentiment %in% sentiment_labels),
    aes(label = sentiment),
    hjust = 1,
    nudge_x = -0.1,
    fontface = 'bold'
  ) +
  geom_text(
    data = sentiment_by_writer %>% filter(season == 4, sentiment %in% sentiment_labels),
    aes(label = rank),
    hjust = 0,
    nudge_x = 0.1,
    size = 3,
    fontface = 'bold'
  ) +
  guides(color = "none") +
  scale_x_continuous(position = "top") +
  scale_y_reverse() +
  scale_color_manual(values = c("#073e1e", "#3a5fe5", "#ff1515", "#800404","#1e193c","tomato3", "tomato4","sienna4")) +
  coord_cartesian(clip = "off") +
  theme_void()  +
  theme(
    plot.background = element_rect(color = NA, fill = "black"), 
    plot.margin = margin(t = 4, r = 40, b = 4, l = 40),
    plot.title = element_text(family = "Sansita Swashed",
                              color = "white",
      size = 50, hjust = 0.5, margin = margin(t = 4, b = 2)),
    plot.subtitle = element_text(color = "#ad0707", hjust = 0.5,size = 10, family = "sans",margin = margin(b = 3)),
    plot.caption = element_text(color = "#ad0707", margin = margin(b = 2)),
    axis.text.x.top = element_text(color = "#ad0707", margin = margin(b = 2))
    )+
  labs(title = "Emotions Felt in Stranger Things", 
       subtitle = 'Plot shows the sentiment rank for each season of Stranger Things based on dialoge written by The Duffer Brothers',
       caption = "Source - #TidyTuesday | plot by: @__sierradavis")

ggsave("stranger_things_final.png", final_plot,dpi = 300, height = 11, width = 13)
