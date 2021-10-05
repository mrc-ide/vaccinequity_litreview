library(dplyr)
library(ggplot2)

df <- read.csv("cleaned_data.csv",
               stringsAsFactors = FALSE)

#quick plot of studies by year
df %>%
  group_by(year_of_article) %>%
  summarise(count = n()) %>%
  
  ggplot()+
  aes(x = year_of_article) +
  geom_histogram(fill = "lightgreen")+
  theme_minimal()+
  labs(x = "Year of article",
       y =  "Article count")

ggsave("figures/studies_by_year.png", height = 10, width = 12)
