library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(countrycode)
library(ggrepel)


# read *cleaned* data
df <- read.csv("cleaned_data.csv",
               stringsAsFactors = FALSE)

#================================================================================================================
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

#================================================================================================================
# quick plot of studies by country

#get world map
world_map <- map_data("world")
#plot background
map_bg <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), 
               fill="grey") +
  coord_quickmap(ylim = c(-51, 70)) +
  theme_void()

#get midpoints of countries in case it is handy later
countries <- CoordinateCleaner::countryref

countries <- countries %>% select(name, iso3, centroid.lon, centroid.lat) %>% 
  group_by( iso3) %>%
  summarise(long = centroid.lon[[1]], lat = centroid.lat[[1]], name = name[[1]])

countries <-  countries %>% mutate(name = toupper(name))

#match case
world_map <- world_map %>% mutate(region = toupper(region))

# join 'em up
world_map <- world_map %>% left_join(df, by = c("region" = "Country"))
# add the continents
world_map <- world_map %>% mutate(continent = countrycode::countrycode(sourcevar = region,
                                                                       origin = "country.name",
                                                                       destination = "continent"))
#add study count
world_map <- world_map %>% 
  group_by(region) %>% 
  mutate(study_count = length(na.omit(unique(covidence_id)))) %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(continent_study_count = length(na.omit(unique(covidence_id))))

g_world <- map_bg +
  
  geom_polygon(data = world_map %>%
                  filter(!is.na(region)),
               aes(x=long, y = lat, group = group, fill = as.factor(study_count)))+ 
  
  labs( fill = "Study count") +
  scale_fill_manual(values = blues9[c(2,4,6,8)])+
  theme(legend.position = "bottom")

g_world

ggsave("figures/studies_by_country.png", height = 10, width = 12)
#================================================================================================================