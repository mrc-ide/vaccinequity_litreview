library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(countrycode)
library(ggrepel)
library(MetBrewer)


# read *cleaned* data
df <- read.csv("cleaned_data.csv",
               stringsAsFactors = FALSE)

#read additional tab for multi country studies
df_multi <- readxl::read_xlsx("data/Data extraction vaccine equity.xlsx", sheet = "Multi country studies")

#================================================================================================================
#quick plot of studies by year
df %>%
  group_by(year_of_article) %>%
  summarise(count = n()) %>%
  mutate(is_complete = ifelse(year_of_article==2021, FALSE, TRUE)) %>%
  
  ggplot()+
  aes(x = year_of_article, y=count,  alpha = is_complete) +
  geom_col(fill = viridis::magma(9)[5])+
  theme_minimal()+
  labs(x = "Year of article",
       y =  "Study count")+
 # scale_fill_viridis_c(option = "magma", end = 0.8)+
  theme(legend.position = "none", text = element_text(size = 18))+
  scale_alpha_discrete(range = c(0.3, 1))

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
  mutate(continent_study_count = length(na.omit(unique(covidence_id)))) %>%
  mutate(iso3 = countrycode(sourcevar = region, origin = "country.name", destination = "iso3c"))

# add multi country
df_multi$study_count_multi <- rowSums(df_multi[, 2:26]==TRUE, na.rm = TRUE) 

world_map <- world_map %>%
  left_join(df_multi %>% select(iso3, study_count_multi)) %>%
  mutate(study_count = ifelse(is.na(study_count), 0, study_count)) %>%
  mutate(tot_count = ifelse(study_count + study_count_multi>0, study_count+study_count_multi, NA))


g_world <- map_bg +
  
  geom_polygon(data = world_map %>%
                  filter(!is.na(region)),
               aes(x=long, y = lat, group = group, fill =(tot_count)))+ 
  
  labs( fill = "Study count") +
  scale_fill_viridis_c(option = "magma", na.value = "azure2", limits = c(1,max(world_map$tot_count, na.rm = TRUE)), end = 0.8)+
  #scale_fill_manual(values = c("azure2",viridis::magma(16)))+
  theme(legend.position = "bottom", text = element_text(size = 18))


g_world

ggsave("figures/studies_by_country.png", height = 10, width = 12)

world_map %>%ungroup() %>% select(region, tot_count) %>% unique() %>% write.csv("study_count.csv", row.names = FALSE)
#================================================================================================================
# Study age ranges

df %>%
  mutate(age_min = as.numeric(age_min), age_max = as.numeric(age_max)) %>%
  mutate(simple_lab = paste0(first_author_surname, ", ", simple_country)) %>%
  
  ggplot()+
  #aes(y = reorder(simple_lab, simple_country), yend = reorder(simple_lab,simple_country), x = age_min, xend = age_max, colour = simple_country)+
  aes(y =  simple_country, yend = simple_country, x = age_min, xend = age_max, colour = simple_country)+
  geom_segment(size = 5, alpha = 0.3)+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Age of vaccinees", y = "Country of study")+
  scale_x_continuous(breaks = seq(0, max(as.numeric(df$age_max), na.rm = TRUE), by = 5))+
  scale_colour_manual(values = met.brewer("Renoir", n = length(unique(df$simple_country)), type = "continuous"))

ggsave("figures/study_age_range.png", height = 10, width = 12)
#================================================================================================================
