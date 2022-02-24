library(dplyr)
library(readxl)

# data is downloaded from the google sheet and read from the "data" folder
df <- readxl::read_xlsx("data/Data extraction vaccine equity.xlsx")

# read in QA
grades <- read.csv("grades.csv", stringsAsFactors = FALSE)

# all cleaning and tidying happens in this script

# covidence ids
df <- df %>% 
  mutate(covidence_id = gsub("\\.0", "", covidence_id))

# countries
fun_split_country <- function(v){
  paste0(
    strsplit(v, split = ", ")[[1]],
    collapse = ";"
  )
}

df <- df %>%
  mutate(Country = toupper(Country))

df$Country_split <-  unlist(lapply(seq_along(df$Country), 
                                   FUN = function(x)fun_split_country(df$Country[x])))

#easy country
df <- df %>% mutate(simple_country = case_when(grepl(";", Country_split)~ "VARIOUS",
                                               grepl("COUNTRIES", Country_split)~"VARIOUS",
                                               grepl("LMIC", Country_split)~"VARIOUS",
                                               grepl("BURKINA FASO AND KENYA", Country_split)~"VARIOUS",
                                               TRUE~Country_split))

# clean year of data
df <- df %>% mutate(year_of_data = gsub(" ", "", year_of_data)) %>%
  mutate(year_of_data = gsub("\\.0", "", year_of_data)) %>%
  mutate(year_of_data = gsub("various", "", year_of_data)) %>%
  mutate(year_of_data = gsub("–", "-", year_of_data))

split_me_years <- function(string){
  split_string <- strsplit(string, "-")[[1]]
  paste0(split_string[1] : split_string[length(split_string)],collapse = ",")
}

df <- df %>%
  rowwise() %>%
  mutate(year_of_data = ifelse(grepl("-", year_of_data),
                               split_me_years(year_of_data), 
                               year_of_data)) 
df <- df %>%
  mutate(year_of_data_min = as.numeric(min(strsplit(year_of_data, ",")[[1]])),
         year_of_data_max = as.numeric(max(strsplit(year_of_data, ",")[[1]]))) %>%
  ungroup() 

# other cleaning


# add grades
df <-  df %>% mutate(covidence_id = as.integer(covidence_id)) %>% 
  left_join(grades, by = "covidence_id")

# save cleaned file
df %>%
  write.csv("cleaned_data.csv",
            row.names = FALSE)

