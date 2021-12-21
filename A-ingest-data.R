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

# other cleaning

# add grades
df <-  df %>% mutate(covidence_id = as.integer(covidence_id)) %>% 
  left_join(grades, by = "covidence_id")

# save cleaned file
df %>%
  write.csv("cleaned_data.csv",
            row.names = FALSE)

