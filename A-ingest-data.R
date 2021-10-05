library(dplyr)
library(readxl)

# data is downloaded from the google sheet and read from the "data" folder
df <- readxl::read_xlsx("data/Data extraction vaccine equity.xlsx")

# all cleaning and tidying happens in this script

# covidence ids
df <- df %>% 
  mutate(covidence_id = gsub("\\.0", "", covidence_id))

# other cleaning

# save cleaned file
df %>%
  write.csv("cleaned_data.csv",
            row.names = FALSE)