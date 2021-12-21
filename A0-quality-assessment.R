library(dplyr)
library(ggplot2)
library(readxl)

df <- readxl::read_xlsx("data/Data extraction vaccine equity.xlsx",
                        sheet = "CASP")
df <- df %>% janitor::clean_names()

# cols to consider in scoring
cols_to_use <- names(df)[c(2:6, 8:10)]

#calculate score where good = 3, adequate = 2, poor = 1
df$score <- sapply(1:nrow(df), FUN = function(x){
  sum(grepl("good", df[x,cols_to_use]))*3 + 
    sum(grepl("adequate", df[x,cols_to_use]))*2 + 
    sum(grepl("poor", df[x,cols_to_use]))*1
})

df <- df %>%
  mutate(grade = case_when(score %in% 23:24 ~ "A",
                                   score %in% 21:22 ~ "B",
                                   score %in% 19:20 ~ "C",
                                   score %in% 17:18 ~ "D"))
df %>%
  ggplot()+
  aes(x = grade, fill = grade)+
  geom_bar()+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(legend.position = "none")+
  labs(x = "Grade", y = "Number of studies")

df %>%
  select(covidence_id, grade) %>%
  distinct() %>%
  write.csv("grades.csv", row.names = FALSE)
