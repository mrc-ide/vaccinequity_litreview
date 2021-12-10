library(dplyr)
library(ggplot2)
library(readxl)

df <- readxl::read_xlsx("data/Data extraction vaccine equity.xlsx",
                        sheet = "CASP")
df <- df %>% janitor::clean_names()

df$score <-  unlist(
  lapply(1:nrow(df), 
         function(x)sum(grepl("yes", df[x, 2:10])))
)

df <- df %>%
  mutate(grade = case_when(score %in% 7:8 ~ "A",
                                   score %in% 5:6 ~ "B",
                                   score %in% 3:4 ~ "C",
                                   score %in% 1:2 ~ "D"))
df %>%
  ggplot()+
  aes(x = score_meaning, fill = score_meaning)+
  geom_bar()+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(legend.position = "none")+
  labs(x = "Grade", y = "Number of studies")

df %>%
  select(covidence_id, grade) %>%
  distinct() %>%
  write.csv("grades.csv")
