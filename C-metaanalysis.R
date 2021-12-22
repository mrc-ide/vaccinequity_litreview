library(dplyr)
library(ggplot2)
library(metafor)
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

df <- df %>% janitor::clean_names()

# gender =========================================================================
# examining relative risk of being female and vaccinated
cols_to_use <- names(df)[grep("male", names(df))]

run_meta(df, cols_to_use, outp="RR")

run_meta(df, cols_to_use, outp="OR")

# wealth =========================================================================
# relative risk
cols_to_use <- names(df)[grep("richest|poorest", names(df))]

run_meta(df, cols_to_use, outp="RR")

run_meta(df, cols_to_use, outp="OR")

# urban/rural =========================================================================
cols_to_use <- names(df)[grep("urban|rural", names(df))]

run_meta(df, cols_to_use, outp="RR")

run_meta(df, cols_to_use, outp="OR")

# mothers edu =========================================================================
df <- df %>% rowwise() %>%
  mutate(n_mother_any_vaccinated = sum(n_mother_education_primary_vaccinated,
                      n_mother_education_secondary_vaccinated, n_mother_education_higher_vaccinated,
                      na.rm = TRUE),
                    n_mother_any_unvaccinated = sum(n_mother_education_primary_unvaccinated,
                      n_mother_education_secondary_unvaccinated, n_mother_education_higher_unvaccinated,
                      na.rm=TRUE)) %>% ungroup() %>%
  mutate(n_mother_any_unvaccinated = ifelse(n_mother_any_unvaccinated==0,NA, n_mother_any_unvaccinated),
         n_mother_any_vaccinated = ifelse(n_mother_any_vaccinated==0,NA, n_mother_any_vaccinated))

cols_to_use <- names(df)[grep("none|r_any", names(df))]

run_meta(df, cols_to_use, outp="RR")

run_meta(df, cols_to_use, outp="OR")

# urban/rural =========================================================================
cols_to_use <- names(df)[grep("married", names(df))]

run_meta(df, cols_to_use, outp="RR")

run_meta(df, cols_to_use, outp="OR")
