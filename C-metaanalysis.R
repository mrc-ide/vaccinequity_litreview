library(dplyr)
library(ggplot2)
library(metafor)
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

df <- df %>% janitor::clean_names()

# gender =========================================================================
# examining relative risk of being female and vaccinated
cols_to_use <- names(df)[grep("male", names(df))]

gender_out <- run_meta(df, cols_to_use, outp="OR")

p <- homemade_forest(gender_out, cols_to_use, 
                     "Adjusted odds ratio of being vaccinated given female compared to male")

ggsave(plot = p, filename = "figures/gender_OR.png", height = 12, width = 10)

# wealth =========================================================================
# relative risk
cols_to_use <- names(df)[grep("richest|poorest", names(df))]

wealth_out <- run_meta(df, cols_to_use, outp="OR")

p <- homemade_forest(wealth_out, cols_to_use,
                     "Adjusted odds ratio of being vaccinated given richest compared to poorest wealth quintile")

ggsave(plot = p, filename = "figures/wealth_OR.png", height = 12, width = 10)

# urban/rural =========================================================================
cols_to_use <- names(df)[grep("urban|rural", names(df))]

rural_out <- run_meta(df, cols_to_use, outp="OR")

p <- homemade_forest(rural_out, cols_to_use,
                     "Adjusted odds ratio of being vaccinated given rural compared to urban")

ggsave(plot = p, filename = "figures/urban_OR.png", height = 12, width = 10)

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

edu_out <- run_meta(df, cols_to_use, outp="OR")

p <- homemade_forest(edu_out, cols_to_use,
                     "Adjusted odds ratio of being vaccinated given mother uneducated compared to any education")

ggsave(plot = p, filename = "figures/edu_OR.png", height = 12, width = 10)

# marital status =========================================================================
cols_to_use <- names(df)[grep("married", names(df))]

married_out <- run_meta(df, cols_to_use, outp="OR")

p <- homemade_forest(married_out, cols_to_use,
                     "Adjusted odds ratio of being vaccinated given mother married compared to unmarried")

ggsave(plot = p, filename = "figures/married_OR.png", height = 12, width = 10)

# combine =========================================================================
list_out <- list(gender_out, wealth_out,rural_out, edu_out,married_out)
out <- bind_rows(lapply(1:length(list_out), 
                        FUN = function(x){data.frame(OR = list_out[[x]]$metan$beta,
                                                     lb = list_out[[x]]$metan$ci.lb,
                                                     ub = list_out[[x]]$metan$ci.ub,
                                                     p  = list_out[[x]]$metan$pval,
                                                     ref = list_out[[x]]$ref_grp)}))

write.csv(out, "metaanalysis_summary.csv", row.names = FALSE)
