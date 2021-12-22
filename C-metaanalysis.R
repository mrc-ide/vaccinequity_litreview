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

# wealth =========================================================================
# relative risk
cols_to_use <- names(df)[grep("richest|poorest", names(df))]

wealth_out <- run_meta(df, cols_to_use, outp="OR")

# urban/rural =========================================================================
cols_to_use <- names(df)[grep("urban|rural", names(df))]

rural_out <- run_meta(df, cols_to_use, outp="OR")

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

# marital status =========================================================================
cols_to_use <- names(df)[grep("married", names(df))]

married_out <- run_meta(df, cols_to_use, outp="OR")

# combine =========================================================================
out <- data.frame(OR = c(gender_out$metan$beta[1], #this needs improving
                         wealth_out$metan$beta[1],
                         rural_out$metan$beta[1],
                         edu_out$metan$beta[1],
                         married_out$metan$beta[1]),
                  lb = c(gender_out$metan$ci.lb,
                          wealth_out$metan$ci.lb,
                          rural_out$metan$ci.lb,
                          edu_out$metan$ci.lb,
                          married_out$metan$ci.lb),
                  ub = c(gender_out$metan$ci.ub,
                          wealth_out$metan$ci.ub,
                          rural_out$metan$ci.ub,
                          edu_out$metan$ci.ub,
                          married_out$metan$ci.ub),
                  p = c(gender_out$metan$pval,
                         wealth_out$metan$pval,
                         rural_out$metan$pval,
                         edu_out$metan$pval,
                         married_out$metan$pval),
                  ref = c(gender_out$ref_grp,
                           wealth_out$ref_grp,
                           rural_out$ref_grp,
                           edu_out$ref_grp,
                           married_out$ref_grp))
write.csv(out, "metaanalysis_summary.csv", row.names = FALSE)