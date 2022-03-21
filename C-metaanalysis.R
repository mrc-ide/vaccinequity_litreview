library(dplyr)
library(ggplot2)
library(metafor)
library(MetBrewer)
library(countrycode)

R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

df <- df %>% janitor::clean_names()

meas <- "RR"
meas_long <- ifelse(meas=="RR", "Risk ratio", "Odds ratio")

df <- df %>% mutate(simple_iso = countrycode(simple_country, origin = "country.name", destination = "iso3c"))
df <- df %>% mutate(simple_iso = ifelse(simple_country=="VARIOUS", "*VAR", simple_iso))

df <- df %>% mutate(simple_vaccine = ifelse(grepl("1974 EPI", vaccine), "1974 EPI +", vaccine))
df <- df %>% mutate(simple_vaccine = factor(simple_vaccine))

# gender =========================================================================
# examining relative risk of being female and vaccinated
cols_to_use <- names(df)[grep("male", names(df))]

gender_out <- run_meta(df, cols_to_use, outp=meas)

axsttl <- paste0(meas_long," of being vaccinated given female compared to male")

p <- homemade_forest(gender_out, cols_to_use, axsttl)

ggsave(plot = p, filename = paste0("figures/gender_", meas,".png"), height = 14, width = 10)

# additionally interested in the trend over time
p <- meas_over_time(gender_out, cols_to_use, axsttl, meas)

ggsave(plot = p, filename = paste0("figures/gender_",meas, "_over_time.png"), height = 12, width = 10)

#ids
ids <- data.frame(type = "gender",
                  ids = paste0(gender_out$escalc_out$covidence_id %>% unique(), collapse = ", "))

# wealth =========================================================================
# relative risk
#cols_to_use <- names(df)[grep("richest|poorest", names(df))]

# change order
cols_to_use <- c("n_wealth_quintile_poorest_vaccinated", "n_wealth_quintile_poorest_unvaccinated", "n_wealth_quintile_richest_vaccinated", "n_wealth_quintile_richest_unvaccinated")

wealth_out <- run_meta(df, cols_to_use, outp=meas)

axsttl <- paste0("Log ", tolower(meas_long),
                 " of being vaccinated given poorest compared to richest wealth quintile")

p <- homemade_forest(wealth_out, cols_to_use, axsttl, lg=TRUE)

ggsave(plot = p, filename = paste0("figures/wealth_",meas,".png"), height = 12, width = 10)

# additionally interested in the trend over time
p <- meas_over_time(wealth_out, cols_to_use, axsttl, meas)

ggsave(plot = p, filename = paste0("figures/wealth_",meas,"_over_time.png"), height = 12, width = 10)

#ids
ids <- ids %>%
  bind_rows(data.frame(type = "wealth",
                       ids = paste0(wealth_out$escalc_out$covidence_id %>% unique(), collapse = ", ")))

# urban/rural =========================================================================
cols_to_use <- names(df)[grep("urban|rural", names(df))]

rural_out <- run_meta(df, cols_to_use, outp=meas)

axsttl <- paste(meas_long," of being vaccinated given rural compared to urban")

p <- homemade_forest(rural_out, cols_to_use,axsttl)

ggsave(plot = p, filename = paste0("figures/urban_",meas,".png"), height = 14, width = 10)

# additionally interested in the trend over time
p <- meas_over_time(rural_out, cols_to_use, axsttl,meas)

ggsave(plot = p, filename = paste0("figures/urban_",meas,"_over_time.png"), height = 12, width = 10)

#ids
ids <- ids %>%
  bind_rows(data.frame(type = "urban",
                       ids = paste0(rural_out$escalc_out$covidence_id %>% unique(), collapse = ", ")))

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

axsttl <- paste(meas_long," of being vaccinated given mother has no formal education compared to any education")

cols_to_use <- names(df)[grep("none|r_any", names(df))]

edu_out <- run_meta(df, cols_to_use, outp=meas)

p <- homemade_forest(edu_out, cols_to_use, axsttl)

ggsave(plot = p, filename = paste0("figures/edu_",meas,".png"), height = 12, width = 10)

# additionally interested in the trend over time
p <- meas_over_time(edu_out, cols_to_use, axsttl, meas)

ggsave(plot = p, filename = paste0("figures/edu_",meas,"_over_time.png"), height = 12, width = 10)

#ids
ids <- ids %>%
  bind_rows(data.frame(type = "edu",
                       ids = paste0(edu_out$escalc_out$covidence_id %>% unique(), collapse = ", ")))

# marital status =========================================================================
cols_to_use <- names(df)[grep("married", names(df))]

married_out <- run_meta(df, cols_to_use, outp=meas)

axsttl <- paste0(meas_long," of being vaccinated given mother married compared to unmarried")

p <- homemade_forest(married_out, cols_to_use,axsttl)

ggsave(plot = p, filename = paste0("figures/married_", meas,".png"), height = 12, width = 10)

# additionally interested in the trend over time
p <- meas_over_time(married_out, cols_to_use, axsttl, meas)

ggsave(plot = p, filename = paste0("figures/married_", meas,"_over_time.png"), height = 12, width = 10)

ids <- ids %>%
  bind_rows(data.frame(type = "married",
                       ids = paste0(married_out$escalc_out$covidence_id %>% unique(), collapse = ", ")))

# combine =========================================================================
list_out <- list(gender_out, wealth_out,rural_out, edu_out,married_out)
out <- bind_rows(lapply(1:length(list_out), 
                        FUN = function(x){data.frame(out_meas = list_out[[x]]$metan$beta,
                                                     lb = list_out[[x]]$metan$ci.lb,
                                                     ub = list_out[[x]]$metan$ci.ub,
                                                     p  = list_out[[x]]$metan$pval,
                                                     ref = list_out[[x]]$ref_grp)}))

out <- out %>% mutate(prob = plogis(out_meas))
out <- out %>% mutate(exp_out = exp(out_meas), exp_lb = exp(lb), exp_ub = exp(ub))

names(out)[names(out) == "out_meas"] <- meas

write.csv(out, "metaanalysis_summary.csv", row.names = FALSE)

write.csv(ids, "metaanalysis_refs.csv", row.names = FALSE)
