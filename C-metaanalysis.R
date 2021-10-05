library(dplyr)
library(ggplot2)
library(metafor)

df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

# gender =========================================================================
# examining relative risk of being female and vaccinated
tmp <- df %>%
  filter(!is.na(n_female.unvaccinated), !is.na(n_male.unvaccinated),
         !is.na(n_female.vaccinated)  , !is.na(n_male.vaccinated))

metan <- metafor::rma(n1i = tmp$number_vaccinated,
                      n2i = tmp$number_unvaccinated,
                      ai  = tmp$n_female.vaccinated,
                      bi  = tmp$n_male.vaccinated, #reference group
                      ci  = tmp$n_female.unvaccinated,
                      di  = tmp$n_male.unvaccinated,
                      measure = "RR")

metan
forest(metan, atransf = "exp",
       slab = paste(tmp$first_author, tmp$year_of_article))

# examining odds ratio of being female and vaccinated
metan <- metafor::rma(n1i = tmp$number_vaccinated,
                      n2i = tmp$number_unvaccinated,
                      ai  = tmp$n_female.vaccinated,
                      bi  = tmp$n_male.vaccinated,
                      ci  = tmp$n_female.unvaccinated,
                      di  = tmp$n_male.unvaccinated,
                      measure = "OR")

metan

forest(metan, atransf = "exp",
       slab = paste(tmp$first_author, tmp$year_of_article))

# wealth =========================================================================
# relative risk
tmp <- df %>%
  filter(!is.na(n_wealth_quintile_richest.unvaccinated), !is.na(n_wealth_quintile_poorest.unvaccinated),
         !is.na(n_wealth_quintile_richest.vaccinated)  , !is.na(n_wealth_quintile_poorest.vaccinated))

metan <- metafor::rma(n1i = tmp$number_vaccinated,
                      n2i = tmp$number_unvaccinated,
                      ai  = tmp$n_wealth_quintile_richest.vaccinated,
                      bi  = tmp$n_wealth_quintile_poorest.vaccinated, #reference group
                      ci  = tmp$n_wealth_quintile_richest.unvaccinated,
                      di  = tmp$n_wealth_quintile_poorest.unvaccinated,
                      measure = "RR")

metan
forest(metan, atransf = "exp",
       slab = paste(tmp$first_author, tmp$year_of_article))

# odds ratio
metan <- metafor::rma(n1i = tmp$number_vaccinated,
                      n2i = tmp$number_unvaccinated,
                      ai  = tmp$n_wealth_quintile_richest.vaccinated,
                      bi  = tmp$n_wealth_quintile_poorest.vaccinated, #reference group
                      ci  = tmp$n_wealth_quintile_richest.unvaccinated,
                      di  = tmp$n_wealth_quintile_poorest.unvaccinated,
                      measure = "OR")

metan
forest(metan, atransf = "exp",
       slab = paste(tmp$first_author, tmp$year_of_article))
