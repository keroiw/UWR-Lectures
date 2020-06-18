library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(nlme)
library(ggplot2)
library(lubridate)
library(bigstep)
library(doBy)
library(lme4)
library(sjPlot)
library(corrplot)

####

dataset.normalized %>% select(-hdi, -country, -year, -state) %>% as.matrix() %>% cor() %>% corrplot(method='circle')

#### Cohorts

dataset.cohort <- dataset.normalized %>% filter(year==min(dataset.normalized$year))
data_pred <- prepare_data(dataset.cohort$hdi, dataset.cohort %>% select(-hdi, -state, -country))
stepwise_res <-stepwise(data_pred, crit=bic)

# Check what kind of analusis can you perform => GLM book
cohort_lm <- lm(hdi ~ gdp_per_cap + risk_poverty_pct, dataset.cohort)

#dataset.cohort %>% ggplot(aes(x=region, y=hdi)) + geom_boxplot()

#### Longitudinal

to_diff <- setdiff(colnames(dataset.full), c("country", "year", "state", "immigration", "emmigration"))
to_sum <- c("an_ind_rate")

make_longit <- function(country_data) {
  baseline <- country_data[country_data$year == 2005,]
  years <- sort(unique(country_data$year))
  longit <- country_data
  i = 1
  for (year in years[2:length(years)]) {
    df_row <-  country_data[country_data$year == year, ]
    longit[country_data$year == year, to_diff] <-  df_row[,to_diff] - baseline[,to_diff]
    longit[country_data$year == year, to_sum] <- tail(cumsum(country_data[1:i, to_sum]), n=1)
    i = i + 1
  }
  longit[-1, ]
}

country_data <- list()
for (country_name in unique(dataset.normalized$country)) {
  country_data[[country_name]] <- dataset.normalized %>% filter(country==country_name)
}

dataset.longit <- dplyr::bind_rows(lapply(country_data, make_longit))

#### Mixed effects  

fit.rand_slp.full <- lme(hdi ~ year*state + population + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration + emmigration, 
                         random=~1+year|country,
                         data=dataset.longit,
                         method="ML")

fit.rand_slp.no_pop <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration + emmigration, 
                           random=~1+year|country,
                           data=dataset.longit,
                           method="ML")

fit.rand_slp.no_emi <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration, 
                           random=~1+year|country,
                           data=dataset.longit,
                           method="ML")

fit.rand_slp.no_emi <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration, 
                           random=~1+year|country,
                           data=dataset.longit,
                           method="ML")

fit.rand_slp.reduced <- lme(hdi ~ year*state + emp_pct + risk_poverty_pct + gdp_per_cap, 
                           random=~year-1|country,
                           data=dataset.normalized,
                           method="ML")


anova(fit.rand_slp.full, fit.rand_slp.no_pop, fit.rand_slp.no_emi, fit.rand_slp.reduced)

summary(fit.rand_slp.reduced)

anova(fit.rand_slp.reduced)

model_lmr4 <- lmer(hdi ~ year*state + emp_pct + risk_poverty_pct + gdp_per_cap + (1| country),
                   dataset.longit)

## Visualization

mgcv::fvisgam(m1, view=c("Time", "Trial"), cond=list(Group="Adults"), rm.ranef=TRUE)

dataset.longit %>% 
  mutate(country = country,
         year=year) %>%
  ggplot(aes(x=year, y=hdi)) + 
  geom_line(aes(color=country)) + 
  facet_grid(. ~ state) + 
  theme() 

dataset.longit %>%
  mutate(country = country,
         year=year) %>%
  ggplot(aes(x=year, y=hdi)) +
  geom_line(aes(color=country)) 
