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
library(MASS)

fit_gls <- function(predictors, dataset, corr_structure, weights_structure, method="REML") {
  predictors <- paste(predictors, collapse = ' + ')
  predictors <- paste(predictors, "year*state", sep = ' + ')
  gls_formula <- as.formula(paste("hdi", predictors, sep=" ~ "))
  gls_fit <- gls(model = gls_formula,                           
                 corr_structure(form=~1|country), 
                 weights_structure,
                 data=dataset,
                 method=method)
  gls_fit
}

chi_sq_right <- function(t_stat, df) {
  chi_p <- pchisq(t_stat, df=df)
  1 - chi_p
}

lrt <- function(m1, m2) {
  df1 <- attr(logLik(m1, REML = TRUE), "df")
  df2 <- attr(logLik(m2, REML = TRUE), "df")
  
  loglik1 <- round(logLik(m1, REML = TRUE), 3)
  loglik2 <- round(logLik(m2, REML = TRUE), 3)
  
  diff_LL <- -2*(loglik2 - loglik1)
  p_val <- chi_sq_right(diff_LL, df=df1-df2)
  
  summary_df <- data.frame(logLik = c(loglik1, loglik2), 
                           df = c(df1, df2),
                           LL_diff = c(round(diff_LL, 3), NA),
                           p_val = c(round(p_val, 6), NA))
  summary_df <- t(summary_df)
  colnames(summary_df) <- c("Full model", "Reduced model")
  summary_df
}

#### Cohorts

dataset.cohort <- dataset.normalized %>% filter(year==min(dataset.normalized$year))
data_pred <- prepare_data(dataset.cohort$hdi, dataset.cohort %>% select(-hdi, -state, -country))
stepwise_red <-stepwise(data_pred, crit=bic)

# Check what kind of analysis can you perform => GLM book
cohort_lm <- lm(hdi ~ gdp_per_cap + risk_poverty_pct, dataset.cohort)

#### Correlation

dataset.normalized %>% select(-hdi, -country, -year, -state) %>% as.matrix() %>% cor() %>% corrplot(method='circle')

#### GLS - predictors selection - stepwise

full_model_predictors <- c("population", "emp_pct", "hicp", "risk_poverty_pct", 
                           "gdp_per_cap", "an_ind_rate", "immigration", "emmigration", "year*state")
full_model_formula <- paste("hdi", paste(full_model_predictors, collapse=' + '), sep=' ~ ')
full_model_formula <- as.formula(full_model_formula)

gls_compsymm_full <- gls(full_model_formula,
                         dataset.normalized, 
                         corCompSymm(form=~1|country), 
                         varIdent(form=~1|year), 
                         method="ML")

gls_corar1_full <- gls(full_model_formula,
                        dataset.longit, 
                        corAR1(form=~1|country), 
                        varIdent(form=~1|year), 
                        method="ML")

comp_symm_stepwise<-stepAIC(gls_compsymm_full, 
                    direction="both", 
                    trace=0)#, 
                    #k=log(nrow(dataset.longit)))

corar1_stepwise<-stepAIC(gls_corar1_full, 
                         direction="both", 
                         trace=0)#,
                         #k=log(nrow(dataset.longit)))

#### GLS - correlation and weights selection

selected_predictors <- c("population", "emp_pct", "risk_poverty_pct", "gdp_per_cap")
gls_ar1_unstr <- fit_gls(selected_predictors, 
                         dataset.longit, 
                         corAR1, 
                         varIdent(form=~1|year))

gls_ar1_constr <- fit_gls(selected_predictors, 
                          dataset.longit, 
                          corAR1, 
                          varIdent(form=~1))

gls_comp_symm_unstr <- fit_gls(selected_predictors, 
                               dataset.longit, 
                               corCompSymm, 
                               varIdent(form=~1|year))

gls_comp_symm_constr <- fit_gls(selected_predictors, 
                                dataset.longit, 
                                corCompSymm, 
                                varIdent(form=~1))

ar1_lrt <- lrt(gls_ar1_unstr, gls_ar1_constr)
comp_symm_lrt <- lrt(gls_com_symm_unstr, gls_com_symm_constr)

print(c(AIC(gls_ar1_unstr), AIC(gls_comp_symm_unstr)))

#### Mixed effects  

fit.rand_slp.full <- lme(hdi ~ year*state + population + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration + emmigration, 
                         random=~1|country,
                         data=dataset.normalized,
                         method="ML")

fit.rand_slp.no_pop <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration + emmigration, 
                           random=~1|country,
                           data=dataset.normalized,
                           method="ML")

fit.rand_slp.no_emi <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration, 
                           random=~1|country,
                           data=dataset.normalized,
                           method="ML")

fit.rand_slp.no_emi <- lme(hdi ~ year*state + emp_pct + hicp + risk_poverty_pct + gdp_per_cap + an_ind_rate + immigration, 
                           random=~1|country,
                           data=dataset.normalized,
                           method="ML")

fit.rand_slp.reduced <- lme(hdi ~ year*state + emp_pct + risk_poverty_pct + gdp_per_cap, 
                           random=~1|country,
                           data=dataset.normalized,
                           method="ML")


anova(fit.rand_slp.full, fit.rand_slp.no_pop, fit.rand_slp.no_emi, fit.rand_slp.reduced)

summary(fit.rand_slp.reduced)

anova(fit.rand_slp.reduced)

model_lmr4 <- lmer(hdi ~ year*state + emp_pct + risk_poverty_pct + gdp_per_cap + (1| country),
                   dataset.normalized)

plot_model(model_lmr4, 
           type = "re", 
           show.values = TRUE, 
           value.offset = 0.3, 
           value.size = 3,
           line.size = 0.5, 
           dot.size = 1, 
           xlim=c(-0.1, 0.1))

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
