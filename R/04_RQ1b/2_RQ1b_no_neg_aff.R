# RQ1 b no negative affect

# install required packages
if(!require("lmerTest")) install.packages("lmerTest")
if(!require("glmmTMB")) install.packages("glmmTMB")
if(!require("emmeans")) install.packages("emmeans")
if(!require("preformance")) install.packages("performance")
if(!require("knitr")) install.packages("knitr")
if(!require("car")) install.packages("car")


# load packages
library(lmerTest)
library(glmmTMB)
library(emmeans)

source("R/99_do_TOST.R")

# read data
data <- readRDS("Data/person-level-data.RDS")
data$er <- data$er_pm / 100
data$er_b <- (0.0005 + data$er) / 1.001



# RQ1 b - beta regression ----


## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_er_b_all <- glmmTMB(er_b ~ 1 + female_sd + female_pd +
                          age_sd + age_pd +
                          (1 + female_pd + age_pd | study),
                        dispformula = ~ 1 + female_sd + female_pd +
                          age_sd + age_pd,
                        data = data,
                        family = beta_family(link = "logit"))

fit_er_b_age <- glmmTMB(er_b ~ 1 + female_sd + female_pd +
                          age_sd + age_pd +
                          (1 + age_pd | study),
                        dispformula = ~ 1 + female_sd + female_pd +
                          age_sd + age_pd,
                        data = data,
                        family = beta_family(link = "logit"))

fit_er_b_female <- glmmTMB(er_b ~ 1 + female_sd + female_pd +
                             age_sd + age_pd +
                             (1 + female_pd | study),
                           dispformula = ~ 1 + female_sd + female_pd +
                             age_sd + age_pd,
                           data = data,
                           family = beta_family(link = "logit"))

fit_er_b <- glmmTMB(er_b ~ 1 + female_sd + female_pd +
                      age_sd + age_pd +
                      (1 | study),
                    dispformula = ~ 1 + female_sd + female_pd +
                      age_sd + age_pd,
                    data = data,
                    family = beta_family(link = "logit"))



# compare fit of models
anova_er_b <- anova(fit_er_b, fit_er_b_female, fit_er_b_age,
                    fit_er_b_all)
knitr::kable(anova_er_b)

# => model with random intercept provides best fit


fit_er_b_2 <- glmmTMB(er_b ~ 1 + female_sd + female_pd +
                      age_sd + age_pd +
                      (1 | study),
                    dispformula = ~ 1 + female_sd + female_pd +
                      age_sd + age_pd +
                      (1 | study),
                    data = data,
                    family = beta_family(link = "logit"))



anova_er_b <- anova(fit_er_b, fit_er_b_female, fit_er_b_age,
                    fit_er_b_all, fit_er_b_2)
knitr::kable(anova_er_b)

# => evidence for random intercept and slope for dispersion

saveRDS(anova_er_b, "R/04_RQ1b/reported_models/anova_er_b_no_neg_aff.RDS")


# check fixed effects;
car::Anova(fit_er_b_2, type = "III")

summary(fit_er_b_2)



### diagnostic checks ----
performance::check_model(fit_er_b_2)



### do tost ----
# setting the SESOI

# overall gender difference in Gross & John (2003) and in Rogier (2019)
men <- c(3.64, 3.70)
women <- c(3.14, 3.18)
# transform to [0-1]-scale
diff <- ((men - women)) / 6
# take half and average over two studies
sesoi <- mean(diff / 2)

TOST_er_b <- do_TOST(fit_er_b_2, covar = "female_pd",  sesoi = sesoi, type = "beta")

knitr::kable(TOST_er_b, digits = 4)

saveRDS(TOST_er_b, "R/04_RQ1b/reported_models/TOST_er_b_no_neg_aff.RDS")








#*****************************************************************************#




# RQ1 b - Gaussian model ----

## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_er_all <- lmer(er ~ 1 + female_sd + female_pd +
                            age_sd + age_pd +
                            (1 + female_pd + age_pd | study),
                          data = data)

fit_er_age <- lmer(er ~ 1 + female_sd + female_pd +
                     age_sd + age_pd +
                     (1 + age_pd | study),
                   data = data)

fit_er_female <- lmer(er ~ 1 + female_sd + female_pd +
                        age_sd + age_pd +
                        (1 + female_pd | study),
                      data = data)

fit_er <- lmer(er ~ 1 + female_sd + female_pd +
                 age_sd + age_pd +
                 (1 | study),
               data = data)

# compare fit of models
anova_er <- anova(fit_er, fit_er_female, fit_er_age,
                  fit_er_all)
knitr::kable(anova_er)

# => model with random intercept provides best fit (AIC)

### diagnostic checks ----
performance::check_model(fit_er)



### do tost ----
# setting the SESOI

# overall gender difference in Gross & John (2003) and in Rogier (2019)
men <- c(3.64, 3.70)
women <- c(3.14, 3.18)
# transform to [0-1]-scale
diff <- ((men - women)) / 6
# take half and average over two studies
sesoi <- mean(diff / 2)


knitr::kable(do_TOST(fit_er, covar = "female_pd", sesoi = sesoi, type = "gauss"), digits = 4)












