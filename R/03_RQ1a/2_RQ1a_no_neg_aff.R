# RQ1a - no negative affect

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


# RQ1 a - beta regression ----

data$erq_b <- (0.0005 + data$erq) / 1.001


## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_erq_b_age_female <- glmmTMB(erq_b ~ 1 + female_sd + female_pd +
                                  age_sd + age_pd +
                                  (1 + female_pd + age_pd | study),
                                dispformula = ~ 1 + female_sd + female_pd +
                                  age_sd + age_pd,
                                data = data[data$has_erq,],
                                family = beta_family(link = "logit"))


fit_erq_b_age <- glmmTMB(erq_b ~ 1 + female_sd + female_pd +
                           age_sd + age_pd +
                           (1 + age_pd | study),
                         dispformula = ~ 1 + female_sd + female_pd +
                           age_sd + age_pd,
                         data = data[data$has_erq,],
                         family = beta_family(link = "logit"))

fit_erq_b_female <- glmmTMB(erq_b ~ 1 + female_sd + female_pd +
                              age_sd + age_pd +
                              (1 + female_pd | study),
                            dispformula = ~ 1 + female_sd + female_pd +
                              age_sd + age_pd,
                            data = data[data$has_erq,],
                            family = beta_family(link = "logit"))

fit_erq_b <- glmmTMB(erq_b ~ 1 + female_sd + female_pd +
                       age_sd + age_pd +
                       (1 | study),
                     dispformula = ~ 1 + female_sd + female_pd +
                       age_sd + age_pd,
                     data = data[data$has_erq,],
                     family = beta_family(link = "logit"))



# compare fit of models
anova_erq_b <- anova(fit_erq_b, fit_erq_b_female, fit_erq_b_age,
                     fit_erq_b_age_female)
knitr::kable(anova_erq_b)

# => model with only random intercept provides best fit



# random effects for dispersion
fit_erq_b_2 <- glmmTMB(erq_b ~ 1 + female_sd + female_pd +
                         age_sd + age_pd +
                         (1 | study),
                       dispformula = ~ 1 + female_sd + female_pd +
                         age_sd + age_pd +
                         (1  | study),
                       data = data[data$has_erq,],
                       family = beta_family(link = "logit"))

anova_erq_b <- anova(fit_erq_b, fit_erq_b_2, fit_erq_b_female, fit_erq_b_age,
                     fit_erq_b_age_female)
knitr::kable(anova_erq_b)


saveRDS(anova_erq_b, "R/03_RQ1a/reported_models/anova_erq_b_no_neg_aff.RDS")

# => evidence for random intercept for dispersion

# check fixed effects;
car::Anova(fit_erq_b_2, type = "III")

summary(fit_erq_b_2)



### diagnostic checks ----
performance::check_model(fit_erq_b_2)



### do tost ----
# setting the SESOI

# overall gender difference in Gross & John (2003) and in Rogier (2019)
men <- c(3.64, 3.70)
women <- c(3.14, 3.18)
# transform to [0-1]-scale
diff <- ((men - women)) / 6
# take half and average over two studies
sesoi <- mean(diff / 2)

TOST_erq_b <- do_TOST(fit_erq_b_2, covar = "female_pd",  sesoi = sesoi, type = "beta")

knitr::kable(TOST_erq_b, digits = 4)

saveRDS(TOST_erq_b, "R/03_RQ1a/reported_models/TOST_erq_b_no_neg_aff.RDS")




#*****************************************************************************#



### model selection ----
# random structure from complex to intercept only
fit_erq_age_female <- lmer(erq ~ 1 + female_sd + female_pd +
                             age_sd + age_pd +
                             (1 + female_pd + age_pd | study),
                           data = data[data$has_erq,])

fit_erq_age <- lmer(erq ~ 1 + female_sd + female_pd +
                      age_sd + age_pd +
                      (1 + age_pd | study),
                    data = data[data$has_erq,])

fit_erq_female <- lmer(erq ~ 1 + female_sd + female_pd +
                         age_sd + age_pd +
                         (1 + female_pd | study),
                       data = data[data$has_erq,])

fit_erq <- lmer(erq ~ 1 + female_sd + female_pd +
                  age_sd + age_pd +
                  (1 | study),
                data = data[data$has_erq,])

# compare fit of models
anova_erq <- anova(fit_erq, fit_erq_female, fit_erq_age,
                   fit_erq_age_female)
knitr::kable(anova_erq)

# => intercept only model provides best fit

### diagnostic checks ----
performance::check_model(fit_erq)

car::Anova(fit_erq, type = "III")

summary(fit_erq)



### do tost ----
# setting the SESOI

# overall gender difference in Gross & John (2003) and in Rogier (2019)

knitr::kable(do_TOST(fit_erq, covar = "female_pd", sesoi = sesoi, type = "gauss"), digits = 4)









