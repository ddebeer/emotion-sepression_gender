# RQ1a

# load packages
library(lmerTest)
library(emmeans)
source("R/03_RQ1/99_do_TOST.R")

# read data
data <- readRDS("Data/person-level-data.RDS")

# RQ1 a - Gaussian model ----

## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_erq_all <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                       age_sd + age_pd +
                       (1 + female_pd + neg_aff_pd + age_pd | study),
                     data = data[data$has_erq,])

fit_erq_age_neg_aff <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                      age_sd + age_pd +
                      (1 + neg_aff_pd + age_pd | study),
                    data = data[data$has_erq,])

fit_erq_age_female <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                              age_sd + age_pd +
                              (1 + female_pd + age_pd | study),
                            data = data[data$has_erq,])

fit_erq_neg_aff_female <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                             age_sd + age_pd +
                             (1 + female_pd + neg_aff_pd | study),
                           data = data[data$has_erq,])

fit_erq_age <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                             age_sd + age_pd +
                             (1 + age_pd | study),
                           data = data[data$has_erq,])

fit_erq_neg_aff <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                          age_sd + age_pd +
                          (1 + neg_aff_pd | study),
                        data = data[data$has_erq,])

fit_erq_female <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                         age_sd + age_pd +
                         (1 + female_pd | study),
                       data = data[data$has_erq,])

fit_erq <- lmer(erq ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                  age_sd + age_pd +
                  (1 | study),
                data = data[data$has_erq,])

# compare fit of models
anova_erq <- anova(fit_erq, fit_erq_female, fit_erq_neg_aff, fit_erq_age,
                   fit_erq_age_female, fit_erq_age_neg_aff, fit_erq_neg_aff_female,
                   fit_erq_all)
knitr::kable(anova_erq)

# => intercept only model provides best fit

### diagnostic checks ----
performance::check_model(fit_erq)



### test expected marginal differences between males and females ----
means <- emmeans::emmeans(fit_erq, specs = ~ female_pd)
contr <- emmeans::contrast(means, method = "pairwise")
contr
confint(contr)

estimate <- summary(contr)$estimate



### do tost ----
# setting the SESOI

# overall gender difference in Gross & John (2003) and in Rogier (2019)
men <- c(3.64, 3.70)
women <- c(3.14, 3.18)
# transform to [0-1]-scale
diff <- ((men - women)) / 6
# take half and average over two studies
sesoi <- mean(diff / 2)


knitr::kable(do_TOST(fit_erq, covar = "female_pd", sesoi = sesoi, type = "gauss"), digits = 4)


summary(fit_erq)



#*****************************************************************************#






# RQ1 a - beta regression ----
library(glmmTMB)
data$erq_b <- (0.0005 + data$erq) / 1.001

## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_erq_b_all <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                            age_sd + age_pd +
                            (1 + female_pd + neg_aff_pd + age_pd | study),
                       dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                         age_sd + age_pd,
                       data = data[data$has_erq,],
                       family = beta_family(link = "logit"))

fit_erq_b_age_neg_aff <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                              age_sd + age_pd +
                              (1 + neg_aff_pd + age_pd | study),
                              dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                                age_sd + age_pd,
                              data = data[data$has_erq,],
                              family = beta_family(link = "logit"))

fit_erq_b_age_female <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                             age_sd + age_pd +
                             (1 + female_pd + age_pd | study),
                             dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                               age_sd + age_pd,
                             data = data[data$has_erq,],
                             family = beta_family(link = "logit"))

fit_erq_b_neg_aff_female <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                                 age_sd + age_pd +
                                 (1 + female_pd + neg_aff_pd | study),
                                 dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                                   age_sd + age_pd,
                                 data = data[data$has_erq,],
                                 family = beta_family(link = "logit"))

fit_erq_b_age <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                      age_sd + age_pd +
                      (1 + age_pd | study),
                      dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                        age_sd + age_pd,
                      data = data[data$has_erq,],
                      family = beta_family(link = "logit"))


fit_erq_b_neg_aff <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                               age_sd + age_pd +
                            (1 + neg_aff_pd | study),
                          dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                            age_sd + age_pd,
                          data = data[data$has_erq,],
                          family = beta_family(link = "logit"))

fit_erq_b_female <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                              age_sd + age_pd +
                           (1 + female_pd | study),
                         dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                           age_sd + age_pd,
                         data = data[data$has_erq,],
                         family = beta_family(link = "logit"))

fit_erq_b <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                       age_sd + age_pd +
                    (1 | study),
                  dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                    age_sd + age_pd,
                  data = data[data$has_erq,],
                  family = beta_family(link = "logit"))



# compare fit of models
anova_erq_b <- anova(fit_erq_b, fit_erq_female, fit_erq_b_neg_aff, fit_erq_b_age,
                   fit_erq_b_age_female, fit_erq_b_age_neg_aff, fit_erq_b_neg_aff_female,
                   fit_erq_b_all)
knitr::kable(anova_erq_b)

# => model with random intercept provides best fit



# random effects for dispersion
fit_erq_b_2 <- glmmTMB(erq_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                         age_sd + age_pd +
                             (1 | study),
                           dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                         age_sd + age_pd +
                             (1  | study),
                           data = data[data$has_erq,],
                           family = beta_family(link = "logit"))

anova_er3 <- anova(fit_erq_b, fit_erq_b_2)
knitr::kable(anova_er3)

# => no evidence for random slope for dispersion

### diagnostic checks ----
performance::check_model(fit_erq_b_neg_aff)



### test expected marginal differences between males and females ----
means <- emmeans::emmeans(fit_erq_b_neg_aff, specs = ~ female_pd, type = "response")
contr <- emmeans::contrast(means, method = "pairwise")

estimate <- summary(contr)


### do tost ----
## the 0.045 is the lower bound of the confidence intervall of the erq analysis
knitr::kable(do_TOST(fit_erq_b, covar = "female_pd",  sesoi = sesoi, type = "beta"), digits = 4)


summary(fit_erq_b)
