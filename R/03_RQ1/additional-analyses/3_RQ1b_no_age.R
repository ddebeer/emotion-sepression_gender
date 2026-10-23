# RQ1 b

# load packages
library(lmerTest)
library(emmeans)
source("R/03_RQ1/99_do_TOST.R")

# read data
data <- readRDS("Data/person-level-data.RDS")
data$er <- data$er_pm / 100

# RQ1 a - Gaussian model ----

## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_er_both <- lmer(er ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                       (1 + female_pd + neg_aff_pd | study),
                     data = data)

fit_er_neg_aff <- lmer(er ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                          (1 + neg_aff_pd | study),
                        data = data)

fit_er_female <- lmer(er ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                         (1 + female_pd | study),
                       data = data)

fit_er <- lmer(er ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                  (1 | study),
                data = data)

# compare fit of models
anova_er <- anova(fit_er, fit_er_neg_aff, fit_er_female, fit_er_both)
knitr::kable(anova_er)

# => model with also random slope for neg_aff provides best fit


### diagnostic checks ----
performance::check_model(fit_er_neg_aff)


### test expected marginal differences between males and females ----
means <- emmeans::emmeans(fit_er_neg_aff, specs = ~ female_pd)
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

# tost
knitr::kable(do_TOST(fit_er_neg_aff, covar = "female_pd", sesoi = sesoi, type = "gauss"), digits = 4)






#*****************************************************************************#






# RQ1 a - beta regression ----
library(glmmTMB)
data$er_b <- (0.0005 + data$er) / 1.001

## using female dummy (0 - 1) ----

### model selection ----
# random structure from complex to intercept only
fit_er_b_both <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                            (1 + neg_aff_pd + female_pd | study),
                          dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd,
                          data = data,
                          family = beta_family(link = "logit"))

fit_er_b_neg_aff <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                               (1 + neg_aff_pd | study),
                             dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd,
                             data = data,
                             family = beta_family(link = "logit"))

fit_er_b_female <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                              (1 + female_pd | study),
                            dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd,
                            data = data,
                            family = beta_family(link = "logit"))

fit_er_b <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                       (1 | study),
                     dispformula = ~ 1,
                     data = data,
                     family = beta_family(link = "logit"))



# compare fit of models
anova_er_b <- anova(fit_er_b, fit_er_b_neg_aff, fit_er_b_female, fit_er_b_both)
knitr::kable(anova_er_b)

# => model with random slope for neg_aff provides best fit


# => no evidence for interaction

# random effects for dispersion
fit_er_b_2 <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                                (1 + neg_aff_pd | study),
                              dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                                (1  | study),
                              data = data,
                              family = beta_family(link = "logit"))

fit_er_b_3 <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                        (1 + neg_aff_pd | study),
                      dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd + neg_aff_pd +
                        (1 + neg_aff_pd| study),
                      data = data,
                      family = beta_family(link = "logit"))

anova_er3 <- anova(fit_er_b_neg_aff, fit_er_b_2, fit_er_b_3)
knitr::kable(anova_er3)

# => evidence for random intercept and slope for dispersion

### diagnostic checks ----
performance::check_model(fit_er_b_3)



### test expected marginal differences between males and females ----
means <- emmeans::emmeans(fit_er_b_neg_aff2, specs = ~ female_pd, type = "response")
contr <- emmeans::contrast(means, method = "pairwise")

estimate <- summary(contr)


### do tost ----
knitr::kable(do_TOST(fit_er_b_3, covar = "female_pd",  sesoi = sesoi, type = "beta"), digits = 4)


summary(fit_er_b_3)


saveRDS(fit_er_b_3, "R/03_RQ1/reported_models/RQ1b.RDS")
