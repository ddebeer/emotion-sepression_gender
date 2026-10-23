# RQ1 b no negative affect

# load packages
library(lmerTest)
library(emmeans)

# read data
data <- readRDS("Data/person-level-data.RDS")
data$er_g <- data$er_pm / 100

# gaussian models ----

### using female ----
fit_er <- lmer(er_g ~ 1 + female_sd + female_pd + neg_aff_sd +
                  (1 | study),
                data = data[data$has_erq,])

fit_er_female <- lmer(er_g ~ 1 + female_sd + female_pd + neg_aff_sd +
                        (1 + female_pd | study),
                    data = data[data$has_erq,])

anova_er <- anova(fit_er, fit_er_female)
knitr::kable(anova_er)

summary(fit_er)

# save best fitting model
saveRDS(fit_er_neg_aff, "Results/RQ1_b_g_female.RDS")


# do TOST
## the 0.047 is the lower bound of the 95% confidence intervall of the erq analysis
do_TOST(fit_er, covar = "female_pd",  meaningfull_diff = .047, type = "gauss")















### using m_f ----
fit_er <- lmer(er_g ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                (1 | study),
               data = data[data$has_erq,])


fit_er_m_f <- lmer(er_g ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                     (1 + m_f_pd | study),
                      data = data[data$has_erq,])

anova_er <- anova(fit_er, fit_er_m_f)
knitr::kable(anova_er)

summary(fit_er)


# save best fitting model
saveRDS(fit_er_neg_aff, "Results/RQ1_b_g_m_f.RDS")


# do TOST
## the 0.047 is the lower bound of the 95% confidence intervall of the erq analysis
do_TOST(fit_er, covar = "m_f_pd",  meaningfull_diff = .047, type = "gauss")











# beta regression ----
library(glmmTMB)
data$er_b <- (0.05 + data$er_pm) / 100.1

## using female ----
fit_er <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd +
                   (1 | study),
                  dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd,
                  data = data[data$has_erq,],
                  family = beta_family(link = "logit"))

fit_er_female <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd +
                           (1 + female_pd | study),
                         dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd,
                         data = data[data$has_erq,],
                         family = beta_family(link = "logit"))


anova_er <- anova(fit_er, fit_er_female)
knitr::kable(anova_er)


# random effects for dispersion
fit_er2 <- glmmTMB(er_b ~ 1 + female_sd + female_pd + neg_aff_sd +
                        (1 | study),
                      dispformula = ~ 1 + female_sd + female_pd + neg_aff_sd +
                        (1  | study),
                      data = data[data$has_erq,],
                      family = beta_family(link = "logit"))

anova_er3 <- anova(fit_er, fit_er2)
knitr::kable(anova_er3)


summary(fit_er2)

performance::check_model(fit_er2)



# save best fitting model
saveRDS(fit_er_neg_aff2, "Results/RQ1_b_b_female.RDS")


# do TOST
## the 0.036 is the lower bound of the confidence itervall of the erq analysis
do_TOST(fit_er2, covar = "female_pd",  meaningfull_diff = .047, type = "beta")












## using m_f ----
fit_er <- glmmTMB(er_b ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                   (1 | study),
                  dispformula = ~ 1 + m_f_sd + m_f_pd + neg_aff_sd,
                  data = data[data$has_erq,],
                  family = beta_family(link = "logit"))

fit_er_m_f <- glmmTMB(er_b ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                        (1 + m_f_pd | study),
                         dispformula = ~ 1 + m_f_sd + m_f_pd + neg_aff_sd,
                         data = data[data$has_erq,],
                         family = beta_family(link = "logit"))




anova_er <- anova(fit_er, fit_er_m_f)
knitr::kable(anova_er)


# random effects for dispersion
fit_er2 <- glmmTMB(er_b ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                       (1 | study),
                     dispformula = ~ 1 + m_f_sd + m_f_pd + neg_aff_sd +
                       (1  | study),
                     data = data[data$has_erq,],
                     family = beta_family(link = "logit"))

anova_er3 <- anova(fit_er, fit_er2)
knitr::kable(anova_er3)


summary(fit_er2)

performance::check_model(fit_er_neg_aff2)


# save best fitting model
saveRDS(fit_er2, "Results/RQ1_b_b_m_f.RDS")


# do TOST
## the 0.036 is the lower bound of the confidence itervall of the erq analysis
do_TOST(fit_er2, covar = "m_f_pd",  meaningfull_diff = .047, type = "beta")




