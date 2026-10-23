# running this script gives the test results that are reported in the manuscript

# load TOST ----
library(emmeans)
source("R/03_RQ1/99_do_TOST.R")


# load data
data <- readRDS("Data/person-level-data.RDS")
data$er <- data$er_pm / 100
data$er_b <- (0.0005 + data$er) / 1.001


# load best fitting models
fit_RQ1a <- readRDS("R/03_RQ1/reported_models/RQ1a.RDS")
fit_RQ1b <- readRDS("R/03_RQ1/reported_models/RQ1b.RDS")

# compute SESOI ----
# overall gender difference in Gross & John (2003) and in Rogier (2019)
men <- c(3.64, 3.70)
women <- c(3.14, 3.18)
# transform to [0-1]-scale
diff <- ((men - women)) / 6
# take half and average over two studies
sesoi <- mean(diff / 2)

# do tost ----

## RQ1 a ----
knitr::kable(do_TOST(fit_RQ1a, covar = "female_pd", sesoi = sesoi, type = "beta"), digits = 4)




## RQ1 b ----
knitr::kable(do_TOST(fit_RQ1b, covar = "female_pd", sesoi = sesoi, type = "beta"), digits = 4)

