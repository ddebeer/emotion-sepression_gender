# Combine study descriptives

# this file combines the descriptives of the studies

# install required packages
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

# list files
descriptives_files <- list.files("Data/descriptives", full.names = TRUE)


## omega reliabilities ----
omega <- do.call(rbind, lapply(descriptives_files, function(file){
  descr <- readRDS(file)
  study <- gsub("Data/descriptives/", "", file, fixed = TRUE)
  study <- gsub(".RDS", "", study, fixed = TRUE)

  if(is.null(descr$omega_NA)) return(NULL)
  return(cbind(descr$omega_NA, study = study))
}))

## cronbach's alphas ----
alpha <- do.call(rbind, lapply(descriptives_files, function(file){
  descr <- readRDS(file)
  study <- gsub("Data/descriptives/", "", file, fixed = TRUE)
  study <- gsub(".RDS", "", study, fixed = TRUE)

  if(is.null(descr$alpha_erq)) return(NULL)
  return(cbind(descr$alpha_erq, study = study, scale = descr$which_erq))
}))

## descriptive statistics ----
descr <- do.call(rbind, lapply(descriptives_files, function(file){
  descr <- readRDS(file)
  study <- gsub("Data/descriptives/", "", file, fixed = TRUE)
  study <- gsub(".RDS", "", study, fixed = TRUE)

  return(cbind(descr$descriptives, study = study))
}))

descr$female_pct_no_other <- descr$female_pct_no_other * 100

## descriptive statistics before removing the "other" categories from gender ----
descr0 <- do.call(rbind, lapply(descriptives_files, function(file){
  descr <- readRDS(file)
  study <- gsub("Data/descriptives/", "", file, fixed = TRUE)
  study <- gsub(".RDS", "", study, fixed = TRUE)

  return(cbind(descr$descriptives0, study = study))
}))

descr0$female_pct_no_other <- descr0$female_pct_no_other * 100




saveRDS(omega, "Data/descriptives/omega.RDS")
saveRDS(alpha, "Data/descriptives/alpha.RDS")
saveRDS(descr, "Data/descriptives/descr.RDS")
saveRDS(descr0, "Data/descriptives/descr0.RDS")
