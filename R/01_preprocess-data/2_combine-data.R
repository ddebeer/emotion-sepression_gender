# Combine data

# this file combines the preprocessed data
# both the person level data as well as the ESM level data

# in addition, the descriptive statistics are combined across studies


# install required packages
if(!require("dplyr")) install.packages("dplyr")


library(dplyr)

# list files
data_files <- list.files("Data/beep-level-data", full.names = TRUE)
data_pp_files <- list.files("Data/person-level-data", full.names = TRUE)
descriptives_files <- list.files("Data/descriptives", full.names = TRUE)


## ESM level data ----

data <- do.call(rbind, lapply(data_files, function(file){
  #browser()
  data <- readRDS(file)

  # check if anger is present
  if("anger" %in% names(data)) {
    data$has_anger <- TRUE
  } else {
    data$has_anger <- FALSE
    data$anger_pm <- data$anger_pmc <- data$anger_pmc_lag <- 0
  }

  # check if sad is present
  if("sad" %in% names(data)) {
    data$has_sad <- TRUE
  } else {
    data$has_sad <- FALSE
    data$sad_pm <- data$sad_pmc <- data$sad_pmc_lag <- 0
  }

  # check if erq is present
  if("erq" %in% names(data)) {
    data$has_erq <- TRUE
  } else {
    data$has_erq <- FALSE
    data$erq <- 0
  }

  # check if type is binary
  if(all(data$type == "binary")) {
    print(file)
    data$er_b <- NA
  }


  # check
  if(any(data$er_b >= 1, na.rm = TRUE)) {
    browser()
  }


  data <- data |> ungroup() |>
    select(er, er_b, age, age_z, age_m, m_f, female_m, female_c,
           erq, has_erq,
           neg_aff_pm, neg_aff_pmc, neg_aff_pmc_lag,
           anger_pm, anger_pmc, anger_pmc_lag, has_anger,
           sad_pm, sad_pmc, sad_pmc_lag, has_sad,
           ppID, study, type, time_diff)
  print(paste0(file, "   OK"))
  data
}))

# save combined data
saveRDS(data, "Data/beep-level-data/data.RDS")


# person level data ----

data_pp <- do.call(rbind, lapply(data_pp_files, function(file){

  data <- readRDS(file)

  # check if anger is present
  if("anger_pm" %in% names(data)) {
    data$has_anger <- TRUE
  } else {
    data$has_anger <- FALSE
    data$anger_pm <- NA
  }

  # check if sad is present
  if("sad_pm" %in% names(data)) {
    data$has_sad <- TRUE
  } else {
    data$has_sad <- FALSE
    data$sad_pm <- NA
  }

  # check if erq is present
  if("erq" %in% names(data)) {
    data$has_erq <- TRUE
  } else {
    data$has_erq <- FALSE
    data$erq <- NA
  }

  # check if type is binary
  if(all(data$type == "binary")) {
    data$er_pm <- data$er_pm * 100
  }

  data <- data |> ungroup() |>
    select(er_pm, age, age_z, age_m, m_f, female, female_m, female_c,
           erq, has_erq,
           neg_aff_pm,
           anger_pm, has_anger,
           sad_pm, has_sad,
           ppID, study, type, age, gender)
  print(paste0(file, "   OK"))
  data
}))


saveRDS(data_pp, "Data/person-level-data/data.RDS")




# descriptives ----

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
