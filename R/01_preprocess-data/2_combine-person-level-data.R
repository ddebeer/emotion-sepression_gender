# Combine and person-level data and compute person-level and study-level covariates

# This R-script combines the preprocessed person-level data and computes
# person-level and study-level covariates

# the person-level covariates are deviations from the study mean.
# the study-level covariates are (average) deviations from the overal mean
# this is done for both numeric and categorical covariates. For the latter, the
# the means and deviations are computed for the dummy/contrast variables.


# person level deviations from study means have suffix pd (person deviation)
# study level deviations from the overall mean have suffix sd (study deviation)


# install required packages ----
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

# list files
data_pp_files <- list.files("Data/person-level-data", full.names = TRUE)


# Combine person level data ----
data <- do.call(rbind, lapply(data_pp_files, function(file){

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

# Compute person-level and study-level covariates ---

# compute study means for
# - age
# - female
# - m_f
# - neg_aff_pm

study_means <- data |> summarize(
  age_sm = mean(age, na.rm = TRUE),
  female_sm = mean(female),
  m_f_sm = mean(m_f),
  neg_aff_sm = mean(neg_aff_pm),
  .by = study
)



# add study means to the data
data <- left_join(data, study_means)

# compute study deviations
data <- data |> mutate(
  age_sd = age_sm - mean(age, na.rm = TRUE),
  female_sd = female_sm - mean(female),
  m_f_sd = m_f_sm - mean(m_f),
  neg_aff_sd = neg_aff_sm - mean(neg_aff_pm),
)

# compute person deviations
data <- data |> mutate(
  age_pd = age - mean(age_sm, na.rm = TRUE),
  female_pd = female - mean(female_sm),
  m_f_pd = m_f - mean(m_f_sm),
  neg_aff_pd = neg_aff_pm - mean(neg_aff_sm),
)


# select relevant covariates + outcomes
data <- data |> select(er_pm, erq, has_erq, study, m_f,
                       ends_with("_sd"),
                       ends_with("_pd"))

saveRDS(data, "Data/person-level-data.RDS")


