# Compute person-level and study-level covariates

# this R-script computes person-level and study-level covariates

# the person-level covariates are deviations from the study mean.
# the study-level covariates are (average) deviations from the overal mean
# this is done for both numeric and categorical covariates. For the latter, the
# the means and deviations are computed for the dummy/contrast variables.


# person level deviations from study means have suffix pd (person deviation)
# study level deviations from the overal mean have suffix sd (study deviation)

library(dplyr)

data <- readRDS("Data/data_pp.RDS")


# compute study means for
# - age
# - female
# - m_f
# - neg_aff_pm

study_means <- data |> summarize(
  age_sm = mean(age),
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
