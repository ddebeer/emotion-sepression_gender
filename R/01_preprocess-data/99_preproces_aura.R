library(dplyr)


# prepare data FEEL Study
data <- read.csv("Data-raw/data_Assessing_Understanding_and_Regulating_Affect_AURA.csv")



# recode and select part 1
data <- data |> mutate(
  age = AGE_BL,
  gender = case_when(
    GENDER_BL == "1" ~ "female",
    GENDER_BL == "2" ~ "male",
    .default = "other"),
  female = ifelse(gender == "female", 1, 0),
  er = SUPR_ES,
  er_b = (0.05 + er) / 100.1,
  sad = SAD_ES / 100,
  anx = ANX_ES / 100,
  dull = DULL_ES / 100,
  irr = IRR_ES / 100,
  study = dataset
  ) |>
  rowwise() |>
  mutate(
    neg_aff = mean(c(anx, dull, irr, sad), na.rm = TRUE)
  ) |>
  ungroup()



# reliabilities

## Negative Affect
library(multilevelTools)
omega_NA <- omegaSEM(c("dull", "irr", "sad", "anx"),
                     id = "UUID",
                     data = data)


# person-level data
data_pp <- data |> group_by(UUID) |>
  summarise(age = mean(age, na.rm = TRUE),
            gender = head(gender, 1),
            female = head(female, 1),
            er_pm = mean(er, na.rm = TRUE),
            sad_pm = mean(sad, na.rm = TRUE),
            neg_aff_pm = mean(neg_aff, na.rm = TRUE),
            study = head(study, 1)
            )




# other desciptives
descriptives0 <- data_pp |> summarize(
  n = n(),
  other = sum(gender == "other"),
  other_pct = other/n * 100,
  female = sum(gender == "female"),
  female_pct_no_other = female / (n - other),
  male = sum(gender == "male"),
  female_pct_no_other = female / (n - other),
  m_age = mean(age, na.rm = TRUE),
  sd_age = sd(age, na.rm = TRUE),
  min_age = min(age, na.rm = TRUE),
  max_age = max(age, na.rm = TRUE)

)

descriptives <- data_pp |>
  filter(gender != "other") |>  summarize(
  n = n(),
  female = sum(gender == "female"),
  female_pct_no_other = female / n,
  male = sum(gender == "male"),
  female_pct_no_other = female / n,
  m_age = mean(age, na.rm = TRUE),
  sd_age = sd(age, na.rm = TRUE),
  min_age = min(age, na.rm = TRUE),
  max_age = max(age, na.rm = TRUE)
)



# transform after removing others
data_pp <- data_pp |> filter(gender != "other") |>
  mutate (age_z = scale(age)[,1],
          age_m = mean(age),
          female_c = scale(female, scale = FALSE)[,1],
          female_m = mean(female),
          ppID = paste0(study, row_number()),
          m_f = ifelse(gender == "male", .5, -.5),
          type = "cont")


# filter data
data <- data |>
  select(age, gender,
         er, er_b,
         sad,
         neg_aff, UUID,
         Date_Local, Time_Local, study)|>
  filter(gender != "other")



data <- left_join(data, data_pp) |>
  mutate(
    sad_pmc = sad - sad_pm,
    neg_aff_pmc = neg_aff - neg_aff_pm
  ) |> group_by(ppID, Date_Local) |>
  mutate(
    sad_pmc_lag = lag(sad_pmc),
    neg_aff_pmc_lag = lag(neg_aff_pmc),
    time = strptime(Time_Local, "%H:%M:%S"),
    time_diff = as.numeric(time - lag(time), units = "secs")
  )





saveRDS(data_pp, "Data/person-level-data/aura.RDS")
saveRDS(data, "Data/beep-level-data/aura.RDS")
saveRDS(list(alpha_erq = NULL,
             which_erq = "none",
             omega_NA = omega_NA$Results,
             descriptives0 = descriptives0,
             descriptives = descriptives), "Data/descriptives/aura.RDS")
rm(data, data_pp, descriptives, alpha, alpha_erq, omega, omega_NA)

