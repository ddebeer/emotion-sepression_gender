library(dplyr)


# prepare data FEEL Study
data <- read.csv("Data-raw/data_Everyday_emotion_regulation.csv")


# recode and select part 1
data <- data |> mutate(
  age = AGE_BL,
  gender = case_when(
    GENDER_BL == "f" ~ "male",
    GENDER_BL == "m" ~ "female",
    .default = "other"),
  female = ifelse(gender == "female", 1, 0),
  er = (SUPR_ES - 1) / 5 * 100,
  er_b = (0.05 + er) / 100.1,
  anger = (ANG_ES - 1) / 5,
  stress = (STR_ES - 1) / 5,
  depr = (DEP_ES - 1) / 5,
  anx = (ANX_ES - 1) / 5,

  study = dataset
  ) |>
  rowwise() |>
  mutate(
    erq = (mean(c(ERQ_2_BL, ERQ_4_BL, ERQ_6_BL, ERQ_9_BL), na.rm = TRUE) - 1) / 6,
    neg_aff = mean(c(anger, anx, depr, stress), na.rm = TRUE)
  ) |>
  ungroup()


# reliabilities

## Negative Affect
library(multilevelTools)
omega_NA <- omegaSEM(c("anger", "stress", "anx", "depr"),
                     id = "UUID",
                     data = data)




# person-level data
data_pp <- data |> group_by(UUID) |>
  summarise(age = mean(age),
            gender = head(gender, 1),
            female = head(female, 1),
            erq = mean(erq, na.rm = TRUE),
            ERQ_2_BL = head(ERQ_2_BL, 1),
            ERQ_4_BL = head(ERQ_4_BL, 1),
            ERQ_6_BL = head(ERQ_6_BL, 1),
            ERQ_9_BL = head(ERQ_9_BL, 1),
            er_pm = mean(er, na.rm = TRUE),
            anger_pm = mean(anger, na.rm = TRUE),
            neg_aff_pm = mean(neg_aff, na.rm = TRUE),
            study = head(study, 1)
            )


# reliabilities

## ERQ (RESS)
alpha <- psych::alpha(data |> select(ERQ_2_BL, ERQ_4_BL, ERQ_6_BL, ERQ_9_BL))
alpha_erq <- alpha$total


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
  select(age, gender, erq,
         er, er_b,
         anger,
         neg_aff, UUID,
         Date_Local, Time_Local, study)|>
  filter(gender != "other",
         !is.na(er))



data <- left_join(data, data_pp) |>
  mutate(
    anger_pmc = anger - anger_pm,
    neg_aff_pmc = neg_aff - neg_aff_pm
  ) |> group_by(ppID, Date_Local) |>
  mutate(
    anger_pmc_lag = lag(anger_pmc),
    neg_aff_pmc_lag = lag(neg_aff_pmc),
    time = strptime(Time_Local, "%H:%M:%S"),
    time_diff = as.numeric(time - lag(time), unit = "secs")
  )




saveRDS(data_pp, "Data/person-level-data/everyday.RDS")
saveRDS(data, "Data/beep-level-data/everyday.RDS")
saveRDS(list(alpha_erq = alpha_erq,
             which_erq = "ERQ",
             omega_NA = omega_NA$Results,
             descriptives0 = descriptives0,
             descriptives = descriptives), "Data/descriptives/everyday.RDS")
rm(data, data_pp, descriptives, alpha, alpha_erq, omega_NA)
