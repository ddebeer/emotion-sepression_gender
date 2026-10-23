library(dplyr)


# prepare data FEEL Study
data <- read.csv("Data-raw/data_Goals_Emotions_Moods_and_Strategies_GEMS_2024_v2.csv")

# recode and select part 1
data <- data |> mutate(
  age = AGE_BL,
  gender = case_when(
    GENDER_BL == "Woman" ~ "female",
    GENDER_BL == "Man" ~ "male",
    .default = "other"),
  female = ifelse(gender == "female", 1, 0),
  er = SUPR_ES,
  er_b = (0.05 + er) / 100.1,
  anger = ANG_ES / 100,
  stress = STR_ES / 100,
  sad = SAD_ES / 100,
  anx = ANX_ES / 100,
  guil = GUILT_ES / 100,
  tire = TIR_ES / 100,
  lone = LONE_ES / 100,
  study = dataset
  ) |>
  rowwise() |>
  mutate(
    erq = (mean(c(RESS_3_BL, RESS_11_BL, RESS_14_BL, RESS_20_BL), na.rm = TRUE) - 1) / 4,
    neg_aff = mean(c(anger, stress, sad, anx, guil, tire, lone), na.rm = TRUE)
  ) |>
  ungroup()







# person-level data
data_pp <- data |> group_by(UUID) |>
  summarise(age = mean(age, na.rm = TRUE),
            UUID = head(UUID, 1),
            gender = head(gender, 1),
            female = head(female, 1),
            erq = head(erq, 1),
            RESS_3_BL = head(RESS_3_BL, 1),
            RESS_11_BL = head(RESS_11_BL, 1),
            RESS_14_BL = head(RESS_14_BL, 1),
            RESS_20_BL = head(RESS_20_BL, 1),
            er_pm = mean(er, na.rm = TRUE),
            anger_pm = mean(anger, na.rm = TRUE),
            sad_pm = mean(sad, na.rm = TRUE),
            neg_aff_pm = mean(neg_aff, na.rm = TRUE),
            study = head(study, 1)
            )


# reliabilities

## Negative Affect
omega_NA <- multilevelTools::omegaSEM(c("anger", "stress", "sad", "anx", "guil", "tire", "lone"),
                                      id = "UUID",
                                      data = data)


## ERQ (RESS)
alpha <- psych::alpha(data |> select(RESS_3_BL, RESS_11_BL, RESS_14_BL, RESS_20_BL))
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
  filter(gender != "other")|>
  select(er, er_b,
         anger,
         sad,
         neg_aff, UUID,
         Date_Local, Time_Local)





data <- left_join(data, data_pp, by = "UUID") |>
  mutate(
    anger_pmc = anger - anger_pm,
    sad_pmc = sad - sad_pm,
    neg_aff_pmc = neg_aff - neg_aff_pm
  ) |> group_by(ppID, Date_Local) |>
  mutate(
    anger_pmc_lag = lag(anger_pmc),
    sad_pmc_lag = lag(sad_pmc),
    neg_aff_pmc_lag = lag(neg_aff_pmc),
    time = strptime(Time_Local, "%H:%M:%S"),
    time_diff = as.numeric(time - lag(time), units = "secs")
  )




saveRDS(data_pp, "Data/person-level-data/goals.RDS")
saveRDS(data, "Data/beep-level-data/goals.RDS")
saveRDS(list(alpha_erq = alpha_erq,
             which_erq = "RESS",
             omega_NA = omega_NA$Results,
             descriptives0 = descriptives0,
             descriptives = descriptives), "Data/descriptives/goals.RDS")
rm(data, data_pp, descriptives, alpha, alpha_erq, omega_NA)
