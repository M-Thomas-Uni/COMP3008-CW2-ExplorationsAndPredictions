library(srvyr)
library(dplyr)
library(ggplot2)
library(patchwork)

survey_1921 <- cleaned.1921.for_reg %>%
  as_survey_design(weights = PWTA22C)

inactivity_region_1921 <- survey_1921 %>%
  group_by(GOVTOF) %>%
  summarise(inactive_rate = survey_mean(INECAC05_recode == "Inactive"))

ggplot(inactivity_region_1921,
       aes(x = reorder(GOVTOF, inactive_rate), y = inactive_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Weighted Inactivity Rate by Region (2019–21)",
    x = "Region",
    y = "Inactivity Rate"
  )

lts_region_1921 <- survey_1921 %>%
  group_by(GOVTOF) %>%
  summarise(lts_rate = survey_mean(LNGLST == 1))

ggplot(lts_region_1921,
       aes(x = reorder(GOVTOF, lts_rate), y = lts_rate)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Weighted Long-Term Sickness by Region (2019–21)",
    x = "Region",
    y = "Long-Term Sickness Rate"
  )

occ_region_1921 <- survey_1921 %>%
  group_by(GOVTOF, SC20MMJ) %>%
  summarise(prop = survey_mean())

ggplot(occ_region_1921,
       aes(x = GOVTOF, y = prop, fill = SC20MMJ)) +
  geom_col(position = "fill") +
  theme_minimal() +
  labs(
    title = "Weighted Occupational Structure by Region (2019–21)",
    x = "Region",
    y = "Proportion",
    fill = "Occupation"
  ) +
  coord_flip()


inactivity_both <- bind_rows(
  cleaned.1921.for_reg %>% mutate(period = "2019–21"),
  cleaned.2224.for_reg %>% mutate(period = "2022–24")
) %>%
  as_survey_design(weights = PWTA22C) %>%
  group_by(period, GOVTOF) %>%
  summarise(inactive_rate = survey_mean(INECAC05_recode == "Inactive"))

inactivity_weights <- ggplot(inactivity_both,
                             aes(x = GOVTOF, y = inactive_rate, fill = period)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Weighted Inactivity by Region (2019–21 vs 2022–24)",
    x = "Region",
    y = "Inactivity Rate"
  ) 


unemployment_both <- bind_rows(
  cleaned.1921.for_reg %>% mutate(period = "2019–21"),
  cleaned.2224.for_reg %>% mutate(period = "2022–24")
) %>%
  as_survey_design(weights = PWTA22C) %>%
  group_by(period, GOVTOF) %>%
  summarise(unemployment_rate = survey_mean(INECAC05_recode == "Unemployed"))

# Plot
unemployment_weights <- ggplot(unemployment_both,
                               aes(x = GOVTOF, y = unemployment_rate, fill = period)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Weighted Unemployment by Region (2019–21 vs 2022–24)",
    x = "Region",
    y = "Unemployment Rate",
    fill = "Period"
  )

inactivity_weights + unemployment_weights
