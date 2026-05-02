
library(nnet)
library(broom)
library(dplyr)
library(ggplot2)

cleaned.1921.for_reg <- cleaned.1921.mca %>%
  mutate(
    INECAC05_recode = case_when(
      INECAC05 %in% 1:4 ~ "Employed",
      INECAC05 == 5 ~ "Unemployed",
      INECAC05 %in% 6:25 ~ "Inactive",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(INECAC05_recode))

cleaned.2224.for_reg <- cleaned.2224.mca %>%
  mutate(
    INECAC05_recode = case_when(
      INECAC05 %in% 1:4 ~ "Employed",
      INECAC05 == 5 ~ "Unemployed",
      INECAC05 %in% 6:25 ~ "Inactive",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(INECAC05_recode))

model_1921 <- multinom(
  INECAC05_recode ~ AAGE + HIQUL_D + SC20MMJ + LNGLST + SEX + GOVTOF,
  data = cleaned.1921.for_reg
)

model_2224 <- multinom(
  INECAC05_recode ~ AAGE + HIQUL_D + SC20MMJ + LNGLST + SEX + GOVTOF,
  data = cleaned.2224.for_reg
)

model_1921
model_2224

tidy_1921 <- tidy(model_1921) %>%
  mutate(period = "2019–21")

tidy_2224 <- tidy(model_2224) %>%
  mutate(period = "2022–24")

all_coefs <- bind_rows(tidy_1921, tidy_2224)

# Pivot wider to get start/end values for arrows
coef_movement <- all_coefs %>%
  select(y.level, term, period, estimate) %>%
  pivot_wider(names_from = period, values_from = estimate) %>%
  rename(estimate_1921 = `2019–21`,
         estimate_2224 = `2022–24`)

ggplot(coef_movement,
       aes(y = term, colour = y.level)) +
  
  # Arrow showing movement
  geom_segment(aes(x = estimate_1921,
                   xend = estimate_2224,
                   yend = term),
               arrow = arrow(length = unit(0.3, "cm")),
               linewidth = 0.7) +
  
  # Start point (2019–21)
  geom_point(aes(x = estimate_1921), size = 3, shape = 16) +
  
  # End point (2022–24)
  geom_point(aes(x = estimate_2224), size = 3, shape = 17) +
  
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  
  labs(
    title = "Coefficient Movement Between 2019–21 and 2022–24",
    x = "Coefficient (log-odds)",
    y = "Predictor",
    colour = "Outcome Category"
  )


