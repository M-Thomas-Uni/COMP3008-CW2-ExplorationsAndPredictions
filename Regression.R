
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

#######################

library(nnet)
library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)

class(cleaned.1921.mca)
class(cleaned.2224.mca)

# Recode outcome + remove SC20MMJ
cleaned.1921.for_reg <- cleaned.1921.mca %>%
  mutate(
    INECAC05_recode = case_when(
      INECAC05 %in% 1:4 ~ "Employed",
      INECAC05 == 5 ~ "Unemployed",
      INECAC05 %in% 6:25 ~ "Inactive",
      TRUE ~ NA_character_
    ),
    INECAC05_recode = factor(INECAC05_recode, levels = c("Inactive", "Employed", "Unemployed"))
  ) %>%
  dplyr::select(-any_of(c("SC20MMJ", "SC20MMN"))) %>%
  filter(!is.na(INECAC05_recode))

cleaned.2224.for_reg <- cleaned.2224.mca %>%
  mutate(
    INECAC05_recode = case_when(
      INECAC05 %in% 1:4 ~ "Employed",
      INECAC05 == 5 ~ "Unemployed",
      INECAC05 %in% 6:25 ~ "Inactive",
      TRUE ~ NA_character_
    ),
    INECAC05_recode = factor(INECAC05_recode, levels = c("Inactive", "Employed", "Unemployed"))
  ) %>%
  dplyr::select(-any_of(c("SC20MMJ", "SC20MMN"))) %>%
  filter(!is.na(INECAC05_recode))

model_1921 <- multinom(
  INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + SEX + GOVTOF,
  data = cleaned.1921.for_reg
)

model_2224 <- multinom(
  INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + SEX + GOVTOF,
  data = cleaned.2224.for_reg
)


# Extract summary
s_1921 <- summary(model_1921)
s_2224 <- summary(model_2224)

# Compute z and p
z_1921 <- s_1921$coefficients / s_1921$standard.errors
p_1921 <- 2 * (1 - pnorm(abs(z_1921)))

z_2224 <- s_2224$coefficients / s_2224$standard.errors
p_2224 <- 2 * (1 - pnorm(abs(z_2224)))

tidy_1921 <- tidy(model_1921) %>%
  mutate(
    period = "2019–21",
    z = as.vector(z_1921),
    p = as.vector(p_1921),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

tidy_2224 <- tidy(model_2224) %>%
  mutate(
    period = "2022–24",
    z = as.vector(z_2224),
    p = as.vector(p_2224),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )


all_coefs <- bind_rows(tidy_1921, tidy_2224)

coef_movement <- all_coefs %>%
  dplyr::select(y.level, term, period, estimate) %>%
  pivot_wider(names_from = period, values_from = estimate) %>%
  rename(estimate_1921 = `2019–21`,
         estimate_2224 = `2022–24`)

coef_movement_plot <- ggplot(coef_movement,
                             aes(y = term, colour = y.level)) + geom_vline(xintercept=0, linetype=2) +
  
  geom_segment(aes(x = estimate_1921,
                   xend = estimate_2224,
                   yend = term),
               arrow = arrow(length = unit(0.3, "cm")),
               linewidth = 0.2) +
  
  geom_point(aes(x = estimate_1921), size = 1, shape = 16) +
  geom_point(aes(x = estimate_2224), size = 3, shape = 16) +
  
  geom_vline(xintercept = c(-2, -1, -0.5, 0.5, 1, 2),
             linetype = "dashed",
             colour = "grey60",
             linewidth = 0.4) +
  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  
  labs(
    title = "Coefficient Movement Between 2019–21 and 2022–24",
    x = "Coefficient (log-odds)",
    y = "Predictor",
    colour = "Outcome Category"
  )

coef_movement_plot

reg_coef_1921 <- ggplot(tidy_1921,
                        aes(x = estimate, y = term, colour = y.level)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Regression Coefficients with Significance (2019–21)",
    x = "Coefficient (log-odds)",
    y = "Predictor",
    colour = "Outcome Category (Baseline of Inactive)"
  ) + geom_text(aes(x = estimate, label = sig),
                hjust = -0.2, size = 5) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_segment(aes(x = 0, xend = estimate,
                   y = term, yend = term),
               linewidth = 0.4, alpha = 0.6)

reg_coef_2224 <- ggplot(tidy_2224,
                        aes(x = estimate, y = term, colour = y.level)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Regression Coefficients with Significance (2022–24)",
    x = "Coefficient (log-odds)",
    y = "Predictor",
    colour = "Outcome Category (Baseline of Inactive)"
  ) + geom_text(aes(x = estimate, label = sig),
                hjust = -0.2, size = 5) + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_segment(aes(x = 0, xend = estimate,
                   y = term, yend = term),
               linewidth = 0.4, alpha = 0.6)


reg_coef_1921 / reg_coef_2224



tidy_1921 <- tidy_1921 %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

tidy_2224 <- tidy_2224 %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )

ggplot(tidy_1921, aes(x = estimate, y = term, colour = y.level)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  facet_wrap(~ y.level, scales = "free_x")

