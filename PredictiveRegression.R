
library(nnet)
library(dplyr)
library(caret)

set.seed(42)

predictors <- c("AAGE", "HIQUL_D", "LNGLST", "LIMACT", 
                "MARSTA", "SEX", "GOVTOF")

model_data <- cleaned.1921.for_reg %>%
  select(all_of(predictors), INECAC05_recode) %>%
  filter(!is.na(INECAC05_recode))

train_index <- createDataPartition(
  model_data$INECAC05_recode,
  p = 0.8,
  list = FALSE
)

train <- model_data[train_index, ]
test  <- model_data[-train_index, ]

model_pred <- multinom(
  INECAC05_recode ~ .,
  data = train,
  trace = FALSE
)

pred <- predict(model_pred, newdata = test)

conf_matrix <- table(Predicted = pred, Actual = test$INECAC05_recode)
accuracy <- mean(pred == test$INECAC05_recode)

print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 3)))

coefs <- summary(model_pred)$coefficients
ses   <- summary(model_pred)$standard.errors

zvals <- coefs / ses
pvals <- 2 * (1 - pnorm(abs(zvals)))

stars <- ifelse(pvals < 0.001, "***",
                ifelse(pvals < 0.01, "**",
                       ifelse(pvals < 0.05, "*", "")))

significance_table <- data.frame(
  Estimate = coefs,
  SE = ses,
  z = zvals,
  p = pvals,
  Sig = stars
)

print(select(significance_table, starts_with("Sig.")))


library(ggplot2)
library(reshape2)

library(reshape2)
cm_rate <- prop.table(conf_matrix, margin = 1)
cm_rate <- prop.table(cm)
cm_df <- as.data.frame(cm_rate)

library(ggplot2)

conf_heatmap.regression <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal() +
  labs(
    title = "Normalised Confusion Matrix (Regression)",
    x = "Actual Class",
    y = "Predicted Class"
  )


