
library(gbm)

# Convert outcome to factor
train$INECAC05_recode <- factor(train$INECAC05_recode)
test$INECAC05_recode  <- factor(test$INECAC05_recode)

# Class weights to boost Unemployed
weights <- ifelse(train$INECAC05_recode == "Unemployed", 10, 1)

gbm_model <- gbm(
  formula = INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + GOVTOF,
  data = train,
  distribution = "multinomial",
  n.trees = 1500,
  interaction.depth = 4,
  shrinkage = 0.03,
  bag.fraction = 0.7,
  weights = weights,
  train.fraction = 1.0
)

# Predict
gbm_pred <- predict(gbm_model, newdata = test, n.trees = 1500, type = "response")
gbm_class <- colnames(gbm_pred)[apply(gbm_pred, 1, which.max)]

# Evaluate
table(Predicted = gbm_class, Actual = test$INECAC05_recode)
mean(gbm_class == test$INECAC05_recode)

library(pdp)

partial(
  gbm_model,
  pred.var = "AAGE",
  n.trees = 1500,
  plot = TRUE
)



desired_order <- c("Employed", "Unemployed", "Inactive")

gbm_conf <- table(
  Predicted = factor(gbm_class, levels = desired_order),
  Actual = factor(test$INECAC05_recode, levels = desired_order)
)
gbm_conf

library(reshape2)
cm_rate <- prop.table(gbm_conf, margin = 1)
cm_rate <- prop.table(gbm_conf)
cm_df <- as.data.frame(cm_rate)

library(ggplot2)

conf_heatmap.gboost <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal() +
  labs(
    title = "Normalised Confusion Matrix (Gradient Boost)",
    x = "Actual Class",
    y = "Predicted Class"
  )

