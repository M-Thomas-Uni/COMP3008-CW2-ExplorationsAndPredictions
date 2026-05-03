library(randomForest)
library(caret)
library(dplyr)

library(rpart.plot)
library(grid)
library(gridGraphics)
predictors <- c("AAGE", "HIQUL_D", "LNGLST", "LIMACT", 
                "MARSTA", "SEX", "GOVTOF")

rf_data <- cleaned.1921.for_reg %>%
  dplyr::select(all_of(predictors), INECAC05_recode) %>%
  filter(!is.na(INECAC05_recode))

set.seed(42)

train_index <- createDataPartition(
  rf_data$INECAC05_recode,
  p = 0.8,
  list = FALSE
)

train <- rf_data[train_index, ]
test  <- rf_data[-train_index, ]

train$INECAC05_recode <- factor(train$INECAC05_recode,
                                levels = c("Employed", "Unemployed", "Inactive"))

test$INECAC05_recode <- factor(test$INECAC05_recode,
                               levels = c("Employed", "Unemployed", "Inactive"))

rf_model <- randomForest(
  INECAC05_recode ~ .,
  data = train,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

rf_pred <- predict(rf_model, newdata = test)

rf_conf_matrix <- table(Predicted = rf_pred, Actual = test$INECAC05_recode)
rf_accuracy <- mean(rf_pred == test$INECAC05_recode)

print(rf_conf_matrix)
print(paste("RF Accuracy:", round(rf_accuracy, 3)))

varImpPlot(rf_model)




library(ggplot2)
library(reshape2)


cm <- table(Predicted = rf_pred, Actual = test$INECAC05_recode)

cm <- table(
  Predicted = factor(rf_pred, levels = desired_order),
  Actual    = factor(test$INECAC05_recode,    levels = desired_order)
)

library(reshape2)
cm_rate <- prop.table(cm, margin = 1)
cm_rate <- prop.table(cm)
cm_df <- as.data.frame(cm_rate)

library(ggplot2)

conf_heatmap.forest <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal() +
  labs(
    title = "Normalised Confusion Matrix (Random Forest)",
    x = "Actual Class",
    y = "Predicted Class"
  )



library(pdp)

partial(rf_model, pred.var = "AAGE", plot = TRUE)
partial(rf_model, pred.var = "HIQUL_D", plot = TRUE)
partial(rf_model, pred.var = "LNGLST", plot = TRUE)


library(rpart)

cart_model <- rpart(
  INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + GOVTOF,
  data = train,
  method = "class",
  control = rpart.control(
    cp = 0.0005,      # moderate pruning → keeps tree small
    maxdepth = 6,   # ensures 3–4 layers
    minsplit = 50  # prevents tiny branches
  )
)

library(rpart.plot)

rpart.plot(
  cart_model,
  type = 3,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1.2,
  faclen = 0,          # don’t truncate factor labels
  varlen = 0,          # don’t truncate variable names
  clip.right.labs = FALSE,
  branch = 1           # curved branches (more readable)
)

# Capture it as a grob
tree_grob <- grid.grab()

# Convert to a ggplot-like object
library(ggplotify)
treeplot.1921 <- as.ggplot(tree_grob)

##################

rf_data.2224 <- cleaned.2224.for_reg %>%
  dplyr::select(all_of(predictors), INECAC05_recode) %>%
  filter(!is.na(INECAC05_recode))

set.seed(42)

train_index <- createDataPartition(
  rf_data.2224$INECAC05_recode,
  p = 0.8,
  list = FALSE
)

train <- rf_data.2224[train_index, ]
test  <- rf_data.2224[-train_index, ]

train$INECAC05_recode <- factor(train$INECAC05_recode,
                                levels = c("Employed", "Unemployed", "Inactive"))

test$INECAC05_recode <- factor(test$INECAC05_recode,
                               levels = c("Employed", "Unemployed", "Inactive"))

rf_model.2224 <- randomForest(
  INECAC05_recode ~ .,
  data = train,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

rf_pred.2224 <- predict(rf_model.2224, newdata = test)

rf_conf_matrix.2224 <- table(Predicted = rf_pred.2224, Actual = test$INECAC05_recode)
rf_accuracy <- mean(rf_pred.2224 == test$INECAC05_recode)

print(rf_conf_matrix.2224)
print(paste("RF Accuracy:", round(rf_accuracy, 3)))

varImpPlot(rf_model.2224)




library(ggplot2)
library(reshape2)


cm.2224 <- table(Predicted = rf_pred.2224, Actual = test$INECAC05_recode)

cm.2224 <- table(
  Predicted = factor(rf_pred.2224, levels = desired_order),
  Actual    = factor(test$INECAC05_recode,    levels = desired_order)
)

library(reshape2)
cm_rate <- prop.table(cm.2224, margin = 1)
cm_rate <- prop.table(cm.2224)
cm_df <- as.data.frame(cm_rate)

library(ggplot2)

conf_heatmap.forest.2224 <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal() +
  labs(
    title = "Normalised Confusion Matrix (Random Forest)",
    x = "Actual Class",
    y = "Predicted Class"
  )



library(pdp)

partial(rf_model.2224, pred.var = "AAGE", plot = TRUE)
partial(rf_model.2224, pred.var = "HIQUL_D", plot = TRUE)
partial(rf_model.2224, pred.var = "LNGLST", plot = TRUE)


library(rpart)

cart_model.2224 <- rpart(
  INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + GOVTOF,
  data = train,
  method = "class",
  control = rpart.control(
    cp = 0.0005,      # moderate pruning → keeps tree small
    maxdepth = 6,   # ensures 3–4 layers
    minsplit = 50  # prevents tiny branches
  )
)

library(rpart.plot)

rpart.plot(
  cart_model.2224,
  type = 3,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  tweak = 1.2,
  faclen = 0,          # don’t truncate factor labels
  varlen = 0,          # don’t truncate variable names
  clip.right.labs = FALSE,
  branch = 1           # curved branches (more readable)
)

# Capture it as a grob
tree_grob <- grid.grab()

# Convert to a ggplot-like object
library(ggplotify)
treeplot.2224 <- as.ggplot(tree_gro)
