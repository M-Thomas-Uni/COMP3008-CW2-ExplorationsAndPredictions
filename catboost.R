
library(gbm)

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

# Convert outcome to factor
train$INECAC05_recode <- factor(train$INECAC05_recode)
test$INECAC05_recode  <- factor(test$INECAC05_recode)

# Class weights to boost Unemployed
weights <- ifelse(train$INECAC05_recode == "Unemployed", 10, 1)

gbm_model <- gbm(
  formula = INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + age,
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
  pred.var = "INECAC05_recode",
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


#############


rf_data <- cleaned.2224.for_reg %>%
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

# Convert outcome to factor
train$INECAC05_recode <- factor(train$INECAC05_recode)
test$INECAC05_recode  <- factor(test$INECAC05_recode)

# Class weights to boost Unemployed
weights <- ifelse(train$INECAC05_recode == "Unemployed", 10, 1)

gbm_model.2224 <- gbm(
  formula = INECAC05_recode ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + age,
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
gbm_pred <- predict(gbm_model.2224, newdata = test, n.trees = 1500, type = "response")
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

gbm_conf.2224 <- table(
  Predicted = factor(gbm_class, levels = desired_order),
  Actual = factor(test$INECAC05_recode, levels = desired_order)
)
gbm_conf.2224

library(reshape2)
cm_rate <- prop.table(gbm_conf.2224, margin = 1)
cm_rate <- prop.table(gbm_conf.2224)
cm_df <- as.data.frame(cm_rate)

library(ggplot2)

conf_heatmap.gboost.2224 <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  theme_minimal() +
  labs(
    title = "Normalised Confusion Matrix (Gradient Boost)",
    x = "Actual Class",
    y = "Predicted Class"
  )


library(dplyr)
library(ggplot2)
library(parallel)
library(rlang)


train$Y_employed   <- ifelse(train$INECAC05_recode == "Employed", 1, 0)
train$Y_unemployed <- ifelse(train$INECAC05_recode == "Unemployed", 1, 0)
train$Y_inactive   <- ifelse(train$INECAC05_recode == "Inactive", 1, 0)

library(gbm)

predictors_formula <- as.formula(
  " ~ AAGE + HIQUL_D + LNGLST + LIMACT + MARSTA + SEX + age"
)

gbm_emp <- gbm(
  formula = update(predictors_formula, Y_employed ~ .),
  data = train,
  distribution = "bernoulli",
  n.trees = 1500,
  interaction.depth = 4,
  shrinkage = 0.03,
  bag.fraction = 0.7
)

gbm_unemp <- gbm(
  formula = update(predictors_formula, Y_unemployed ~ .),
  data = train,
  distribution = "bernoulli",
  n.trees = 1500,
  interaction.depth = 4,
  shrinkage = 0.03,
  bag.fraction = 0.7
)

gbm_inact <- gbm(
  formula = update(predictors_formula, Y_inactive ~ .),
  data = train,
  distribution = "bernoulli",
  n.trees = 1500,
  interaction.depth = 4,
  shrinkage = 0.03,
  bag.fraction = 0.7
)

library(dplyr)
library(ggplot2)
library(parallel)
library(rlang)

pdp_binary_fast <- function(model, data, varname, title,
                            subsample = 3000,
                            ncores = detectCores() - 1) {
  
  # Subsample
  set.seed(42)
  if (nrow(data) > subsample) {
    data_sub <- data %>% sample_n(subsample)
  } else {
    data_sub <- data
  }
  
  values <- unique(data_sub[[varname]])
  var_sym <- sym(varname)
  
  # Parallel cluster
  cl <- makeCluster(ncores)
  clusterExport(cl, varlist = c("model", "data_sub", "varname", "values"),
                envir = environment())
  clusterEvalQ(cl, library(gbm))
  
  # PDP loop
  pdp_list <- parLapply(cl, values, function(v) {
    newdata <- data_sub
    newdata[[varname]] <- v
    
    preds <- predict(model, newdata = newdata, type = "response")
    avg <- mean(preds)
    
    data.frame(
      value = v,
      y = avg
    )
  })
  
  stopCluster(cl)
  
  pdp_all <- bind_rows(pdp_list)
  
  if (is.factor(data[[varname]])) {
    pdp_all$value <- factor(pdp_all$value, levels = levels(data[[varname]]))
  }
  
  ggplot(pdp_all, aes(x = value, y = y)) +
    geom_point(size = 3, colour = "#1f78b4") +
    geom_line(group = 1, colour = "#1f78b4") +
    labs(
      title = paste("PDP for", varname, ", predicting", title),
      x = varname,
      y = "Predicted Probability"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

pdp_emp_aage <- pdp_binary_fast(gbm_emp, train, "AAGE", "Employment")
pdp_emp_hiqul  <- pdp_binary_fast(gbm_emp, train, "HIQUL_D", "Employment")
pdp_emp_lnglst <- pdp_binary_fast(gbm_emp, train, "LNGLST", "Employment")
pdp_emp_limact <- pdp_binary_fast(gbm_emp, train, "LIMACT", "Employment")
pdp_emp_marsta <- pdp_binary_fast(gbm_emp, train, "MARSTA", "Employment")
pdp_emp_sex    <- pdp_binary_fast(gbm_emp, train, "SEX", "Employment")
pdp_emp_govtofage <- pdp_binary_fast(gbm_emp, train, "GOVTOF", "Employment")

pdp_unemp_aage   <- pdp_binary_fast(gbm_unemp, train, "AAGE", "Unemployment")
pdp_unemp_hiqul  <- pdp_binary_fast(gbm_unemp, train, "HIQUL_D", "Unemployment")
pdp_unemp_lnglst <- pdp_binary_fast(gbm_unemp, train, "LNGLST", "Unemployment")
pdp_unemp_limact <- pdp_binary_fast(gbm_unemp, train, "LIMACT", "Unemployment")
pdp_unemp_marsta <- pdp_binary_fast(gbm_unemp, train, "MARSTA", "Unemployment")
pdp_unemp_sex    <- pdp_binary_fast(gbm_unemp, train, "SEX", "Unemployment")
pdp_unemp_govtof <- pdp_binary_fast(gbm_unemp, train, "GOVTOF", "Unemployment")

pdp_inact_aage   <- pdp_binary_fast(gbm_inact, train, "AAGE", "Inactive")
pdp_inact_hiqul  <- pdp_binary_fast(gbm_inact, train, "HIQUL_D", "Inactive")
pdp_inact_lnglst <- pdp_binary_fast(gbm_inact, train, "LNGLST", "Inactive")
pdp_inact_limact <- pdp_binary_fast(gbm_inact, train, "LIMACT", "Inactive")
pdp_inact_marsta <- pdp_binary_fast(gbm_inact, train, "MARSTA", "Inactive")
pdp_inact_sex    <- pdp_binary_fast(gbm_inact, train, "SEX", "Inactive")
pdp_inact_govtof <- pdp_binary_fast(gbm_inact, train, "GOVTOF", "Inactive")


(pdp_emp_aage / pdp_unemp_aage / pdp_inact_aage)
(pdp_emp_limact / pdp_unemp_limact / pdp_inact_limact)
(pdp_emp_hiqul / pdp_unemp_hiqul / pdp_inact_hiqul)

