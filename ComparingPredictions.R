library(pdp)

partial(
  gbm_model,
  pred.var = "AAGE",
  n.trees = 1500,
  plot = TRUE
)


conf_heatmap.regression + conf_heatmap.forest + conf_heatmap.gboost


plot_confusion_heatmap <- function(conf_mat, title, normalise = c("none", "recall", "precision", "global")) {
  
  normalise <- match.arg(normalise)
  
  # Reorder axes
  conf_mat <- conf_mat[
    desired_order,
    desired_order,
    drop = FALSE
  ]
  
  # Normalisation
  if (normalise == "recall") {
    conf_mat <- prop.table(conf_mat, margin = 2)   # column-normalised
  }
  if (normalise == "precision") {
    conf_mat <- prop.table(conf_mat, margin = 1)   # row-normalised
  }
  if (normalise == "global") {
    conf_mat <- prop.table(conf_mat)               # whole matrix
  }
  
  # Convert to data frame
  cm_df <- as.data.frame(as.table(conf_mat))
  names(cm_df) <- c("Predicted", "Actual", "Freq")
  
  # Plot
  return(ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 5) +
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    theme_minimal() +
    labs(
      title = title,
      x = "Actual Class",
      y = "Predicted Class"
    ))
}



plot_confusion_heatmap(gbm_conf, "GBM Confusion Matrix", normalise = "recall") + 
plot_confusion_heatmap(cm, "Random Forest Confusion Matrix", normalise = "recall") + 
plot_confusion_heatmap(conf_matrix, "Multinomial Regression Confusion Matrix", normalise = "recall")


