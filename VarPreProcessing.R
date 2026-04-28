
library(dplyr)

data_19to21 <- read.csv("data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")


### Num of unique vals per col
uniques <- sapply(data_19to21, function(x) length(unique(x)))

uniques_df <- data.frame(
  var_name = names(uniques),
  level_count = as.integer(uniques),
  row.names = NULL
  )

uniques_df <- uniques_df[order(-uniques_df$level_count), ]


###Proportions
uniqueness_ratio <- sapply(data_19to21, function(x) length(unique(x)) / length(x))

uniqueness_df <- data.frame(
  var_name = names(uniqueness_ratio),
  ratio = as.numeric(uniqueness_ratio),
  row.names = NULL
)

uniqueness_df <- uniqueness_df[order(-uniqueness_df$ratio), ]

###Merge and sort

diagnostics <- merge(uniques_df, uniqueness_df, by="var_name")

diagnostics$marker <- with(diagnostics, ifelse(
  level_count > 200 | ratio > 0.05, "exclude",
  ifelse(level_count > 100, "supplementary",
  ifelse(level_count > 50, "review", "ok"))
))


head(diagnostics[order(-diagnostics$level_count),], 20)

head(diagnostics[order(-diagnostics$ratio), ], 20)

hist(diagnostics$level_count, breaks=50, main="Dist of Variable Uniqueness", xlab="Number of Categories")
