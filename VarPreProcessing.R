
library(dplyr)
library(ggplot2)

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


head(diagnostics[order(-diagnostics$level_count),], 30)

head(diagnostics[order(-diagnostics$ratio), ], 30)

ggplot(diagnostics, aes(x=marker)) + geom_bar(fill="steelblue") + 
  geom_text(
    stat = "count",
    aes(label=..count..),
    vjust = -0.5,
    size=4
  ) +
  labs(
  title="Frequency of Uniqueness Suggestions",
  x="Marker",
  y="Count"
) + theme_minimal(base_size=14)

hist(diagnostics$level_count, breaks=50, main="Dist of Variable Uniqueness", xlab="Number of Categories")

excludes <- diagnostics$var_name[diagnostics$marker=="exclude"]
to_keep <- c("GROSS99", "HOURPAY", "HRRATE", "NET99", "USNETPAY", "USUGPAY", "GRSSWK", "NETWK", "SECNET", "SECGRO", "TOTAC2", "TOTUS2", "TOTAC1", "GRSSWK2", "ACTHR", "USUHR", "NETWK2", "soc20S", "HRRATE2")

excludes <- setdiff(excludes, to_keep)

review <- diagnostics$var_name[diagnostics$marker=="review"]
review_okayed <- c("")

excludes <- c(excludes, setdiff(review, review_okayed))