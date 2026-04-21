
library(dplyr)

data_19to21 <- read.csv("./data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")

reduced_data <- data_19to21 %>% select(AAGE, CIGEVER)

reduced_as_table <- table(reduced_data$AAGE, reduced_data$CIGEVER)

reduced_as_table <- reduced_as_table[-(1:2),]

chisq.test(reduced_as_table)

head(reduced_as_table, n=15)
