library(tidyverse)
library(dplyr)
library(lubridate)

data_19to21 <- read.csv("./data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")

reduced_data <- data_19to21 %>% select(AAGE, CIGEVER, REFDTE)

reduced_data <- reduced_data %>% mutate(ACT_DTE = parse_date_time(REFDTE, orders=c("Ymd","dmy","mdy","Y-m-d","d/m/Y")))
summary(reduced_data$ACT_DTE)
n_distinct(is.na(reduced_data$ACT_DTE))

daily <- reduced_data %>%
  count(ACT_DTE)
ggplot(daily, aes(ACT_DTE, n)) + geom_line() + labs(title="Responses per day")
                                        
reduced_as_table <- table(reduced_data$AAGE, reduced_data$CIGEVER)

reduced_as_table <- reduced_as_table[-(1:2),]

#Chi-Square test
chisq.test(reduced_as_table)

head(reduced_as_table, n=15)

data_age_as_factor <- data_19to21 %>% mutate(AAGE = as.character(AAGE)) %>% mutate(AAGE = fct_recode(AAGE,
                                                        "Under 16" = "1",
                                                        "Under 20" = "2",
                                                        "Under 20" = "3",
                                                        "20 to 50" = "4",
                                                        "20 to 50" = "5",
                                                        "20 to 50" = "6",
                                                        "20 to 50" = "7",
                                                        "20 to 50" = "8",
                                                        "20 to 50" = "9",
                                                        "50 and Above" = "10",
                                                        "50 and Above" = "11",
                                                        "50 and Above" = "12",
                                                        "50 and Above" = "13"
                                                        ))

table(data_age_as_factor$AAGE)
plot(table(data_age_as_factor$AAGE))

