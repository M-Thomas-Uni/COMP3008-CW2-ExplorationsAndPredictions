
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(dplyr)

data_19to21 <- read.csv("data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")
data_22to24 <- read.csv("data/AnnualPopulationSurvey_Jan2022_Dec2024.csv")

quant_vars <- c("AGE", "CAMEYR", "CAMEYR2", "CONSEY", "DVHRPNUM", "EMPMON", "FAMUNIT", "fileyear", "FDSNGDEG", "GRSSWK", "GRSSWK2", "HOURPAY", "ILLST17", "LEFTYR", "NETWK2", "npwt22c", "PWTA22C", "REFWKD", "REFWKM", "REFWKY", "RESMTH", "THISQTR", "THISWV", "REFDTE", "SNGDEGN", "SNGHD")

supp_vars <- c("REFDTE", "REFWKD", "REFWKM", "REFWKY",
               "ETH11EW", "ETH11S", "ETHAFS", "ETHAS11", "ETHAS11S", "ETHBL11",
               "ETHCBS", "ETHEWEUL", "ETHGBEUL", "ETHMX11", "ETHUKEUL", "ETHWHE",
               "ETHWHW", "ETHWSC")

which(data_19to21)

supp_idx_A <- match(supp_vars, names(data_19to21))

supp_idx_B <- match(supp_vars, names(data_22to24))

quant_idx_A <- match(quant_vars, names(data_19to21))

quant_idx_B <- match(quant_vars, names(data_22to24))

to_be_factors_idx_A <- setdiff(seq_along(data_19to21), quant_idx_A)
to_be_factors_idx_B <- setdiff(seq_along(data_22to24), quant_idx_B)

data_19to21[to_be_factors_idx_A] <- lapply(data_19to21[to_be_factors_idx_A], as.factor)
data_22to24[to_be_factors_idx_B] <- lapply(data_22to24[to_be_factors_idx_B], as.factor)

data_19to21[to_be_factors_idx_A] <- lapply(data_19to21[quant_idx_A], as.numeric)
data_22to24[to_be_factors_idx_B] <- lapply(data_22to24[quant_idx_B], as.numeric)

library(Factoshiny)
res <- Factoshiny(data_19to21)

set.seed(42)
sample_size = 30000

sub_19to21 <- data_19to21[sample(nrow(data_19to21), sample_size), ]
sub_22to24 <- data_22to24[sample(nrow(data_22to24), sample_size), ]

sub_19to24.shiny <- Factoshiny(sub_19to21)
sub_22to24.shiny <- Factoshiny(sub_22to24)


sub_22to24.mca_result <- MCA(sub_19to21)

fviz_eig(sub_22to24.mca_result, addlabels=TRUE)

fviz_mca_ind(sub_22to24.mca_result,
             col.ind = "cos2",
             axes= c(1, 2),
             repel = TRUE)

fviz_contrib(sub_22to24.mca_result,
             choice = "var",
             axes = 1,
             top = 10)

fviz_mca_var(sub_22to24.mca_result,
             col.var = "contrib",
             axes = c(1, 2),
             repel = TRUE)



mca_res19_22 <- MCA(sub_19to21,
               ncp=20,
               graph = FALSE)

mca_res22_24 <- MCA(sub_22to24,
                    ncp=20,
                    quali.sup = supp_idx_B,
                    graph = FALSE)

