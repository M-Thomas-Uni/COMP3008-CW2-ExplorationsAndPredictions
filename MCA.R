
library(FactoMineR)
library(factoextra)

data_19to21 <- read.csv("data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")
data_22to24 <- read.csv("data/AnnualPopulationSurvey_Jan2022_Dec2024.csv")

quant_vars <- c("AGE", "CAMEYR", "CAMEYR2", "CONSEY", "DVHRPNUM", "EMPMON", "FAMUNIT", "fileyear", "FDSNGDEG", "GRSSWK", "GRSSWK2", "HOURPAY", "ILLST17", "LEFTYR", "NETWK2", "npwt22c", "PWTA22C", "REFWKD", "REFWKM", "REFWKY", "RESMTH", "THISQTR", "THISWV", "REFDTE", "SNGDEGN", "SNGHD")

supp_vars <- c("REFDTE", "REFWKD", "REFWKM", "REFWKY",
               "ETH11EW", "ETH11S", "ETHAFS", "ETHAS11", "ETHAS11S", "ETHBL11",
               "ETHCBS", "ETHEWEUL", "ETHGBEUL", "ETHMX11", "ETHUKEUL", "ETHWHE",
               "ETHWHW", "ETHWSC")



supp_idx_A <- match(supp_vars, names(data_19to21))

supp_idx_B <- match(supp_vars, names(data_22to24))

quant_idx_A <- match(quant_vars, names(data_19to21))

quant_idx_B <- match(quant_vars, names(data_22to24))

to_be_factors_idx_A <- setdiff(seq_along(data_19to21), quant_idx_A)
to_be_factors_idx_B <- setdiff(seq_along(data_22to24), quant_idx_B)

data_19to21[to_be_factors_idx_A] <- lapply(data_19to21[to_be_factors_idx_A], factor)
data_22to24[to_be_factors_idx_B] <- lapply(data_22to24[to_be_factors_idx_B], factor)

data_19to21 <- data_19to21[, to_be_factors_idx_A]
data_22to24 <- data_22to24[, to_be_factors_idx_A]

set.seed(42)
sample_size = 30000

sub_19to21 <- data_19to21[sample(nrow(data_19to21), sample_size), ]
sub_22to24 <- data_22to24[sample(nrow(data_22to24), sample_size), ]




mca_res19_22 <- MCA(sub_19to21,
               ncp=20,
               graph = FALSE)

mca_res22_24 <- MCA(sub_22to24,
                    ncp=20,
                    quali.sup = supp_idx_B,
                    graph = FALSE)

