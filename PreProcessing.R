
library(dplyr)
library(tidyverse)
library(lubridate)
library(FactoMineR)


data_19to21 <- read.csv("data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")
data_22to24 <- read.csv("data/AnnualPopulationSurvey_Jan2022_Dec2024.csv")

###clean name
pre_clean.1921 <- data_19to21 %>% rename(
  NPWT22C = npwt22c
  ) %>% select(-DISCURR13, -starts_with("HEALPB"), -starts_with("QUAL_"), -starts_with("SC2010"))

pre_clean.2224 <- data_22to24 %>% rename(
  GOR9d = GOR9dcensus2021,
  CombinedAuthorities = CombinedAuthoritiescensus2021
) %>% select( -starts_with("FLEX"), -FLED22)


## Lining up encodings of SOC 2010->SOC 2020
soc_mappings <- read.csv("mappings_cleaned.csv")
for (i in seq_len(nrow(soc_mappings))) {
  if (soc_mappings$SOC.2010[i] == "" || is.na(soc_mappings$SOC.2010[i])) {
    soc_mappings$SOC.2010[i] <- soc_mappings$SOC.2010[i - 1]
  }
}

pct_to_num <- function(x) { as.numeric(sub("%", "", x)) / 100}

soc_mappings$men_avg <- pct_to_num(soc_mappings$men_avg_percent)
soc_mappings$women_avg <- pct_to_num(soc_mappings$women_avg_percent)
soc_mappings$avg_avg <- (soc_mappings$men_avg + soc_mappings$women_avg) / 2

soc_list <- split(soc_mappings, soc_mappings$SOC.2010)

harmonise_soc_var <- function(df, soc_var, sex_var, map) {
  
  df %>%
    mutate(
      soc2010_val = as.character(.data[[soc_var]]),
      sex_val = .data[[sex_var]]
    ) %>%
    left_join(map, by = c("soc2010_val" = "soc2010")) %>%
    mutate(
      weight = case_when(
        sex_val == 1 ~ men_avg,
        sex_val == 2 ~ women_avg,
        TRUE ~ avg_both
      )
    ) %>%
    group_by(row_id = row_number()) %>%
    mutate(weight_norm = weight / sum(weight, na.rm = TRUE)) %>%
    slice_max(weight_norm, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(soc2020) %>%
    pull()
}

harmonise_soc_vector <- function(soc2010_vec, sex_vec, soc_map_list) {
  
  out <- character(length(soc2010_vec))
  
  for (i in seq_along(soc2010_vec)) {
    SOC10 <- soc2010_vec[i]
    SEX   <- sex_vec[i]
    
    # If SOC2010 code missing, return NA
    if (is.na(SOC10) || SOC10 == "") {
      out[i] <- NA
      next
    }
    
    # Retrieve all possible mappings
    rows <- soc_map_list[[as.character(SOC10)]]
    
    if (is.null(rows)) {
      out[i] <- NA
      next
    }
    
    # Choose correct weight column
    if (SEX == 1) {
      w <- rows$men_avg
    } else if (SEX == 2) {
      w <- rows$women_avg
    } else {
      w <- rows$avg_avg
    }
    
    # Normalise weights
    w_norm <- w / sum(w, na.rm = TRUE)
    
    # Select SOC2020 with highest weight
    best_idx <- which.max(w_norm)
    out[i] <- rows$SOC.2020[best_idx]
  }
  
  return(out)
}


soc_vars <- c("SC10LMN", "SC10MMN", "SC10SMN")

run_harmonise <- function (df, occ_vars) {
  for (v in occ_vars) {
    new_v <- sub("SC10", "SC20", v)
    df[[new_v]] <- harmonise_soc_vector(
      soc2010_vec = df[[v]],
      sex_vec = df$SEX,
      soc_map_list = soc_list
    )
  }
  return(df)
}

pre_clean.1921 <- run_harmonise(pre_clean.1921, soc_vars)


clean_missing <- function(x, varname) {
  if (is.factor(x)) x <- as.character(x)
  
  x <- trimws(x)
  
  x[x %in% c("", "-8", "-9", "na", "NA", "null", "NULL", "99995", "99996", "99997", "99998", "99999", "41000")] <- NA
  
  if (varname %in% c("LNGLST", "APPRCURR", "ERNFILT", "HEALYL")) {
    x[x %in% c("3", "4")] <- NA
  }
  
  if (varname == "HIQUAL15") {
    x[x %in% c("85")] <- NA
  }
  if (varname %in% c("HIQUAL22", "HITQUA15")) {
    x[x %in% c("74")] <- NA
  }
  if (varname %in% c("HIQUL15D", "HIQUL22D", "ILLDAYS1", "ILLDAYS2", "ILLDAYS2", "ILLDAYS3", "ILLDAYS4","ILLDAYS5","ILLDAYS6","ILLDAYS7")) {
    x[x %in% c("7")] <- NA
  }
  if (varname %in% c("JSATYP")) {
    x[x %in% c("4")] <- NA
  }
  if (varname %in% c("LEVQUL22")) {
    x[x %in% c("12")] <- NA
  }
  
  
  num_na <- sum(!is.na(x))
  suppressWarnings(xn <- as.numeric(x))
  if (sum(!is.na(xn)) > num_na) {
    return(x)
  } else {
    return(xn)
  }
  
}

pre_clean.1921 <- as.data.frame(
  mapply(clean_missing, pre_clean.1921, names(pre_clean.1921), SIMPLIFY = FALSE)
)
pre_clean.2224 <- as.data.frame(
  mapply(clean_missing, pre_clean.2224, names(pre_clean.2224), SIMPLIFY = FALSE)
)

# Handle HIQUL
pre_clean.1921$HIQUL_D <- pre_clean.1921$HIQUL15D
pre_clean.2224$HIQUL_D <- coalesce(pre_clean.2224$HIQUL22D, pre_clean.2224$HIQUL15D)

weights.categorical <- c("PRXREL", "IOUTCOME")
weights.continuous <- c("PWTA22C", "NPWT22C")

variables.categorical <- c(
  #DEMOGRAPHICS
  "SEX", # Sex
  "AAGE", # Age bands
  "ETHUKEUL", # Ethnicity 9 cat, UK Level
  "MARSTA", # Marital Status
  "NATOX7_EUL_Main", # Nationality
  "GOR9d", # Regions
  "GOVTOF", # Regions #2,
  
  "INECAC05", # Economic activity ILO
  "FTPTW", # Full time or Part time work
  "FTPTWK", # Full time or part time in main job
  "JOBTYP", # Permanency of job

  "SC20MMJ", # SOC2020 major codes, main job 
  "SC20MMN", # SOC2020 minor codes, main job
  
  "INDE07M", # Industry sector in main job
  "PUBLICR", # Public vs Private sector
  
  "HIQUL_D", # Highest Education
  "ENROLL", # Enrolled in education?
  "STUCUR", # YesNo -Full time student?
  
  "LIMACT", # Does health problem limit activity?
  "LNGLST" # Lasted or expect to last 1yr+?
)
variables.continuous <- c(
  "TTACHR", # Total actual hours
  "TTUSHR", # Total usual hours
  
  "GRSSWK", # Gross weekly pay in main job
  "HOURPAY" # Gross hourly pay
)

apply_typing <- function(df, var.cat, var.con, weight.cat, weight.con) {
  df[var.cat] <- lapply(
    df[var.cat], function(x) factor(x)
    )
  df[var.con] <- lapply(
    df[var.con], function(x) as.numeric(x)
  )
  df[weight.cat] <- lapply(
    df[weight.cat], function(x) factor(x)
  )
  df[weight.cat] <- lapply(
    df[weight.cat], function(x) as.numeric(x)
  )
  
  return(df)
}

pre_clean.1921 <- apply_typing(pre_clean.1921, variables.categorical, variables.continuous, weights.categorical, weights.continuous)
pre_clean.2224 <- apply_typing(pre_clean.2224, variables.categorical, variables.continuous, weights.categorical, weights.continuous)


### Final steps

add_refmonth <- function(df) {
  df %>% mutate(
    REFWKY = as.numeric(REFWKY),
    REFWKM = as.numeric(REFWKM),
    REFMONTH = make_date(year=REFWKY, month=REFWKM, day=1)
  )

}

stratified_sampling <- function(df, sample_frac, keep_vars) {
  df %>% group_by(REFMONTH) %>%
    sample_frac(sample_frac) %>%
    ungroup() %>%
    select(all_of(keep_vars))
  }

prep_clean <- function(df, sample_frac, var.cat, var.con, weight.cat, weight.con) {
  df <- add_refmonth(df)
  keep_vars <- c(var.cat, var.con, weight.cat, weight.con, "REFMONTH")
  df_sub <- stratified_sampling(df, sample_frac, keep_vars)
  
  return(df_sub)
}

sample_frac <- 0.15

cleaned.1921 <- prep_clean(pre_clean.1921, sample_frac, variables.categorical, variables.continuous, weights.categorical, weights.continuous)
cleaned.2224 <- prep_clean(pre_clean.2224, sample_frac, variables.categorical, variables.continuous, weights.categorical, weights.continuous)

sapply(cleaned.1921[variables.categorical], class)
sapply(cleaned.2224[variables.categorical], class)
sapply(cleaned.1921[variables.continuous], class)
sapply(cleaned.2224[variables.continuous], class)
sapply(cleaned.1921[weights.categorical], class)
sapply(cleaned.2224[weights.categorical], class)
sapply(cleaned.1921[weights.continuous], class)
sapply(cleaned.2224[weights.continuous], class)

# then redraft rpt struct