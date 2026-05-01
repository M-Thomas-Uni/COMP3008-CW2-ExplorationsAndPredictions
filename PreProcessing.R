
library(dplyr)
library(tidyverse)

data_19to21 <- read.csv("data/AnnualPopulationSurvey_Jan2019_Dec2021.csv")
data_22to24 <- read.csv("data/AnnualPopulationSurvey_Jan2022_Dec2024.csv")

###clean names (janitor)
pre_clean.1921 <- rename(
  
) %>% select(-discurr13, -starts_with("HEALPB"), -starts_with("QUAL_"), -starts_with("SC2010"))

pre_clean.2224 <- clean_names(data_22to24) %>% rename(
  gGOR9d = GOR9DCENSUS2021,
  CombinedAuthorities = CombinedAuthoritiescensus2021
) %>% select( -starts_with("flex"), -fled22)

##get uniques to 1921 // 2224, then common (interesect, setdiffs)
vnames.1921 <- names(pre_clean.1921) 
vnames.2224 <- names(pre_clean.2224) 

common <- intersect(vnames.1921, vnames.2224)
only_1921 <- setdiff(vnames.1921, vnames.2224)
only_2224 <- setdiff(vnames.2224, vnames.1921)

length(common)
length(only_1921)
length(only_2224)

only_1921
only_2224


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
    soc10 <- soc2010_vec[i]
    sex   <- sex_vec[i]
    
    # If SOC2010 code missing, return NA
    if (is.na(soc10) || soc10 == "") {
      out[i] <- NA
      next
    }
    
    # Retrieve all possible mappings
    rows <- soc_map_list[[as.character(soc10)]]
    
    if (is.null(rows)) {
      out[i] <- NA
      next
    }
    
    # Choose correct weight column
    if (sex == 1) {
      w <- rows$men_avg
    } else if (sex == 2) {
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

soc_vars <- c("sc10lmn", "sc10lmn", "sc10lmn")

run_harmonise <- function (df, occ_vars) {
  for (v in occ_vars) {
    new_v <- sub("sc10", "sc20", v)
    df[[new_v]] <- harmonise_soc_vector(
      soc2010_vec = df[[v]],
      sex_vec = df$sex,
      soc_map_list = soc_list
    )
  }
  return(df)
}

pre_clean.1921 <- run_harmonise(pre_clean.1921, soc_vars)

vnames.1921 <- names(pre_clean.1921) 
vnames.2224 <- names(pre_clean.2224) 

common <- intersect(vnames.1921, vnames.2224)
only_1921 <- setdiff(vnames.1921, vnames.2224)
only_2224 <- setdiff(vnames.2224, vnames.1921)

length(common)
length(only_1921)
length(only_2224)

only_1921
only_2224
common



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
  if (varname %in% c("HIQUL15D", "HIQUL22D", "ILLDAYS1", "ILLDAYS2", "ILLDAYS2", "ILLDAYS3", "ILLDAYS4","ILLDAYS5","ILLDAYS6","ILLDAYS7",)) {
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

# basic demographics
# geography (region/county + lad?)

# ILO emp status
# full vs part
# perm vs temp
# usual weekly hrs
# industry sector (SIC)
# occupation (SOC) 

# education (highest qual, whether in education, apprenticeship, training)
# health + disabilities
# household (income, own/rent, earners, children)
# weightings


# then redraft rpt struct