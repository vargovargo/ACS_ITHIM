library(jsonlite)
library(tidyverse)

state = "55"
county = "025"

getCountyMeansTravel <- function(state, county) {
  varString <-
    "B08301_001E,B08301_001M,B08301_002E,B08301_002M,B08301_010E,B08301_010M,B08301_016E,B08301_016M,B08301_017E,B08301_017M,B08301_018E,B08301_018M,B08301_019E,B08301_019M,B08301_020E,B08301_020M,B08301_021E,B08301_021M"
  
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
  ACScommute <-
    as.data.frame(fromJSON(
      paste0(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=county:",
        county,
        "&in=state:",
        state,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038"
      )
    ))
  ACScommute <- ACScommute[-1,]
  
  colnames(ACScommute) <-
    c(
      "name",
      "B08301_001E",
      "B08301_001M",
      "B08301_002E",
      "B08301_002M",
      "B08301_010E",
      "B08301_010M",
      "B08301_016E",
      "B08301_016M",
      "B08301_017E",
      "B08301_017M",
      "B08301_018E",
      "B08301_018M",
      "B08301_019E",
      "B08301_019M",
      "B08301_020E",
      "B08301_020M",
      "B08301_021E",
      "B08301_021M",
      "state",
      "county"
    )
  
  
  xwk <-
    data.frame(
      variable = c(
        "B08301_001",
        "B08301_002",
        "B08301_010",
        "B08301_016",
        "B08301_017",
        "B08301_018",
        "B08301_019",
        "B08301_020",
        "B08301_021"
      ),
      varName = c(
        "workers",
        "drive",
        "transit",
        "other",
        "drive",
        "bicycle",
        "walk",
        "other",
        "other"
      )
    )
  
  
  CNTYtravel <-
    ACScommute %>% gather(
      B08301_001E,
      B08301_001M,
      B08301_002E,
      B08301_002M,
      B08301_010E,
      B08301_010M,
      B08301_016E,
      B08301_016M,
      B08301_017E,
      B08301_017M,
      B08301_018E,
      B08301_018M,
      B08301_019E,
      B08301_019M,
      B08301_020E,
      B08301_020M,
      B08301_021E,
      B08301_021M,
      key = variable,
      value = value
    ) %>%
    mutate(metric = factor(stringr::str_sub(variable,-1,-1)),
           variable = factor(stringr::str_sub(variable, 1,-2))) %>%
    spread(key = metric, value = value) %>%
    mutate(E = as.numeric(E),
           M = as.numeric(M))  %>%
    mutate(lower95CI = E - (1.96 / 1.645 * M),
           upper95CI = E + (1.96 / 1.645 * M)) %>%
    left_join(xwk) %>%
    group_by(name, state, county, varName) %>%
    summarize(
      estimate = sum(E, na.rm = T),
      lower95CI = sum(lower95CI, na.rm = T),
      upper95CI = sum(upper95CI, na.rm = T)
    ) %>%
    filter(varName != "workers")
  
  
  return(CNTYtravel)
  
}


# example
# DaneTravelMeans <- getCountyMeansTravel(state = 55,county = 025)
