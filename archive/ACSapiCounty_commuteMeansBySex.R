rm(list=ls())
library(jsonlite)
library(tidyverse)

singleSex <- function(varString, sex) {
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
      "personsE",
      "personsM",
      "carTruckVanE",
      "carTruckVanM",
      "publicTransportE",
      "publicTransportM",
      "bicycleE",
      "bicycleM",
      "walkE",
      "walkM",
      "taxiMotoE",
      "taxiMotoM",
      "workHomeE",
      "workHomeM",
      "state",
      "county"
    )
  
  
  xwk <-
    data.frame(
      variable = c(
        "persons",
        "carTruckVan",
        "publicTransport",
        "bicycle",
        "walk",
        "taxiMoto",
        "workHome"
      ),
      varName = c(
        "workers",
        "drive",
        "transit",
        "bicycle",
        "walk",
        "other",
        "other"
      )
    )
  
  
  CNTYtravel <-
    ACScommute %>% gather(
      personsE,
      personsM,
      carTruckVanE,
      carTruckVanM,
      publicTransportE,
      publicTransportM,
      bicycleE,
      bicycleM,
      walkE,
      walkM,
      taxiMotoE,
      taxiMotoM,
      workHomeE,
      workHomeM,
      key = variable,
      value = value
    ) %>%
    mutate(metric = factor(stringr::str_sub(variable, -1, -1)),
           variable = factor(stringr::str_sub(variable, 1, -2))) %>%
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
    mutate(sex = sex) %>%
    filter(varName != "workers")
}

getCountyMeansTravelBySex <- function(state, county) {
  
  varStringTOTAL <-
    "B08406_001E,B08406_001M,B08406_002E,B08406_002M,B08406_008E,B08406_008M,B08406_014E,B08406_014M,B08406_015E,B08406_015M,B08406_016E,B08406_016M,B08406_017E,B08406_017M"
  
  varStringMALE <-
    "B08406_018E,B08406_018M,B08406_019E,B08406_019M,B08406_025E,B08406_025M,B08406_031E,B08406_031M,B08406_032E,B08406_032M,B08406_033E,B08406_033M,B08406_034E,B08406_034M"
  
  varStringFEMALE <-
    "B08406_035E,B08406_035M,B08406_036E,B08406_036M,B08406_042E,B08406_042M,B08406_048E,B08406_048M,B08406_049E,B08406_049M,B08406_050E,B08406_050M,B08406_051E,B08406_051M"
  
  
   #tblTOT <- singleSex(varStringTOTAL, "TOTAL")
   tblMAL <- singleSex(varStringMALE, "MALE")
   tblFEM <- singleSex(varStringFEMALE, "FEMALE")
   
   
   all <- bind_rows(tblMAL, tblFEM)

  return(all)
  
}


# example
# DaneTravelMeansBySex <- getCountyMeansTravelBySex(state = 55,county = 025)
