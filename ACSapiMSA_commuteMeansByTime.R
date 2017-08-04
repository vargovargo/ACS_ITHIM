library(jsonlite)
library(tidyverse)


# function to process single mode
singleModeByTime <- function(varString, mode) {
  # Example
  # https://api.census.gov/data/2015/acs5?get=NAME,B08534_011E,B08534_061E,B08534_101E,B08534_111E&for=metropolitan+statistical+area/micropolitan+statistical+area:38900
  
  ACScommute <-
    as.data.frame(fromJSON(
      paste0(
        "https://api.census.gov/data/2015/acs5?get=NAME,",
        varString,
        "&for=metropolitan+statistical+area/micropolitan+statistical+area:",
        msa,
        "&key=f78d6b6c18608edc379b5a06c55407ceb45e7038"
      )
    ))
  ACScommute <- ACScommute[-1,]
  
  colnames(ACScommute) <-
    c(
      "name",
      "minLT10E",
      "minLT10M",
      "min10to14E",
      "min10to14M",
      "min15to19E",
      "min15to19M",
      "min20to24E",
      "min20to24M",
      "min25to29E",
      "min25to29M",
      "min30to34E",
      "min30to34M",
      "min35to44E",
      "min35to44M",
      "min45to59E",
      "min45to59M",
      "minOver60E",
      "minOver60M",
      "msa"
    )
  
  CNTYtravel <-
    ACScommute %>% gather(
      minLT10E,
      minLT10M,
      min10to14E,
      min10to14M,
      min15to19E,
      min15to19M,
      min20to24E,
      min20to24M,
      min25to29E,
      min25to29M,
      min30to34E,
      min30to34M,
      min35to44E,
      min35to44M,
      min45to59E,
      min45to59M,
      minOver60E,
      minOver60M,
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
    group_by(name, msa, variable) %>%
    summarize(
      estimate = sum(E, na.rm = T),
      lower95CI = sum(lower95CI, na.rm = T),
      upper95CI = sum(upper95CI, na.rm = T)
    ) %>%
    mutate(mode = mode, 
           travelTime = factor(variable, levels =c("minLT10","min10to14","min15to19","min20to24","min25to29","min30to34","min35to44","min45to59","minOver60"))) %>%
    arrange(mode, travelTime) %>%
    select(-variable)
}




# function to process county data for each mode
getMSAMeansTravelByTime <- function(msa) {
  
  varStringCAR <-
    "B08534_012E,B08534_012M,B08534_013E,B08534_013M,B08534_014E,B08534_014M,B08534_015E,B08534_015M,B08534_016E,B08534_016M,B08534_017E,B08534_017M,B08534_018E,B08534_018M,B08534_019E,B08534_019M,B08534_020E,B08534_020M"
  
  varStringTRANSIT <-
    "B08534_062E,B08534_062M,B08534_063E,B08534_063M,B08534_064E,B08534_064M,B08534_065E,B08534_065M,B08534_066E,B08534_066M,B08534_067E,B08534_067M,B08534_068E,B08534_068M,B08534_069E,B08534_069M,B08534_070E,B08534_070M"
  
  varStringWALK <-
    "B08534_102E,B08534_102M,B08534_103E,B08534_103M,B08534_104E,B08534_104M,B08534_105E,B08534_105M,B08534_106E,B08534_106M,B08534_107E,B08534_107M,B08534_108E,B08534_108M,B08534_109E,B08534_109M,B08534_110E,B08534_110M"
  
  varStringBIKE <-
    "B08534_112E,B08534_112M,B08534_113E,B08534_113M,B08534_114E,B08534_114M,B08534_115E,B08534_115M,B08534_116E,B08534_116M,B08534_117E,B08534_117M,B08534_118E,B08534_118M,B08534_119E,B08534_119M,B08534_120E,B08534_120M"
  
   tblCAR <- singleModeByTime(varStringCAR, "drive")
   tblTRANSIT <- singleModeByTime(varStringTRANSIT, "transit")
   tblWALK <- singleModeByTime(varStringWALK, "walk")
   tblBIKE <- singleModeByTime(varStringWALK, "bicycle")
   
   all <- bind_rows(tblCAR, tblTRANSIT, tblWALK, tblBIKE)

  return(all)
  
}


# example
# PortlandMSATravelMeansByTime <- getMSAMeansTravelByTime(msa = 38900)


