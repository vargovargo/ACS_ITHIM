library(jsonlite)
library(tidyverse)

# function to process single mode
singleModeByIncome <- function(varString, mode, msa) {
  # https://api.census.gov/data/2015/acs5?get=NAME,B01001_001E&for=county:013&in=state:02
  
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
      "incLT10E",
      "incLT10M",
      "inc10to15E",
      "inc10to15M",
      "inc15to25E",
      "inc15to25M",
      "inc25to35E",
      "inc25to35M",
      "inc35to50E",
      "inc35to50M",
      "inc50to65E",
      "inc50to65M",
      "inc65to75E",
      "inc65to75M",
      "incOver75E",
      "incOver75M",
      "msa"
    )
  
  CNTYtravel <-
    ACScommute %>% gather(
      incLT10E,
      incLT10M,
      inc10to15E,
      inc10to15M,
      inc15to25E,
      inc15to25M,
      inc25to35E,
      inc25to35M,
      inc35to50E,
      inc35to50M,
      inc50to65E,
      inc50to65M,
      inc65to75E,
      inc65to75M,
      incOver75E,
      incOver75M,
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
           income = factor(variable, levels =c("incLT10","inc10to15","inc15to25","inc25to35","inc35to50","inc50to65","inc65to75","incOver75"))) %>%
    arrange(mode, income) %>%
    select(-variable)
}

# function to process county data for each mode
getCountyMeansTravelByIncome <- function(msa) {
  
  varStringCAR <-
    "B08519_011E,B08519_011M,B08519_012E,B08519_012M,B08519_013E,B08519_013M,B08519_014E,B08519_014M,B08519_015E,B08519_015M,B08519_016E,B08519_016M,B08519_017E,B08519_017M,B08519_018E,B08519_018M"
  
  varStringCARPOOL <-
    "B08519_020E,B08519_020M,B08519_021E,B08519_021M,B08519_022E,B08519_022M,B08519_023E,B08519_023M,B08519_024E,B08519_024M,B08519_025E,B08519_025M,B08519_026E,B08519_026M,B08519_027E,B08519_027M"
  
  varStringTRANSIT <-
    "B08519_029E,B08519_029M,B08519_030E,B08519_030M,B08519_031E,B08519_031M,B08519_032E,B08519_032M,B08519_033E,B08519_033M,B08519_034E,B08519_034M,B08519_035E,B08519_035M,B08519_036E,B08519_036M"
  
  varStringWALK <-
    "B08519_038E,B08519_038M,B08519_039E,B08519_039M,B08519_040E,B08519_040M,B08519_041E,B08519_041M,B08519_042E,B08519_042M,B08519_043E,B08519_043M,B08519_044E,B08519_044M,B08519_045E,B08519_045M"
  
  varStringBIKE <-
    "B08519_047E,B08519_047M,B08519_048E,B08519_048M,B08519_049E,B08519_049M,B08519_050E,B08519_050M,B08519_051E,B08519_051M,B08519_052E,B08519_052M,B08519_053E,B08519_053M,B08519_054E,B08519_054M"
  
  tblCAR <- singleModeByIncome(varStringCAR, "drive", msa)
  tblCARPOOL <- singleModeByIncome(varStringCARPOOL, "drive", msa)
  tblTRANSIT <- singleModeByIncome(varStringTRANSIT, "transit", msa)
  tblWALK <- singleModeByIncome(varStringWALK, "walk", msa)
  tblBIKE <- singleModeByIncome(varStringBIKE, "bicycle", msa)
  
  all <-
    bind_rows(tblCAR, tblCARPOOL, tblTRANSIT, tblWALK, tblBIKE) %>%
    group_by(name, msa, mode, income) %>%
    summarize(
      estimate = sum(estimate, na.rm = T),
      lower95CI = sum(lower95CI, na.rm = T),
      upper95CI = sum(upper95CI, na.rm = T)
    ) %>%
    within({
    income <- factor(income, levels = c("incLT10","inc10to15","inc15to25","inc25to35","inc35to50","inc50to65","inc65to75","incOver75"))
    mode <- factor(mode, levels = c("drive","walk","bicycle","transit"))
  }) 
  
  return(all)
  
}
# example
# getCountyMeansTravelByIncome(state = 55,county = 025) %>%  ggplot(aes(x=income, y=estimate, fill=mode)) + geom_bar(stat="identity")

PortlandTravelMeansByIncome <- getCountyMeansTravelByIncome(msa = 38900)
ggplot(PortlandTravelMeansByIncome, aes(x = income, y = estimate, fill = mode)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("Number of Commuters") + xlab("Workers Annual Salary (2012 adj.)") +coord_flip()


