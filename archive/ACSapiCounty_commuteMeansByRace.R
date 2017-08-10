library(jsonlite)
library(tidyverse)

singleRace <- function(varString, race) {
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
      "driveE",
      "driveM",
      "carPoolE",
      "carPoolM",
      "transitE",
      "transitM",
      "walkE",
      "walkM",
      "bicycleE",
      "bicycleM",
      "workHomeE",
      "workHomeM",
      "state",
      "county"
    )
  
  CNTYtravel <-
    ACScommute %>% gather(
      driveE,
      driveM,
      carPoolE,
      carPoolM,
      transitE,
      transitM,
      walkE,
      walkM,
      bicycleE,
      bicycleM,
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
    group_by(name, state, county, variable) %>%
    summarize(
      estimate = sum(E, na.rm = T),
      lower95CI = sum(lower95CI, na.rm = T),
      upper95CI = sum(upper95CI, na.rm = T)
    ) %>%
    mutate(race = race)
}

getCountyMeansTravelByRace <- function(state, county) {
  
  varStringWHITE <-
    "B08505A_002E,B08505A_002M,B08505A_003E,B08505A_003M,B08505A_004E,B08505A_004M,B08505A_005E,B08505A_005M,B08505A_006E,B08505A_006M,B08505A_007E,B08505A_007M"
  varStringBLACK <-
    "B08505B_002E,B08505B_002M,B08505B_003E,B08505B_003M,B08505B_004E,B08505B_004M,B08505B_005E,B08505B_005M,B08505B_006E,B08505B_006M,B08505B_007E,B08505A_007M"
  varStringINDIAN <-
    "B08505C_002E,B08505C_002M,B08505C_003E,B08505C_003M,B08505C_004E,B08505C_004M,B08505C_005E,B08505C_005M,B08505C_006E,B08505C_006M,B08505C_007E,B08505A_007M"
  varStringASIAN <-
    "B08505D_002E,B08505D_002M,B08505D_003E,B08505D_003M,B08505D_004E,B08505D_004M,B08505D_005E,B08505D_005M,B08505D_006E,B08505D_006M,B08505D_007E,B08505A_007M"
  varStringISLANDER <-
    "B08505E_002E,B08505E_002M,B08505E_003E,B08505E_003M,B08505E_004E,B08505E_004M,B08505E_005E,B08505E_005M,B08505E_006E,B08505E_006M,B08505E_007E,B08505A_007M"
  varStringOTHER <-
    "B08505F_002E,B08505F_002M,B08505F_003E,B08505F_003M,B08505F_004E,B08505F_004M,B08505F_005E,B08505F_005M,B08505F_006E,B08505F_006M,B08505F_007E,B08505A_007M"
  varStringTWO <-
    "B08505G_002E,B08505G_002M,B08505G_003E,B08505G_003M,B08505G_004E,B08505G_004M,B08505G_005E,B08505G_005M,B08505G_006E,B08505G_006M,B08505G_007E,B08505A_007M"
  varStringNONHISP <-
    "B08505H_002E,B08505H_002M,B08505H_003E,B08505H_003M,B08505H_004E,B08505H_004M,B08505H_005E,B08505H_005M,B08505H_006E,B08505H_006M,B08505H_007E,B08505A_007M"
  varStringHISP <-
    "B08505I_002E,B08505I_002M,B08505I_003E,B08505I_003M,B08505I_004E,B08505I_004M,B08505I_005E,B08505I_005M,B08505I_006E,B08505I_006M,B08505I_007E,B08505A_007M"
  
  
   tblWHITE <- singleRace(varStringWHITE, "White")
   tblBLACK <- singleRace(varStringBLACK, "Black")
   tblINDIAN <- singleRace(varStringINDIAN, "American Indian")
   tblASIAN <- singleRace(varStringASIAN, "Asian")
   tblISLANDER <- singleRace(varStringISLANDER, "Pacific Islander")
   tblOTHER <- singleRace(varStringOTHER, "Other")
   tblTWO <- singleRace(varStringTWO, "Multiple races")
   tblNONHISP <- singleRace(varStringNONHISP, "White Non-Hispanic")
   tblHISP <- singleRace(varStringHISP, "Hispanic")
   
   
   all <- bind_rows(tblWHITE, tblBLACK, tblINDIAN, tblASIAN, tblISLANDER, tblOTHER, tblTWO, tblNONHISP, tblHISP) %>%
     within({
       race <- factor(race, levels = c("White","Black","American Indian","Asian","Pacific Islander","Other","Multiple races","White Non-Hispanic", "Hispanic"))
       mode <- factor(variable, levels = c("drive","carPool","walk","bicycle","transit","workHome"))
     }) 

  return(all)
}



# example
DaneTravelMeansByRace <- getCountyMeansTravelByRace(state = 55,county = 025)
ggplot(DaneTravelMeansByRace, aes(x = race, y = estimate, fill = mode)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("Number of Commuters") + xlab("Workers Annual Salary (2012 adj.)")
