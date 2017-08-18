rm(list=ls())
library(jsonlite)
library(tidyverse)

lookupCounty <- function(word) {

  countyList <-  as.data.frame(fromJSON("https://api.census.gov/data/2015/acs5?get=NAME&for=county:*&in=state:*&key=f78d6b6c18608edc379b5a06c55407ceb45e7038")) %>% 
    .[-1,] %>% 
    mutate(county = matrix(unlist(stringr::str_split(V1, ", ")), ncol = 2, byrow = T)[,1], 
           state = matrix(unlist(stringr::str_split(V1, ", ")), ncol = 2, byrow = T)[,2]) %>%
    select(county, state, V1, V2, V3)
  
  names(countyList) <- c("county","state","countyState","countyFIPS","stateFIPS")
 
  return(countyList[grep(word, countyList$countyState),])
  
}

lookupMSA <- function(word) {
  
  msaList <- as.data.frame(fromJSON("http://api.census.gov/data/2015/acs5?get=NAME&for=metropolitan+statistical+area/micropolitan+statistical+area:*&key=f78d6b6c18608edc379b5a06c55407ceb45e7038")) %>%
    .[-1,]
  
  names(msaList) <- c("msa","msaFIPS")
  
  return(msaList[grep(word, msaList$msa),])
  
}
