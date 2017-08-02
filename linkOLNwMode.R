library(tidyverse)

# this file is available from the 2009 Transferability study
# Data dictionary https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/national_household_travel_survey/transfterability_statistics
# download the data here https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/national_household_travel_survey/census
# available in the repository as well https://github.com/vargovargo/ACS_ITHIM/tree/master/data
file <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_2009_transfer_US.txt"

OLN <- read.table(file, sep="\t", as.is = T, skip=319, stringsAsFactors=FALSE)
OLNheader <- read.table(file, sep="\t", as.is = T, nrows = 1)
colnames(OLN) <- unlist(OLNheader)
OLN <- select(OLN, geoid, Cluster, urban_group, est_pmiles2007_11) %>%
  mutate(STFIPSnum = as.numeric(ifelse(nchar(geoid) == 10, stringr::str_sub(geoid, 1, 1), stringr::str_sub(geoid, 1, 2))),
         CNTYFIPSnum = as.numeric(ifelse(nchar(geoid) == 10, stringr::str_sub(geoid, 2, 4), stringr::str_sub(geoid, 3, 5))))

STclusterKey <- data.frame(Cluster = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6), 
                           STFIPSnum = c(9,23,25,33,34,36,42,44,50,17,18,19,20,26,27,29,31,38,39,46,55,10,11,12,13,24,37,45,51,54,1,5,21,22,28,40,47,48,4,8,16,30,32,35,49,56,2,6,15,41,53),
                           stateName = c("Connecticut",
                                        "Maine",
                                        "Massachusetts",
                                        "New Hampshire",
                                        "New Jersey",
                                        "New York",
                                        "Pennsylvania",
                                        "Rhode Island",
                                        "Vermont",
                                        "Illinois",
                                        "Indiana",
                                        "Iowa",
                                        "Kansas",
                                        "Michigan",
                                        "Minnesota",
                                        "Missouri",
                                        "Nebraska",
                                        "North Dakota",
                                        "Ohio",
                                        "South Dakota",
                                        "Wisconsin",
                                        "Delaware",
                                        "District of Columbia",
                                        "Florida",
                                        "Georgia",
                                        "Maryland",
                                        "North Carolina",
                                        "South Carolina",
                                        "Virginia",
                                        "West Virginia",
                                        "Alabama",
                                        "Arkansas",
                                        "Kentucky",
                                        "Louisiana",
                                        "Mississippi",
                                        "Oklahoma",
                                        "Tennessee",
                                        "Texas",
                                        "Arizona",
                                        "Colorado",
                                        "Idaho",
                                        "Montana",
                                        "Nevada",
                                        "New Mexico",
                                        "Utah",
                                        "Wyoming",
                                        "Alaska",
                                        "California",
                                        "Hawaii",
                                        "Oregon",
                                        "Washington")
)

# this is a file created by Maggie from NHTS
# it was created in Access and we need to check in with her to see how it was created and figure out how to recreate
# available in the repository as well https://github.com/vargovargo/ACS_ITHIM/tree/master/data
modeFile <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_clusterByMode.csv"

NHTSmode <- read.csv(modeFile, header=T) %>% select(-TRP_MI_AVG) %>% 
  mutate(urban_group = ifelse(Urbanicity %in% c("Second City","Suburban"), 2,  
                              ifelse(Urbanicity == "Urban", 1, 3)),
         modeNew = factor(ifelse(as.character(Mode) %in% c("Car","Van","SUV","Pickup Truck", "Motorcycle"),"Drive",
                                 ifelse(as.character(Mode) == "Walk","Walk",
                                        ifelse(as.character(Mode) == "Bicycle","Bicycle",
                                               ifelse(as.character(Mode) %in% c("Local Public Bus","City to City Bus","street car/trolley","Subway/elevated train"),"Transit","Other")))),
                          levels = c("Drive","Walk","Bicycle","Transit","Other"))) %>% 
  full_join(STclusterKey) %>%
  group_by(STFIPSnum,  urban_group, modeNew) %>% 
  summarize(avgTravelTime = mean(TRVL_MIN_AVG, na.rm=T)) %>%
  ungroup() %>%
  complete(STFIPSnum, urban_group, modeNew, fill = list(avgTravelTime = 0) )

tractTravel <- left_join(OLN, NHTSmode)




