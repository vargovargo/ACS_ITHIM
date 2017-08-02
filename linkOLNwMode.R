library(tidyverse)

# this file is available from the 2009 Transferability study
# Data dictionary https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/national_household_travel_survey/transfterability_statistics
# download the data 
file <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_2009_transfer_US.txt"

OLN <- read.table(file, sep="\t", as.is = T, skip=319, stringsAsFactors=FALSE)
OLNheader <- read.table(file, sep="\t", as.is = T, nrows = 1)
colnames(OLN) <- unlist(OLNheader)


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
modeFile <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_clusterByMode.csv"

NHTSmode <- read.csv(modeFile, header=T) %>% select(-TRP_MI_AVG) %>% mutate(urban_group = ifelse(Urbanicity %in% c("Second City","Suburban"), 2, 
                                                                                   ifelse(Urbanicity == "Urban", 1, 3)))



