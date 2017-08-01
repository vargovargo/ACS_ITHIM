library(tidyverse)

file <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_2009_transfer_US.txt"

OLN <- read.table(file, sep="\t", as.is = T, skip=319, stringsAsFactors=FALSE)
OLNheader <- read.table(file, sep="\t", as.is = T, nrows = 1)
colnames(OLN) <- unlist(OLNheader)

modeFile <- "~/Documents/GitHub/ACS_ITHIM/data/NHTS_clusterByMode.csv"

NHTSmode <- read.csv(modeFile, header=T) %>% spread()


