library(dataRetrieval)
library(stringr)
library(withr)

IDs <- read.csv("GagesUSGS.csv")


IDs$ID <- with_options(
  c(scipen = 999),
  str_pad(IDs$ID,8,pad ="0")
)


for(y in 1:length(IDs$ID)){
  siteNumber <- IDs$ID[y] 
  parameterCd <- c("99133","00060") #select parameter 
  startDate <- "2012-10-01"
  endDate <- "2018-09-30"
  
  no_dis <- readNWISdv(siteNumber, parameterCd, startDate, endDate)
  
  form = sprintf('no_dis%s.csv', IDs$ID[y])
  write.csv(no_dis, file = form)
}
setwd("C:/Users/Jonathan/Documents")
