# scrapp rank 

library(tidyverse)
library(lubridate)

year=2016

# Filtrer les lundis
days_year <- seq.Date(as.Date(paste(year, "-01-01", sep = "")), 
                      as.Date(paste(year, "-12-31", sep = "")), by = "day")

mondays <- days_year[weekdays(days_year) == "lundi"] %>% as.data.frame()

colnames(mondays)[1]="Date"

mondays$week=week(mondays$Date)

rank=data.frame()

for (i in 1:nrow(mondays)){
  
  date=mondays$Date[i]
  
  data=rank_scrap(date)
  
  data$Date=date
  
  data$Year=year(data$Date)
  
  data$Week=week(date)
  
  rank=rbind(rank,data)
  
  print(date)
  
}

save(rank,file = paste0(getwd(),"/Scrapping tennis data/","RANK_ATP_",year,".RData"))
