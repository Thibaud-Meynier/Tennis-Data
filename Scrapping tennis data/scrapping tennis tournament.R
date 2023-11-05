get_tournament=function(tournament,year) {
  # import des librairies
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  
  url=paste('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/',sep='')
  # extraire les données à partir de la page Web
  page <- read_html(url)
  matches <- page %>% html_nodes("table.result") %>% html_table()
  # nettoyer et organiser les données extraites
  matches <- matches[[1]]
  colnames(matches)=matches[1,]
  matches=matches[-1,]
  # rennomer toutes les colonnes
  colnames(matches)=c('Date','Round','Player','Score','Set1','Set2','Set3','Set4','Set5','Odd_W','Odd_L','Info')
  matches=matches[matches$Player!='',]
  # extraire les jeux des sets tb
  matches$Set1=as.numeric(substr(matches$Set1, 1, 1))
  matches$Set2=as.numeric(substr(matches$Set2, 1, 1))
  matches$Set3=as.numeric(substr(matches$Set3, 1, 1))
  # On transforme la date
  matches$Date=dmy(paste(gsub("['^.^']", "-", substr(matches$Date,1,5)),sep="-",year))
  matches$Player=str_replace(matches$Player,'\\s+\\((.*$)',"")
  matches$test=c(1:nrow(matches))%%2
  # un data set winner et undata set looser
  winner=matches[matches$test==1,]
  colnames(winner)=c('Date','Round','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Odd_W','Odd_L','Info')
  winner=winner[,c(1:10)]
  loser=matches[matches$test==0,]
  colnames(loser)=c('Date','Round','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Odd_W','Odd_L','Info')
  loser=loser[,c(1:9,11)]
  data_set=winner[,1:2]
  
  for (i in 1:8){
    data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
    n=ncol(data_set)
    colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
    #print(i)
  }
  data_set$Score_W=as.numeric(as.character(data_set$Score_W))
  data_set$Score_L=as.numeric(as.character(data_set$Score_L))
  data_set$Odd_W=as.numeric(as.character(data_set$Odd_W))
  data_set$Odd_L=as.numeric(as.character(data_set$Odd_L))
  data_set$info=ifelse(data_set$Score_W<=1,'Walkover or Retired','Completed')
  data_set$Outcome=ifelse(data_set$Odd_W<=data_set$Odd_L,'Fav_W','Out_W')
  data_set=cbind(tournament,data_set)
  #Passage du nom du tournoi en maj
  data_set$tournament=str_to_title(as.character(data_set$tournament))
  data_set$N_match=c(1:nrow(data_set))
  data_set=data_set[order(data_set$N_match,decreasing=T),]
  
  return(data_set)
}

year=2023
tournament=c('hertogenbosch','stuttgart','halle',"queen-s-club","mallorca","eastbourne")
df_final=data.frame(matrix(nrow=0,ncol=21,NA))

for (i in tournament){
  df=get_tournament(i,year)
  df_final=rbind(df_final,df)
  rm(df)
  print(i)
}

df_bet=data.frame("Tournament"=df_final$tournament,"Round"=df_final$Round,"Outcome"=df_final$Outcome,
                  "Odd_play"=ifelse(df_final$Odd_W<df_final$Odd_L,df_final$Odd_L,df_final$Odd_W))

df_bet$Gain=ifelse(df_bet$Outcome=='Out_W',(df_bet$Odd_play*1-1),-1)

plot(cumsum(na.omit(df_bet$Gain)),type='l')

table(na.omit(df_bet$Outcome))

mean(df_bet$Odd_play,na.rm=T)

odd_dist=hist(df_bet$Odd_play,breaks = 30)

n=length(odd_dist$breaks)

odd=matrix(nrow=n,ncol=4,NA)
odd[,1][2:n]=odd_dist$breaks[1:(n-1)]
odd[,2]=odd_dist$breaks
odd[,3][2:n]=odd_dist$counts
odd=odd[-1,]

n2=length(hist(df_bet[df_bet$Outcome=='Out_W',]$Odd_play,breaks = 20)$counts)

odd[,4][1:n2]=hist(df_bet[df_bet$Outcome=='Out_W',]$Odd_play,breaks = 20)$counts

# Bet odd between 2 and max_odd_winner
obs=min(which(is.na(odd[,4])))-1
odd_max=odd[obs,2]

df_bet2=df_bet[df_bet$Odd_play>=2&df_bet$Odd_play<=odd_max,]

plot(cumsum(na.omit(df_bet2$Gain)),type='l')

table(df_bet2$Outcome)
