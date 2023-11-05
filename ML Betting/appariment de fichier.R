# Import des librairies

library(ggpubr)
library(gridExtra)
library(grid)
library(tidyr)
library(tidyverse)
library(cowplot)
library(sqldf)
library(ggplot2)
library(readxl)
library(dplyr)
library(sjmisc)
library(stringr)
library(DescTools)

# import des données

year=2021

url="https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches"

path='C:/Users/Thiti/Desktop/data_tennis/Bet_data/'

link=paste(url,'_',year,'.csv',sep='')

link2=paste(path,year,'.xlsx',sep="")

data=read.csv2(link,sep=',')

data2=read_xlsx(path=link2)

# Manipulation des données

# names of players

patern2=c('Davis Cup')

new_df2 <- filter(data, grepl(paste(patern2, collapse='|'), data$tourney_name),.preserve=TRUE)

data$test2=data$tourney_name %in% new_df2$tourney_name # is in

# clean data

data=data[data$test2==FALSE,]

str(data$winner_name)
str(data2$Winner)

data$winner_name=as.character(data$winner_name)

names=distinct(data,winner_name,winner_id)
names2=distinct(data2,Winner)

#n=max(lengths(regmatches(names$winner_name, gregexpr(" ",names$winner_name))))

#str_split_fixed(names$winner_name," ",n)
#names$names=str_split_fixed(names$winner_name," ",n)

#etape 1 enlever tous les tirets des noms

names2$Winner2=gsub('-',' ',names2$Winner)

#etape 2

names2$Winner_name=NA

for (i in 1:nrow(names2)){
  
  names2$Winner_name[i]=str_replace(names2$Winner2[i],'\\s+[A-Za-z]\\.(.*$)',"")
  
  print(i)
  
}

# Noms à soucis
names$winner_name[names$winner_name=='Albert Ramos']='Albert Ramos Vinolas'
names$winner_name[names$winner_name=='Christopher Oconnell']='Christopher O Connell'
names2=rbind(names2,c('Mahut N.','Mahut N.','Mahut'))

#etape 3 test match noms

names$test=NA

for (i in 1:nrow(names)){
  # On stock les réponses du test
  df=data.frame('test'=str_contains(names$winner_name[i],names2$Winner_name))
  #on cherche quelle réponse est true pour prendre la valeur qui match
  n=which(df$test==TRUE)

# siplusieurs noms pareils on prend le nom vainqueur fichier cote dont la lettre du prenom est comprise dans le nom testé
  if (n>0){
    # on prend la première lettre du prénom du joueur
    t=StrLeft(names$winner_name[i],1)
    # on compte combien de fois cette lettre apparait dans le nom du vainqueur fichier cotes
    str_count(names2$Winner[n],t)
    # on prend l'obs qui contient la lettre du prénom
    o=which(str_count(names2$Winner[n],t)>=1)
    # on remplace par le bon nom
    names$test[i]=names2$Winner[n][o]
  }
  
  else {
    names$test[i]=names2$Winner[n]

  }
  print(i)
}

# tous les noms sont mappés

# faire pareil pour les tournois

# Tournois à problème 
data2$Tournament[data2$Tournament=='French Open']='Roland Garros'
data2$Location[data2$Tournament=='Belgrade Open']='Belgrade 2'
data$tourney_name=as.character(data$tourney_name)
data$tourney_name[data$tourney_name=='Canada Masters']='Toronto Masters'
data$tourney_name[data$tourney_name=='Tour Finals']='Masters Cup'

tournament=distinct(data,tourney_name,tourney_id)
tournament2=distinct(data2,Tournament,Location)
tournament$Tournament=gsub(' Masters|\'','',tournament$tourney_name)

tournament_merge=sqldf("select distinct a.*,b.*
                       from tournament a
                       left join tournament2 b on b.location=a.Tournament or b.Tournament=a.Tournament")

# voir pour les tournois (faire une indexation dans le xlsx directement)