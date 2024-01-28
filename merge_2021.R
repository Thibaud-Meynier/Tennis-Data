library(readxl)
library(reshape2)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(sqldf)
library(sjmisc)

#Annee 2021

##### IMPORT DES DONNEES #####

path_21='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2021.xlsx'

data_21=read_xlsx(path=path_21)

year=2021

url="https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches"

link=paste(url,'_',year,'.csv',sep='')

data=read.csv2(link,sep=',')

# player=read.csv2("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv",sep=",")
# 
# player$dob=as.Date(paste0(substr(player$dob,1,4),"-",
#                           substr(player$dob,5,6),"-",
#                           substr(player$dob,7,8)))
# 
# player=player %>% filter(year(dob)>=1970)

##### NORMALISATION DES TOURNOIS DANS LES 2 BASES #####

x=unique(data_21$Tournament) %>% data.frame()
z=unique(data$tourney_name) %>% data.frame()

z=sqldf("select distinct tourney_id,tourney_name,tourney_date,surface
        from data
        where tourney_name not like 'Davis Cup%'")

z$tourney_date=as.Date(paste0(substr(z$tourney_date,1,4),"-",substr(z$tourney_date,5,6),"-",substr(z$tourney_date,7,8)))

x=data_21 %>% 
  group_by(Tournament,Location,Surface,Series) %>% 
  summarise(Date=min(Date)) %>% 
  arrange(Date)

z$tourney_name2=str_remove(z$tourney_name," Masters| 1| 2")
#x$Location=str_remove(x$Location," ")

x$Date=as.Date(x$Date)

patern2=c('Davis Cup','Tokyo Olympics','Laver Cup','Atp Cup','United Cup','NextGen Finals')

z=z %>% filter(!tourney_name %in% patern2)

z$Week=paste0(year(z$tourney_date),"-",week(z$tourney_date))
x$Week=paste0(year(x$Date),"-",week(x$Date))

## Redressement des données de tournois

#Napoli, Nur-Sultan, Belgrade Us Open, Queens, Hertogenbosch, Canada

#x$Location[x$Location=="Nur-Sultan"]="Astana"
#x$Location[x$Location=="Napoli"]="Naples"
x$Location[x$Location=="'s-Hertogenbosch"]="Hertogenbosch"
x$Location[x$Location=="Queens Club"]="Queen's Club"
x$Series[x$Location=="Hamburg"]="ATP500"
x$Tournament[x$Tournament=="French Open"]="Roland Garros"
z$tourney_name2[z$tourney_name2=="Us Open"]="US Open"
z$tourney_name2[z$tourney_name2=="Canada"]="Toronto"
z$tourney_name2[z$tourney_name2=="s Hertogenbosch"]="Hertogenbosch"
z$tourney_name2[z$tourney_name=="Tour Finals"]="Turin"


##Table de pivot des tournois

Tournament_pivot=sqldf("select distinct a.*,b.tourney_name
      from x a
      left join z b on (b.tourney_name2=a.location and b.Week=a.Week) or (b.tourney_name2=a.Tournament and b.Week=a.Week)")

data2=data %>% select(tourney_date,tourney_name,match_num,winner_name,loser_name,round,winner_rank,loser_rank,winner_rank_points,loser_rank_points,winner_hand,loser_hand,winner_ht,loser_ht,winner_id,loser_id,winner_age,loser_age,winner_ioc,loser_ioc)

#On importe les typologies de tournoi

data2=sqldf("select a.*,c.Series
            from data2 a
            left join Tournament_pivot c on c.tourney_name=a.tourney_name")

Round_pivot=data.frame(Round_data2=unique(data_21$Round),
                       Round_data=c("R128","R64","QF","SF","F","R32","R16","RR"),
                       Round_H=c("R1","R2","QF","SF","F","R3","R4","RR")) %>% data.frame()

sqldf("select distinct round,tourney_name from data2 where series='ATP500'")

sqldf("select distinct round,tourney_name from data2 where series='ATP250'")

sqldf("select distinct round,tourney_name from data2 where series='Masters 1000'")

# Redressement du tour dans la base pour joindre avec la base avec les cotes

data2=sqldf("select *,(case 
when Series='ATP250' and tourney_name not in ('Winston-Salem','Great Ocean Road Open','Murray River Open') and round='R32' then 'R128' 
when Series='ATP250' and tourney_name not in ('Winston-Salem','Great Ocean Road Open','Murray River Open') and round='R16' then 'R64'

when Series='ATP250' and tourney_name in ('Winston-Salem','Great Ocean Road Open','Murray River Open') and round='R64' then 'R128'
when Series='ATP250' and tourney_name in ('Winston-Salem','Great Ocean Road Open','Murray River Open') and round='R32' then 'R64'
when Series='ATP250' and tourney_name in ('Winston-Salem','Great Ocean Road Open','Murray River Open') and round='R16' then 'R32'

when Series='ATP500' and tourney_name not in ('Barcelona','Washington','Dubai') and round='R32' then 'R128' 
when Series='ATP500' and tourney_name not in ('Barcelona','Washington','Dubai') and round='R16' then 'R64'

when Series='ATP500' and tourney_name in ('Barcelona','Washington','Dubai') and round='R64' then 'R128'
when Series='ATP500' and tourney_name in ('Barcelona','Washington','Dubai') and round='R32' then 'R64'
when Series='ATP500' and tourney_name in ('Barcelona','Washington','Dubai') and round='R16' then 'R32'

when Series='Masters 1000' and tourney_name not in ('Miami Masters','Indian Wells Masters') and round='R64' then 'R128'
when Series='Masters 1000' and tourney_name not in ('Miami Masters','Indian Wells Masters') and round='R32' then 'R64'
when Series='Masters 1000' and tourney_name not in ('Miami Masters','Indian Wells Masters') and round='R16' then 'R32'
else round end) as Round_red
from data2")

#Round harmonisées

data2=sqldf("select a.*,b.Round_H 
            from data2 a
            left join Round_pivot b on b.round_data=a.round_red")

# Redressement de la table des paris

data_21$Tournament[data_21$Tournament=="French Open"]="Roland Garros"
data_21$Series[data_21$Location=="Hamburg"]="ATP500"

data_21=data_21 %>% left_join(Round_pivot %>% select(Round_data2,Round_H),by=c("Round"="Round_data2")) %>% 
  left_join(Tournament_pivot %>% select(Tournament,Series,tourney_name),by=c("Tournament"="Tournament","Series"="Series"))


# Redressement sur le classement manquant de certains joueurs

data_21$WRank=ifelse(data_21$WRank=="N/A",1000,data_21$WRank)
data_21$WPts=ifelse(data_21$WPts=="N/A",10,data_21$WPts)
data_21$LRank=ifelse(data_21$LRank=="N/A",1000,data_21$LRank)
data_21$LPts=ifelse(data_21$LPts=="N/A",10,data_21$LPts)

data2$winner_rank=ifelse(is.na(data2$winner_rank)==T,1000,data2$winner_rank)
data2$winner_rank_points=ifelse(is.na(data2$winner_rank_points)==T,10,data2$winner_rank_points)
data2$loser_rank=ifelse(is.na(data2$loser_rank)==T,1000,data2$loser_rank)
data2$loser_rank_points=ifelse(is.na(data2$loser_rank_points)==T,10,data2$loser_rank_points)


# data_21=sqldf("select distinct a.*,b.Round_H,c.tourney_name
#             from data_21 a
#             left join Round_pivot b on b.round_data2=a.round
#             left join Tournament_pivot c on c.tournament=a.tournament and c.series=a.series") %>% select(!Date)

# Merging entre les 2 bases

merge=sqldf("select distinct a.*,b.date,b.W1,b.L1,b.W2,b.L2,b.W3,b.L3,b.W4,b.L4,b.W5,b.L5,b.Wsets,b.Lsets,
      b.PSW,b.PSL,b.AvgW,b.AvgL,b.comment
      from data2 a 
      left join data_21 b on B.Round_H=a.Round_H and b.WPts=a.winner_rank_points and b.LPts=a.loser_rank_points and b.tourney_name=a.tourney_name")

# Test de correspondance de la jointure 

NA_df=merge %>% filter(is.na(merge$AvgW)==TRUE)

Summary=NA_df %>% group_by(tourney_name) %>% summarise("NB"=n()) %>% filter(str_starts(tourney_name,"Davis Cup|Atp Cup|Laver Cup|NextGen Finals|Tokyo Olympics")==FALSE)

Summary2=data_21 %>% group_by(Tournament) %>% summarise("NB"=sum(is.na(AvgW))) %>% filter(NB>0)

# ADM 1991,Paul 1058,Karatsev 2109,Sinner 3100, Gerasimov 761,Rusuvuori 923,
# Tiafoe 1312, Mcdo 1105, Mannarino 1287, Millman 1100, Caruso 548, Pospisil 1000

# Redressement du masters

# Delray Harrison Christian Points 225 rang 280

data2$winner_rank_points[data2$winner_name=="Christian Harrison"&data2$tourney_name=="Delray Beach"]=225
data2$winner_rank[data2$winner_name=="Christian Harrison"&data2$tourney_name=="Delray Beach"]=280

data2$loser_rank_points[data2$loser_name=="Christian Harrison"&data2$tourney_name=="Delray Beach"]=225
data2$loser_rank[data2$loser_name=="Christian Harrison"&data2$tourney_name=="Delray Beach"]=280

data2$winner_rank_points[data2$winner_name=="Alex De Minaur"&data2$tourney_name=="Indian Wells Masters"]=1991
data2$loser_rank_points[data2$loser_name=="Alex De Minaur"&data2$tourney_name=="Indian Wells Masters"]=1991

data2$winner_rank_points[data2$winner_name=="Tommy Paul"&data2$tourney_name=="Indian Wells Masters"]=1058
data2$loser_rank_points[data2$loser_name=="Tommy Paul"&data2$tourney_name=="Indian Wells Masters"]=1058

data2$winner_rank_points[data2$winner_name=="Aslan Karatsev"&data2$tourney_name=="Indian Wells Masters"]=2109
data2$loser_rank_points[data2$loser_name=="Aslan Karatsev"&data2$tourney_name=="Indian Wells Masters"]=2109

data2$winner_rank_points[data2$winner_name=="Jannik Sinner"&data2$tourney_name=="Indian Wells Masters"]=3100
data2$loser_rank_points[data2$loser_name=="Jannik Sinner"&data2$tourney_name=="Indian Wells Masters"]=3100

data2$winner_rank_points[data2$winner_name=="Frances Tiafoe"&data2$tourney_name=="Indian Wells Masters"]=1312
data2$loser_rank_points[data2$loser_name=="Frances Tiafoe"&data2$tourney_name=="Indian Wells Masters"]=1312

data2$winner_rank_points[data2$winner_name=="Emil Ruusuvuori"&data2$tourney_name=="Indian Wells Masters"]=923
data2$loser_rank_points[data2$loser_name=="Emil Ruusuvuori"&data2$tourney_name=="Indian Wells Masters"]=923

data2$winner_rank_points[data2$winner_name=="John Millman"&data2$tourney_name=="Indian Wells Masters"]=1100
data2$loser_rank_points[data2$loser_name=="John Millman"&data2$tourney_name=="Indian Wells Masters"]=1100

data2$winner_rank_points[data2$winner_name=="Mackenzie Mcdonald"&data2$tourney_name=="Indian Wells Masters"]=1105
data2$loser_rank_points[data2$loser_name=="Mackenzie Mcdonald"&data2$tourney_name=="Indian Wells Masters"]=1105

data2$winner_rank_points[data2$winner_name=="Vasek Pospisil"&data2$tourney_name=="Indian Wells Masters"]=1000
data2$loser_rank_points[data2$loser_name=="Vasek Pospisil"&data2$tourney_name=="Indian Wells Masters"]=1000

data2$winner_rank_points[data2$winner_name=="Salvatore Caruso"&data2$tourney_name=="Indian Wells Masters"]=548
data2$loser_rank_points[data2$loser_name=="Salvatore Caruso"&data2$tourney_name=="Indian Wells Masters"]=548

data2$loser_rank_points[data2$loser_name=="Egor Gerasimov"&data2$tourney_name=="Indian Wells Masters"]=761
data2$loser_rank_points[data2$loser_name=="Adrian Mannarino"&data2$tourney_name=="Indian Wells Masters"]=1287
