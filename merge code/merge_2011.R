library(readxl)
library(reshape2)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(sqldf)
library(sjmisc)

#Annee 2011

##### IMPORT DES DONNEES #####

path_11='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2011.xlsx'

data_11=read_xlsx(path=path_11)

year=2011

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

x=unique(data_11$Tournament) %>% data.frame()
z=unique(data$tourney_name) %>% data.frame()

z=sqldf("select distinct tourney_id,tourney_name,tourney_date,surface
        from data
        where tourney_name not like 'Davis Cup%'")

z$tourney_date=as.Date(paste0(substr(z$tourney_date,1,4),"-",substr(z$tourney_date,5,6),"-",substr(z$tourney_date,7,8)))

x=data_11 %>% 
  group_by(Tournament,Location,Surface,Series) %>% 
  summarise(Date=min(Date)) %>% 
  arrange(Date)

z$tourney_name2=str_remove(z$tourney_name," Masters| 1| 2")
#x$Location=str_remove(x$Location," ")

x$Date=as.Date(x$Date)

patern2=c('Davis Cup','London Olympics','Laver Cup','Atp Cup','United Cup','NextGen Finals')

z=z %>% filter(!tourney_name %in% patern2)

z$Week=paste0(year(z$tourney_date),"-",week(z$tourney_date))
x$Week=paste0(year(x$Date),"-",week(x$Date))

# z$Day=wday(z$tourney_date,label=T)
# x$Day=wday(x$Date,label=T)

## Redressement des données de tournois

#Napoli, Nur-Sultan, Belgrade Us Open, Queens, Hertogenbosch, Canada

x$Location[x$Location=="'s-Hertogenbosch"]="Hertogenbosch"
#x$Location[x$Location=="Queens Club"]="Queen's Club"
#x$Week[x$Location=="Los Cabos"]="2019-30"
#x$Location[x$Location=="St. Petersburg"]="St Petersburg"
x$Series[x$Location=="Hamburg"]="ATP500"
#x$Series[x$Location=="Nottingham"]="ATP250"
x$Tournament[x$Tournament=="French Open"]="Roland Garros"
x$Surface[x$Location=="Bogota"]="Hard"
z$tourney_name2[z$tourney_name2=="Us Open"]="US Open"
z$tourney_name2[z$tourney_name2=="Canada"]="Montreal"
z$tourney_name2[z$tourney_name2=="s Hertogenbosch"]="Hertogenbosch"
z$tourney_name2[z$tourney_name=="Tour Finals"]="London"
#z$tourney_name[z$tourney_name=="Estoril"]="Oeiras"
#z$tourney_name2[z$tourney_name2=="Estoril"]="Oeiras"
z$tourney_name[z$tourney_name=="Queen's Club"]="Queens Club"
z$tourney_name2[z$tourney_name2=="Queen's Club"]="Queens Club"
# z$tourney_name[z$tourney_name=="Santiago"]="Vina del Mar"
# z$tourney_name2[z$tourney_name2=="Santiago"]="Vina del Mar"
#redressement des numéros de semaine

##Table de pivot des tournois

Tournament_pivot=sqldf("select a.*,b.tourney_name
      from x a
      left join z b on (b.tourney_name2=a.location and b.Week<=a.Week and b.Surface=a.Surface) or (b.tourney_name2=a.Tournament and b.Week<=a.Week and b.Surface=a.Surface)
                       /*where (substr(a.Week,6,2)-substr(b.Week,6,2)) in (-1,0,52)*/")

data2=data %>% select(tourney_date,tourney_name,match_num,winner_name,loser_name,round,winner_rank,loser_rank,winner_rank_points,loser_rank_points,winner_hand,loser_hand,winner_ht,loser_ht,winner_id,loser_id,winner_age,loser_age,winner_ioc,loser_ioc)

#On importe les typologies de tournoi

# redressement du nom d'un tournpi
#data2$tourney_name[data2$tourney_name=="Santiago"]="Vina del Mar"
#data2$tourney_name[data2$tourney_name=="Estoril"]="Oeiras"
data2$tourney_name[data2$tourney_name=="Queen's Club"]="Queens Club"

data2=sqldf("select a.*,c.Series
            from data2 a
            left join Tournament_pivot c on c.tourney_name=a.tourney_name")

Round_pivot=data.frame(Round_data2=unique(data_11$Round),
                       Round_data=c("R128","R64","QF","SF","F","R32","R16","RR"),
                       Round_H=c("R1","R2","QF","SF","F","R3","R4","RR")) %>% data.frame()

sqldf("select distinct round,tourney_name from data2 where series='ATP500' and round='R64'")

sqldf("select distinct round,tourney_name from data2 where series='ATP250' and round='R64'")

sqldf("select distinct round,tourney_name from data2 where series='Masters 1000' and round='R128'")

# Redressement du tour dans la base pour joindre avec la base avec les cotes

data2=sqldf("select *,(case 
when Series='ATP250' and tourney_name not in ('Winston-Salem','Queens Club') and round='R32' then 'R128' 
when Series='ATP250' and tourney_name not in ('Winston-Salem','Queens Club') and round='R16' then 'R64'

when Series='ATP250' and tourney_name in ('Winston-Salem','Queens Club') and round='R64' then 'R128'
when Series='ATP250' and tourney_name in ('Winston-Salem','Queens Club') and round='R32' then 'R64'
when Series='ATP250' and tourney_name in ('Winston-Salem','Queens Club') and round='R16' then 'R32'

when Series='ATP500' and tourney_name not in ('Barcelona','Hamburg','Washington') and round='R32' then 'R128' 
when Series='ATP500' and tourney_name not in ('Barcelona','Hamburg','Washington') and round='R16' then 'R64'

when Series='ATP500' and tourney_name in ('Barcelona','Hamburg','Washington') and round='R64' then 'R128'
when Series='ATP500' and tourney_name in ('Barcelona','Hamburg','Washington') and round='R32' then 'R64'
when Series='ATP500' and tourney_name in ('Barcelona','Hamburg','Washington') and round='R16' then 'R32'

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

data_11$Tournament[data_11$Tournament=="French Open"]="Roland Garros"
data_11$Series[data_11$Location=="Hamburg"]="ATP500"
#data_11$Series[data_11$Location=="Nottingham"]="ATP250"
data_11$Surface[data_11$Location=="Bogota"]="Hard"

data_11=data_11 %>% left_join(Round_pivot %>% select(Round_data2,Round_H),by=c("Round"="Round_data2")) %>% 
  left_join(Tournament_pivot %>% select(Tournament,Series,tourney_name),by=c("Tournament"="Tournament","Series"="Series"))

# Redressement sur le classement manquant de certains joueurs

data_11$WRank=ifelse(data_11$WRank=="N/A",1000,data_11$WRank)
data_11$WPts=ifelse(data_11$WPts=="N/A",10,data_11$WPts)
data_11$LRank=ifelse(data_11$LRank=="N/A",1000,data_11$LRank)
data_11$LPts=ifelse(data_11$LPts=="N/A",10,data_11$LPts)

data2$winner_rank=ifelse(is.na(data2$winner_rank)==T,1000,data2$winner_rank)
data2$winner_rank_points=ifelse(is.na(data2$winner_rank_points)==T,10,data2$winner_rank_points)
data2$loser_rank=ifelse(is.na(data2$loser_rank)==T,1000,data2$loser_rank)
data2$loser_rank_points=ifelse(is.na(data2$loser_rank_points)==T,10,data2$loser_rank_points)

#Redressement de certaines valeurs différentes d'une table à l'autre
# # 
# data2$loser_rank_points[data2$loser_name=="Reilly Opelka"&data2$tourney_name=="Houston"]=432
# data2$loser_rank[data2$loser_name=="Reilly Opelka"&data2$tourney_name=="Houston"]=134
# 
# data2$loser_rank_points[data2$loser_name=="Reilly Opelka"&data2$tourney_name=="Washington"]=363
# data2$loser_rank[data2$loser_name=="Reilly Opelka"&data2$tourney_name=="Washington"]=155

# Merging entre les 2 bases

merge=sqldf("select distinct a.*,b.date,b.W1,b.L1,b.W2,b.L2,b.W3,b.L3,b.W4,b.L4,b.W5,b.L5,b.Wsets,b.Lsets,
      b.PSW,b.PSL,b.AvgW,b.AvgL,b.comment
      from data2 a 
      left join data_11 b on B.Round_H=a.Round_H and b.WPts=a.winner_rank_points and b.LPts=a.loser_rank_points and b.tourney_name=a.tourney_name")

# Test de correspondance de la jointure 

NA_df=merge %>% filter(is.na(merge$AvgW)==TRUE)

Summary=NA_df %>% group_by(tourney_name) %>% summarise("NB"=n()) %>% filter(str_starts(tourney_name,"Davis Cup|Atp Cup|Laver Cup|NextGen Finals|Tokyo Olympics")==FALSE)

Summary2=data_11 %>% group_by(Tournament) %>% summarise("NB"=sum(is.na(AvgW))) %>% filter(NB>0)

list_na=merge %>% filter(tourney_name %in% unique(Summary$tourney_name) & is.na(Date)==T & tourney_name!='Dusseldorf')

# 
List=sqldf("select *
from (select distinct winner_id as id,winner_name as name
           from merge where winner_ht is null or winner_ht='') as table1
union
select * 
from (select distinct winner_id as id,winner_name as name
           from merge where winner_age is null or winner_age='') as table2
union
select *
from (select distinct winner_id as id,winner_name as name
       from merge where winner_hand is null or winner_hand='U') as table3
union
select * 
from (select distinct loser_id as id,loser_name as name
           from merge where loser_ht is null or loser_ht='') as table4
union
select * 
from (select distinct loser_id as id,loser_name as name
           from merge where loser_age is null or loser_age='') as table5
union
select *
from (select distinct loser_id as id,loser_name as name
           from merge where loser_hand is null or loser_hand='U') as table6")