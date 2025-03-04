library(readxl)
library(reshape2)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(sqldf)
library(sjmisc)

#Annee 2022

##### IMPORT DES DONNEES #####

path_22='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2022.xlsx'

data_22=read_xlsx(path=path_22)

year=2022

url="https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches"

link=paste(url,'_',year,'.csv',sep='')

data=read.csv2(link,sep=',')

player=read.csv2("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv",sep=",")

player$dob=as.Date(paste0(substr(player$dob,1,4),"-",
                          substr(player$dob,5,6),"-",
                          substr(player$dob,7,8)))

player=player %>% filter(year(dob)>=1970)

##### NORMALISATION DES TOURNOIS DANS LES 2 BASES #####

x=unique(data_22$Tournament) %>% data.frame()
z=unique(data$tourney_name) %>% data.frame()

z=sqldf("select distinct tourney_id,tourney_name,tourney_date,surface
        from data
        where tourney_name not like 'Davis Cup%'")

z$tourney_date=as.Date(paste0(substr(z$tourney_date,1,4),"-",substr(z$tourney_date,5,6),"-",substr(z$tourney_date,7,8)))

x=data_22 %>% 
  group_by(Tournament,Location,Surface,Series) %>% 
  summarise(Date=min(Date)) %>% 
  arrange(Date)

z$tourney_name2=str_remove(z$tourney_name," Masters| 1| 2")
#x$Location=str_remove(x$Location," ")

x$Date=as.Date(x$Date)

patern2=c('Davis Cup','Olympics','Laver Cup','Atp Cup','NextGen Finals')

z=z %>% filter(!tourney_name %in% patern2)

z$Week=paste0(year(z$tourney_date),"-",week(z$tourney_date))
x$Week=paste0(year(x$Date),"-",week(x$Date))

## Redressement des données de tournois

#Napoli, Nur-Sultan, Belgrade Us Open, Queens, Hertogenbosch, Canada

x$Location[x$Location=="Nur-Sultan"]="Astana"
x$Location[x$Location=="Napoli"]="Naples"
x$Location[x$Location=="'s-Hertogenbosch"]="Hertogenbosch"
x$Location[x$Location=="Queens Club"]="Queen's Club"
x$Series[x$Location=="Hamburg"]="ATP500"
x$Tournament[x$Tournament=="French Open"]="Roland Garros"
z$tourney_name2[z$tourney_name2=="Us Open"]="US Open"
z$tourney_name2[z$tourney_name2=="Belgrade "]="Belgrade"
z$tourney_name2[z$tourney_name2=="Canada"]="Montreal"
z$tourney_name2[z$tourney_name2=="s Hertogenbosch"]="Hertogenbosch"
z$tourney_name2[z$tourney_name=="Tour Finals"]="Turin"

##Table de pivot des tournois

Tournament_pivot=sqldf("select distinct a.*,b.tourney_name
      from x a
      left join z b on (b.tourney_name2=a.location and b.Week=a.Week) or (b.tourney_name2=a.Tournament and b.Week=a.Week)")

##### MERGE ENTRE LES 2 BASES #####

## on reprend la base avec les infos des joueurs 

data2=data %>% select(tourney_date,tourney_name,match_num,winner_name,loser_name,round,winner_rank,loser_rank,winner_rank_points,loser_rank_points,winner_hand,loser_hand,winner_ht,loser_ht,winner_id,loser_id,winner_age,loser_age,winner_ioc,loser_ioc)

# Redressement sur certains joueurs

data2$winner_rank_points[data2$winner_name=="Andres Martin"&data2$tourney_name=="Atlanta" & data2$round=="R32"]=260
data2$winner_rank[data2$winner_name=="Andres Martin"&data2$tourney_name=="Atlanta" & data2$round=="R32"]=203

data2$loser_rank_points[data2$loser_name=="Andres Martin"&data2$tourney_name=="Atlanta" & data2$round=="R16"]=260
data2$loser_rank[data2$loser_name=="Andres Martin"&data2$tourney_name=="Atlanta" & data2$round=="R16"]=203

data2$loser_rank_points[data2$loser_name=="Martin Landaluce"&data2$tourney_name=="Gijon" & data2$round=="R32"]=10
data2$loser_rank[data2$loser_name=="Martin Landaluce"&data2$tourney_name=="Gijon" & data2$round=="R32"]=1000

data2$loser_rank_points[data2$loser_name=="Caleb Chakravarthi"&data2$tourney_name=="Dallas" & data2$round=="R32"]=10
data2$loser_rank[data2$loser_name=="Caleb Chakravarthi"&data2$tourney_name=="Dallas" & data2$round=="R32"]=1000

#On importe les typologies de tournoi

data2=sqldf("select a.*,c.Series
            from data2 a
            left join Tournament_pivot c on c.tourney_name=a.tourney_name")

Round_pivot=data.frame(Round_data2=unique(data_22$Round),
                       Round_data=c("R128","R64","QF","SF","F","R32","R16","RR"),
                       Round_H=c("R1","R2","QF","SF","F","R3","R4","RR")) %>% data.frame()


#Round_pivot$Order=c(1,2,5,6,7,3,4,1)
#                     
# data_22b=sqldf("select *,(case 
# when Series='ATP250' and round='1st Round' then '2nd Round'
# when Series='ATP250' and round='2nd Round' then '3rd Round'
# when Series='ATP250' and round='3rd Round' then '4th Round'
# 
# when Series='ATP500' and round='1st Round' then '2nd Round'
# when Series='ATP500' and round='2nd Round' then '3rd Round'
# when Series='ATP500' and round='3rd Round' then '4th Round'
# 
# when Series='Masters 1000' and location not in ('Miami Masters','Indian Wells') and round='1st Round' then '2nd Round'
# when Series='Masters 1000' and location not in ('Miami','Indian Wells') and round='2nd Round' then '3rd Round'
# when Series='Masters 1000' and location not in ('Miami','Indian Wells') and round='3rd Round' then '4th Round'
# else Round end) as Round_red 
# from data_22") 

# Redressement du tour dans la base pour joindre avec la base avec les cotes

data2=sqldf("select *,(case 
when Series='ATP250' and tourney_name not in ('Winston-Salem') and round='R32' then 'R128' 
when Series='ATP250' and tourney_name not in ('Winston-Salem') and round='R16' then 'R64'

when Series='ATP250' and tourney_name='Winston-Salem' and round='R64' then 'R128'
when Series='ATP250' and tourney_name='Winston-Salem' and round='R32' then 'R64'
when Series='ATP250' and tourney_name='Winston-Salem' and round='R16' then 'R32'

when Series='ATP500' and tourney_name not in ('Barcelona','Washington') and round='R32' then 'R128' 
when Series='ATP500' and tourney_name not in ('Barcelona','Washington') and round='R16' then 'R64'

when Series='ATP500' and tourney_name in ('Barcelona','Washington') and round='R64' then 'R128'
when Series='ATP500' and tourney_name in ('Barcelona','Washington') and round='R32' then 'R64'
when Series='ATP500' and tourney_name in ('Barcelona','Washington') and round='R16' then 'R32'

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

data_22$Tournament[data_22$Tournament=="French Open"]="Roland Garros"
data_22$Series[data_22$Location=="Hamburg"]="ATP500"

data_22=data_22 %>% left_join(Round_pivot %>% select(Round_data2,Round_H),by=c("Round"="Round_data2")) %>% 
                    left_join(Tournament_pivot %>% select(Tournament,Series,tourney_name),by=c("Tournament"="Tournament","Series"="Series"))


# data_22=sqldf("select distinct a.*,b.Round_H,c.tourney_name
#             from data_22 a
#             left join Round_pivot b on b.round_data2=a.round
#             left join Tournament_pivot c on c.tournament=a.tournament and c.series=a.series") %>% select(!Date)

# Merging entre les 2 bases

merge=sqldf("select distinct a.*,b.date,b.W1,b.L1,b.W2,b.L2,b.W3,b.L3,b.W4,b.L4,b.W5,b.L5,b.Wsets,b.Lsets,
      b.PSW,b.PSL,b.AvgW,b.AvgL,b.comment
      from data2 a 
      left join data_22 b on B.Round_H=a.Round_H and b.WPts=a.winner_rank_points and b.LPts=a.loser_rank_points and b.tourney_name=a.tourney_name")

# Test de correspondance de la jointure 

NA_df=merge %>% filter(is.na(merge$AvgW)==TRUE)

Summary=NA_df %>% group_by(tourney_name) %>% summarise("NB"=n()) %>% filter(str_starts(Summary$tourney_name,"Davis Cup|Atp Cup|Laver Cup|NextGen Finals")==FALSE)

Summary2=data_22 %>% group_by(Tournament) %>% summarise("NB"=sum(is.na(AvgW))) %>% filter(NB>0)

# Liste de joueurs avec manque d'info

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

# new_df <- filter(merge, grepl(paste(patern2, collapse='|'), merge$tourney_name),.preserve=TRUE)
# 
# merge$test=grepl(paste(patern2, collapse='|'), merge$tourney_name)
# 
# merge$test2=merge$tourney_name %in% new_df$tourney_name # is in
# 
# # clean data
# 
# merge=merge[merge$test==FALSE&merge$test2==FALSE,]
# 
# 
# 
# write.table(z,file="tourney_name.csv",row.names = F,sep=";")
# write.table(x,file="tournament_name.csv",row.names = F,sep=";")

#Laver Cup, David Cup new gen finals, atp_cup

## Creer une variable id_match pour naviguer entre les 2 bases et verifier les données manquantes suite au merge

#id_winner,id_loser,round,tournament (3premlettre),series
