library(readxl)
library(ggplot2)
path="C:/Users/Thiti/Desktop/data_tennis/Fichier 2021.xlsx"
data=read_xlsx(path = path,sheet = 1)
data2=read_xlsx(path = path,sheet = 2)
data3=read_xlsx(path = path,sheet = 3)

data=data[,-c(6,9,11,15,17,19)]
data2=data2[,c(1,2,3,5,12,18,19,20)]
data3=data3[,c(1,2,3,5,11,18,19)]

data$index=c(1:2598)
data$winner_n_match=0
data$loser_n_match=0

data2$winner_rank_points=as.numeric(data2$winner_rank_points)
data2$loser_rank_points=as.numeric(data2$loser_rank_points)
data2$minutes=as.numeric(data2$minutes)

data2[is.na(data2$winner_rank_points),]$winner_rank_points=sample(10:150,1)
data2[is.na(data2$loser_rank_points),]$loser_rank_points=sample(10:150,1)
data2[is.na(data2$minutes),]$minutes=sample(60:120,1)

data3[is.na(data3$winner_rank_points),]$winner_rank_points=sample(10:150,1)
data3[is.na(data3$loser_rank_points),]$loser_rank_points=sample(10:150,1)

data_t=data[data$Cat!="Exib",]
data_t=data_t[data_t$Comm=="Completed",]

table(data_t$Fav_W,data_t$Cat)
table(data_t$Fav_W)
1582/(1582+919)

mydata=cbind(data$winner_rank_points,data$loser_rank_points)
mydata=matrix(nrow = 2*nrow(data),NA)
mydata=as.data.frame(mydata)
mydata$V1[1:2598]=data$winner_rank_points
mydata$V1[2599:5196]=data$loser_rank_points
mydata=unique(mydata[,1])
# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)


S_1=415
s_2=958
S_3=1715
S_4=2932
S_5=6691

# On enlève les abandons et Walkover et on garde les matchs avec une durée de plus de 45 minutes
data=data[data$minutes>45,]
n=nrow(data)

#Variable n_match
for (i in 1:n) {
  s=data$Semaine[i]
  # Data frame de tout les matchs sur le mois précédent le match du joueur i et j
  
  #ATP
  data_s=data[data$Semaine<=s&data$Semaine>=(s-4),]
  index=data$index[i]
  data_s=data_s[data_s$index<index,]
  # Qualif ATP
  data_q=data2[data2$Semaine<=s&data2$Semaine>=(s-4),]
  # Cicruit Challenger
  data_c=data3[data3$Semaine<=s&data3$Semaine>=(s-4),]
  
  # Recupération du nom du joueur i
  W_N=data[data$index==index,]$winner_name
  # Calcul du nombre d'apparition du joueur (en tant que W ou L sur le mois précédent le match j)
  data$winner_n_match[i]=sum(data_s$winner_name==W_N)+sum(data_s$loser_name==W_N)+
    sum(data_q$winner_name==W_N)+sum(data_q$loser_name==W_N)+
    sum(data_c$winner_name==W_N)+sum(data_c$loser_name==W_N)
  
  # Recupération du nom du joueur j
  L_N=data[data$index==index,]$loser_name
  # Calcul du nombre d'apparition du joueur (en tant que W ou L sur le mois précédent le match j)
  data$loser_n_match[i]=sum(data_s$winner_name==L_N)+sum(data_s$loser_name==L_N)+
    sum(data_q$winner_name==L_N)+sum(data_q$loser_name==L_N)+
    sum(data_c$winner_name==L_N)+sum(data_c$loser_name==L_N)
  
  print(i)
}


mydata=matrix(nrow = 2*nrow(data),NA)
mydata=as.data.frame(mydata)
mydata$V1[1:2560]=data$winner_n_match
mydata$V1[2561:5120]=data$loser_n_match
mydata=unique(mydata[,1])
# K-Means Cluster Analysis
fit <- kmeans(mydata, 4) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
table(mydata$V1)

# variable match précédent en bo3
data$winner_pm3=0
data$loser_pm3=0

# Variable match > 2h30
for (i in 1:n) {
#Tounoi en cours
t=data$tourney_name[i]
index=data$index[i]
data_s=data[data$tourney_name==t,]

# Joueur i
W_N=data[data$index==index,]$winner_name
# on récup tout le matchs du joueur i pour le tournoi en cours
data_s=data_s[data_s$winner_name==W_N|data_s$loser_name==W_N,]
row=which(data_s$index==index)
# on récup le match précédent
data_s=data_s[row-1,]
data$winner_pm3[i][data_s$minutes>=150]=1

#Joueur j
data_s=data[data$tourney_name==t,]
L_N=data[data$index==index,]$loser_name
# on récup tout le matchs du joueur i pour le tournoi en cours
data_s=data_s[data_s$winner_name==L_N|data_s$loser_name==L_N,]
row=which(data_s$index==index)
# on récup le match précédent
data_s=data_s[row-1,]
data$loser_pm3[i][data_s$minutes>=150]=1

print(i)
}

# Variable match > 3h30
data$winner_pm5=0
data$loser_pm5=0

for (i in 1:n) {
  #Tounoi en cours
  t=data$tourney_name[i]
  index=data$index[i]
  data_s=data[data$tourney_name==t,]
  
  # Joueur i
  W_N=data[data$index==index,]$winner_name
  # on récup tout le matchs du joueur i pour le tournoi en cours
  data_s=data_s[data_s$winner_name==W_N|data_s$loser_name==W_N,]
  row=which(data_s$index==index)
  # on récup le match précédent
  data_s=data_s[row-1,]
  data$winner_pm5[i][data_s$minutes>=210]=1
  
  #Joueur j
  data_s=data[data$tourney_name==t,]
  L_N=data[data$index==index,]$loser_name
  # on récup tout le matchs du joueur i pour le tournoi en cours
  data_s=data_s[data_s$winner_name==L_N|data_s$loser_name==L_N,]
  row=which(data_s$index==index)
  # on récup le match précédent
  data_s=data_s[row-1,]
  data$loser_pm5[i][data_s$minutes>=210]=1
  
  print(i)
}

# Variable de perf (nbr victoires)
data$winner_perf=0
data$loser_perf=0

for (i in 1:n){
# arriver à un data set ou il n'y a que les victoires du joueur i, on compare les points pour voir si perf
s=data$Semaine[i]
# Data frame de tout les matchs sur le mois précédent le match du joueur i et j

# Perf joueur W
data_s=data[data$Semaine<=s&data$Semaine>=(s-4),]
index=data$index[i]
data_s=data_s[data_s$index<index,]
# Qualif ATP
data_q=data2[data2$Semaine<=s&data2$Semaine>=(s-4),]
# Cicruit Challenger
data_c=data3[data3$Semaine<=s&data3$Semaine>=(s-4),]
# Recupération du nom du joueur i
W_N=data[data$index==index,]$winner_name

# Data set avec victoires
data_s=data_s[data_s$winner_name==W_N,]
data_q=data_q[data_q$winner_name==W_N,]
data_c=data_c[data_c$winner_name==W_N,]

#Nbr Perf
data$winner_perf[i]=sum(data_s$winner_name==W_N)+
  sum(data_q$winner_name==W_N)+
  sum(data_c$winner_name==W_N)
  
# Perf joueur L

data_s=data[data$Semaine<=s&data$Semaine>=(s-4),]
index=data$index[i]
data_s=data_s[data_s$index<index,]
# Qualif ATP
data_q=data2[data2$Semaine<=s&data2$Semaine>=(s-4),]
# Cicruit Challenger
data_c=data3[data3$Semaine<=s&data3$Semaine>=(s-4),]
# Recupération du nom du joueur i
W_L=data[data$index==index,]$loser_name

# Data set avec victoires
data_s=data_s[data_s$winner_name==W_L,]
data_q=data_q[data_q$winner_name==W_L,]
data_c=data_c[data_c$winner_name==W_L,]

#Nbr Perf
data$loser_perf[i]=sum(data_s$winner_name==W_L)+
  sum(data_q$winner_name==W_L)+
  sum(data_c$winner_name==W_L)

print(i)
}

# Variable de contre perf
data$winner_c_perf=0
data$loser_c_perf=0

for (i in 1:n){
  # arriver à un data set ou il n'y a que les victoires du joueur i, on compare les points pour voir si perf

  s=data$Semaine[i]
  # Data frame de tout les matchs sur le mois précédent le match du joueur i et j
  
  # Perf joueur W
  data_s=data[data$Semaine<=s&data$Semaine>=(s-4),]
  index=data$index[i]
  data_s=data_s[data_s$index<index,]
  # Qualif ATP
  data_q=data2[data2$Semaine<=s&data2$Semaine>=(s-4),]
  # Cicruit Challenger
  data_c=data3[data3$Semaine<=s&data3$Semaine>=(s-4),]
  # Recupération du nom du joueur i
  W_N=data[data$index==index,]$winner_name
  
  # Data set avec défaites
  data_s=data_s[data_s$loser_name==W_N,]
  data_q=data_q[data_q$loser_name==W_N,]
  data_c=data_c[data_c$loser_name==W_N,]
  
  #Nbr Perf
  data$winner_c_perf[i]=sum(data_s$winner_rank_points<data_s$loser_rank_points)+
    sum(data_q$winner_rank_points<data_q$loser_rank_points)+
    sum(data_c$winner_rank_points<data_c$loser_rank_points)
  
  # Perf joueur L
  
  data_s=data[data$Semaine<=s&data$Semaine>=(s-4),]
  index=data$index[i]
  data_s=data_s[data_s$index<index,]
  # Qualif ATP
  data_q=data2[data2$Semaine<=s&data2$Semaine>=(s-4),]
  # Cicruit Challenger
  data_c=data3[data3$Semaine<=s&data3$Semaine>=(s-4),]
  # Recupération du nom du joueur i
  W_L=data[data$index==index,]$loser_name
  
  # Data set avec victoires
  data_s=data_s[data_s$loser_name==W_L,]
  data_q=data_q[data_q$loser_name==W_L,]
  data_c=data_c[data_c$loser_name==W_L,]
  
  #Nbr Perf
  data$loser_c_perf[i]=sum(data_s$winner_rank_points<data_s$loser_rank_points)+
    sum(data_q$winner_rank_points<data_q$loser_rank_points)+
    sum(data_c$winner_rank_points<data_c$loser_rank_points)
  
  print(i)
}

# Temps de jeu cumulé tournoi
data$winner_tp_cum=0
data$loser_tp_cum=0

for (i in 1:n) {
t=data$tourney_name[i]
index=data$index[i]
data_s=data[data$tourney_name==t,]
data_q=data2[data2$tourney_name==t,]
# Joueur i
W_N=data[data$index==index,]$winner_name
# on récup tout le matchs du joueur i pour le tournoi en cours
data_s=data_s[data_s$winner_name==W_N|data_s$loser_name==W_N,]
data_s=data_s[data_s$index<index,]
data_q=data_q[data_q$winner_name==W_N|data_q$loser_name==W_N,]
#temps de jeux cumulé du tournoi (qualif inclusent)
data$winner_tp_cum[i]=sum(data_s$minutes)+sum(data_q$minutes)

# Joueur j
data_s=data[data$tourney_name==t,]
data_q=data2[data2$tourney_name==t,]

W_L=data[data$index==index,]$loser_name
# on récup tout le matchs du joueur i pour le tournoi en cours
data_s=data_s[data_s$winner_name==W_L|data_s$loser_name==W_L,]
data_s=data_s[data_s$index<index,]
data_q=data_q[data_q$winner_name==W_L|data_q$loser_name==W_L,]
#temps de jeux cumulé du tournoi joueur i
data$loser_tp_cum[i]=sum(data_s$minutes)+sum(data_q$minutes)

print(i)
}

# Variable joueur joue domicile
data$winner_dom=0
data$loser_dom=0

for (i in 1:n){
  Loc=data$Loc[i]
  data$winner_dom[i][data$winner_ioc[i]==Loc]=1
  data$loser_dom[i][data$loser_ioc[i]==Loc]=1
  print(i)
}

write.table(data,"data_final.csv",sep=";",row.names = F)

