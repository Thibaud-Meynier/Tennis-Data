library(readxl)
library(reshape2)
library(ggplot2)
library(quantmod)

path_22='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2022.xlsx'

data_22=read_xlsx(path=path_22)

path_21='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2021.xlsx'

data_21=read_xlsx(path=path_21)

path_20='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2020.xlsx'

data_20=read_xlsx(path=path_20)

path_19='C:/Users/Thiti/Desktop/Tennis-Data/Bet_data/2019.xlsx'

data_19=read_xlsx(path=path_19)

data=rbind(data_19,data_20,data_21,data_22)

# We delete matches with retired plauer or walkover
data=data[data$Comment=='Completed',]

data=data[,c("Tournament","Series","Surface","Winner","Loser","WRank","LRank","AvgW","AvgL")]
data$WRank=as.numeric(data$WRank)
data$LRank=as.numeric(data$LRank)
data$WRank=na.approx(data$WRank)
data$LRank=na.approx(data$LRank)
data$Rank_diff=abs(data$WRank-data$LRank)

head(data,10)

data2=data[data$Rank_diff<=5,]

#data2$AvgW[data2$AvgW==161]=1.61

data2$Fav_W=0
data2$Fav_W[data2$WRank<data2$LRank]=1

data2$Issue_match = ifelse(data2$Fav_W==1&data2$AvgW<data2$AvgL|
                             data2$Fav_W==0&data2$AvgW<data2$AvgL,"Fav Win", "Underdog Win")

table(data2$Fav_W)/nrow(data2)

table(data2$Issue_match)/nrow(data2)

# Play always the underdog

data2$odd_play=ifelse(data2$AvgW<data2$AvgL,data2$AvgL,data2$AvgW)

g=hist(data2$odd_play,breaks =50,xlim=c(1,10),ylim=c(0,350))
g$breaks
w=hist(data2$odd_play[data2$Issue_match=='Underdog Win'],breaks = 15,xlim=c(1,7),ylim = c(0,100))
w$breaks
breaks=as.data.frame(g$breaks)
counts=as.data.frame(rbind(as.data.frame(g$counts),0))


df_summary=cbind(breaks,counts)
colnames(df_summary)[1:2]=c("breaks_g",'counts_g')
df_summary=df_summary[df_summary$breaks_g<=max(w$breaks),]
df_summary$count_w=as.data.frame(rbind(as.data.frame(w$counts),0))
colnames(df_summary)[3]=c("counts_w")
df_summary$ratio=round(df_summary$counts_w/df_summary$counts_g,2)*100
df_summary$ratio_d=round(cumsum(df_summary$counts_w)/cumsum(df_summary$counts_g)*100,2)
df_summary$mean_odd=0

for (i in 1:(nrow(df_summary)-1)){
s=df_summary$breaks_g[i+1]
df_summary$mean_odd[i+1]=mean(data2[data2$odd_play>=1.8&data2$odd_play<=s,]$odd_play,na.rm=T)
print(i)

}

df_summary$odd_t=100/df_summary$ratio_d
df_summary$diff_off=df_summary$mean_odd-df_summary$odd_t

odd_s=4

t=table(data2$odd_play>=1.8&data2$odd_play<=odd_s,data2$Issue_match)
t
t[2,2]/(t[2,2]+t[2,1])

# On regarde la moyenne des cotes jou?es dans cette configuration

mean(data2$odd_play[data2$odd_play>=1.8&data2$odd_play<=odd_s],na.rm = T)
# 2.42

odd_t=100/((t[2,2]/(t[2,2]+t[2,1]))*100)
odd_t

# cote th?orique

# On commence la simulation 

data2=data2[data2$odd_play>=1.8&data2$odd_play<=odd_s,]
data2=na.omit(data2)

# tirer un num?ro de ligne
# le mettre dans un data frame (odd_play & issue_match)
# tirer 10000 fois une ligne

n=2000
k=250
data3=data.frame("Issue"=matrix(NA,nrow=n),"Odd"=matrix(NA,nrow=n))
bk=matrix(NA,nrow=k)
bk2=matrix(NA,nrow=n,ncol=k)

for (j in 1:k){
  
  for (i in 1:n){
    # on tire un num?ro de ligne du data set 50/50
    row=sample(1:nrow(data2),size=1,replace = T)
    # on ajoute l'issue du match avec la cote jou?e dans un data set
    data3[i,1]=data2$Issue_match[row]
    data3[i,2]=data2$odd_play[row]
    
  }
  # On calcul les gains cumul?s suites au n paris
  cash=cumsum(ifelse(data3$Issue=='Underdog Win',5*data3$Odd-5,-5))
  # on prend les gains finaux cumul?s
  bk[j]=cash[n]
  # on stock une BK parmis les 500
  bk2[,j]=cash
  print(j)
}

bk2=as.data.frame(bk2)

hist(bk)

table(bk<0) # 1/250

B_quantile = apply(bk2, 1, function(x) quantile(x, c(0.025,0.5,0.975)))

bk3=data.frame('Q1'=as.matrix(t(B_quantile)[,1]),
               'Med'=as.matrix(t(B_quantile)[,2]),
               'Q2'=as.matrix(t(B_quantile)[,3]),
               'BK'=bk2[,sample(c(1:k),replace=T,size=1)],
               'Mean'=apply(bk2[,1:k], 1,mean),
               'NB'=c(1:n))


# graphique d'une BK et des quantiles 2.5 et 97.5% 

ggplot(bk3,aes(x=NB))+
  geom_line(aes(y=BK))+
  geom_line(aes(y=Mean),color='purple')+
  geom_line(aes(y=Q1),color='steelblue')+
  geom_line(aes(y=Q2),color='steelblue')+
  geom_line(aes(y=0),color='red',linetype='dashed')+
  scale_y_continuous("Gains cumul?s",breaks = seq(-100,3000,250))+
  scale_x_continuous("Nombre de paris")+
  theme_classic()

Roi_end=bk3$Mean[n]/(n*5)*100
Roi_end

plot((bk3$Mean/(bk3$NB*5))*100,type='l')

# Graph with multiple BK

colnames(bk2) = paste("BK", seq(k), sep="")
rownames(bk2) = paste("Bet",seq(n),sep=" ")
bk2$Bet=rownames(bk2)
mdat = melt(bk2, id.vars="Bet")
mdat$N_bet = as.numeric(gsub("Bet"," ", mdat$Bet))

ggplot(mdat, aes(x=N_bet, y=value,color=variable)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  geom_line(size=0.3)+
  theme(legend.position = "none")+
  scale_y_continuous("Gains cumul?s",breaks = seq(-200,3000,250))+
  scale_x_continuous("Nombre de paris")+
  ggtitle("Simulation de Monte Carlo sur des matchs avec un setup type 50/50 servbot")

# Exp?rience comparative en jouant que des underdog mais ? l'aveugle

data_naive=data
data_naive$Fav_W=0
data_naive$Fav_W[data_naive$WRank<data_naive$LRank]=1

data_naive$Issue_match = ifelse(data_naive$Fav_W==1&data_naive$AvgW<data_naive$AvgL|
                                  data_naive$Fav_W==0&data_naive$AvgW<data_naive$AvgL,"Fav Win", "Underdog Win")

table(data_naive$Issue_match)/nrow(data_naive)

data_naive$odd_play=ifelse(data_naive$AvgW<data_naive$AvgL,data_naive$AvgL,data_naive$AvgW)

# Stat sur les paris retenus

t=table(data_naive$odd_play>=1.8&data_naive$odd_play<=odd_s,data_naive$Issue_match)
t
t[2,2]/(t[2,2]+t[2,1])

# On regarde la moyenne des cotes jou?es dans cette configuration

mean(data_naive$odd_play[data_naive$odd_play>1.8&data_naive$odd_play<=odd_s],na.rm = T)
# 2.42

odd_t=100/((t[2,2]/(t[2,2]+t[2,1]))*100)
odd_t

# On lance la simulation 

data_naive=data_naive[data_naive$odd_play>1.8&data_naive$odd_play<=odd_s,]
data_naive=na.omit(data_naive)

data_naive=data_naive[sample(c(1:nrow(data_naive)),size=600,replace=T),]

# tirer un num?ro de ligne
# le mettre dans un data frame (odd_play & issue_match)
# tirer 10000 fois une ligne

n=2000
k=250
data4=data.frame("Issue"=matrix(NA,nrow=n),"Odd"=matrix(NA,nrow=n))
bk=matrix(NA,nrow=k)
bk_naive=matrix(NA,nrow=n,ncol=k)

for (j in 1:k){
  
  for (i in 1:n){
    # on tire un num?ro de ligne du data set 50/50
    row=sample(1:nrow(data_naive),size=1,replace = T)
    # on ajoute l'issue du match avec la cote jou?e dans un data set
    data4[i,1]=data_naive$Issue_match[row]
    data4[i,2]=data_naive$odd_play[row]
    
  }
  # On calcul les gains cumul?s suites au n paris
  cash=cumsum(ifelse(data4$Issue=='Underdog Win',5*data4$Odd-5,-5))
  # on prend les gains finaux cumul?s
  bk[j]=cash[n]
  # on stock une BK parmis les 500
  bk_naive[,j]=cash
  print(j)
}

bk_naive=as.data.frame(bk_naive)

hist(bk)# Histogram des gains finaux sur 2k paris

B_quantile = apply(bk_naive, 1, function(x) quantile(x, c(0.025,0.5,0.975)))

bk3=data.frame('Q1'=as.matrix(t(B_quantile)[,1]),
               'Med'=as.matrix(t(B_quantile)[,2]),
               'Q2'=as.matrix(t(B_quantile)[,3]),
               'BK'=bk_naive[,sample(c(1:k),replace=T,size=1)],
               'Mean'=apply(bk_naive[,1:k], 1,mean),
               'NB'=c(1:n))


# graphique d'une BK et des quantiles 2.5 et 97.5% 

ggplot(bk3,aes(x=NB))+
  geom_line(aes(y=BK))+
  geom_line(aes(y=Mean),color='purple')+
  geom_line(aes(y=Q1),color='steelblue')+
  geom_line(aes(y=Q2),color='steelblue')+
  geom_line(aes(y=0),color='red',linetype='dashed')+
  scale_y_continuous("Gains cumul?s",breaks = seq(-100,3000,250))+
  scale_x_continuous("Nombre de paris")+
  theme_classic()

Roi_end=bk3$Mean[n]/(n*5)*100
Roi_end