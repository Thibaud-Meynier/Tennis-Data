library(readxl)
library(ggplot2)
library(reshape2)

path='C:/Users/Thiti/Desktop/data_tennis/data_final.xlsx'

data=read_xlsx(path=path,sheet=2)

data2=data[data$`Fav Ht`>=190&data$`Out Ht`>=190,] # match type 'servbot mirror'

# On creer un champ pour rendre compte de la cote qui passe

data2$Issue_match = ifelse(data2$Fav_W==1&data2$`Fav Odd`<data2$`Out Odd`|
                data2$Fav_W==0&data2$`Fav Odd`>data2$`Out Odd`,"Fav Win", "Underdog Win")

table(data2$Issue_match)/nrow(data2)

data2=data2[c(1:9,13,14,39)]

# L'expérience est la suivante, dans un match de servebot qu'on juge du type 50/50, on joue tjr la cote la plus elevé des 2 (donc underdog à chaque fois)

data2$odd_play=ifelse(data2$`Fav Odd`<data2$`Out Odd`,data2$`Out Odd`,data2$`Fav Odd`)

table(data2$odd_play>=5,data2$Issue_match)
#Dans l'échantillon, passent 14% du temps 

hist(data2$odd_play,breaks = 15,xlim=c(0,12),ylim=c(0,120))
hist(data2$odd_play[data2$Issue_match=='Underdog Win'],breaks = 10,xlim=c(0,12),ylim = c(0,50))

odd_s=2.7

t=table(data2$odd_play<=odd_s,data2$Issue_match)
t
t[2,2]/(t[2,2]+t[2,1])
# 46%

mean(data2$odd_play[data2$odd_play<=odd_s],na.rm = T)

100/((t[2,2]/(t[2,2]+t[2,1]))*100)

data2=data2[data2$odd_play<=odd_s,]
data2=na.omit(data2)

# tirer un numéro de ligne
# le mettre dans un data frame (odd_play & issue_match)
# tirer 10000 fois une ligne

n=5000
k=100
data3=data.frame("Issue"=matrix(NA,nrow=n),"Odd"=matrix(NA,nrow=n))
bk=matrix(NA,nrow=k)
bk2=matrix(NA,nrow=n,ncol=k)

for (j in 1:k){
  
for (i in 1:n){
  # on tire un numéro de ligne du data set 50/50
  row=sample(1:nrow(data2),size=1,replace = T)
  # on ajoute l'issue du match avec la cote jouée dans un data set
  data3[i,1]=data2$Issue_match[row]
  data3[i,2]=data2$odd_play[row]
  
}
  # On calcul les gains cumulés suites au n paris
  cash=cumsum(ifelse(data3$Issue=='Underdog Win',5*data3$Odd-5,-5))
  # on prend les gains finaux cumulés
  bk[j]=cash[n]
  # on stock une BK parmis les 500
  bk2[,j]=cash
  print(j)
}

bk2=as.data.frame(bk2)

hist(bk)

B_quantile = apply(bk2, 1, function(x) quantile(x, c(0.025, 0.975)))

bk3=data.frame('Q1'=as.matrix(t(B_quantile)[,1]),
               'Q2'=as.matrix(t(B_quantile)[,2]),
               'BK'=bk2[,sample(c(1:100),replace=T,size=1)],
               'NB'=c(1:5000))


# graphique d'une BK et des quantiles 2.5 et 97.5% 

ggplot(bk3,aes(x=NB))+
  geom_line(aes(y=BK))+
  geom_line(aes(y=Q1),color='steelblue')+
  geom_line(aes(y=Q2),color='steelblue')+
  scale_y_continuous("Gains cumulés",breaks = seq(-100,3000,250))+
  scale_x_continuous("Nombre de paris")+
  theme_classic()

bk2=bk2[,1:500] 

# Graph with multiple BK

colnames(bk2) = paste("BK", seq(k), sep="")
rownames(bk2) = paste("Bet",seq(n),sep=" ")
bk2$Bet=rownames(bk2)
mdat = melt(bk2, id.vars="Bet")
mdat$N_bet = as.numeric(gsub("Bet"," ", mdat$Bet))

library(RColorBrewer)

ggplot(mdat, aes(x=N_bet, y=value,color=variable)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  geom_line(size=0.3)+
  theme(legend.position = "none")+
  scale_y_continuous("Gains cumulés",breaks = seq(-200,3000,250))+
  scale_x_continuous("Nombre de paris")+
  ggtitle("Simulation de Monte Carlo sur des matchs avec un setup type 50/50 servbot")

  
