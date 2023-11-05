library(readxl)
library(ggplot2)
library(mgcv)
library(pscl)
library(EnvStats)
library(car)
library(RRF)
library(nnet)
library(kknn)

path="C:/Users/Thiti/Desktop/data_tennis/data_final.xlsx"
data=read_xlsx(path = path,sheet = 3)

data=data[data$Comm=="Completed",]
data=data[data$Cat!="Exib",]

# Factorisation des var quali

data$Fav_W=as.factor(data$Fav_W)
data$Market_S=as.factor(data$Market_S)

table(data$Fav_W,data$Market_S)

str(data)

k=1500

train=sample(1:nrow(data),k)
#train=c(1:k)

mod_1<-glm(Fav_W~Class+Match+Age+Market_S+Height,data=data[train,],family=binomial(link="logit"))
summary(mod_1)
#vif(mod_1)

hitmiss(mod_1,k=0.5)

Pred=round(predict(mod_1,newdata = data[-train,],type="response"),2)

pred.moda<-factor(ifelse(Pred>0.5,"1","0"))
mc<-table(data[-train,]$Fav_W,pred.moda)
print(mc)
err.rate=1-sum(diag(mc)/sum(mc))
print(paste(round(err.rate*100,2),"%"))

# Knn

knn_1=kknn(Fav_W~Class+Match+Age+Market_S+Height,train=data[train,],test=data[-train,],k=15,kernel = "optimal")
knn_1$prob[,2]
# k=25 marche très bien, 20 encore mieux, 15 encore mieux, 10 encore mieux, 5 enorme, 

Pred=round(knn_1$prob[,2],3)

pred.moda<-factor(ifelse(Pred>0.5,"1","0"))
mc<-table(data[-train,]$Fav_W,pred.moda)
print(mc)
err.rate=1-sum(diag(mc)/sum(mc))
print(paste(round(err.rate*100,2),"%"))


##### Betting App #####

Pred2=as.data.frame(Pred)
colnames(Pred2)=c("pred")
Pred2=cbind(Pred2,data[-train,])
Pred2=Pred2[,c(1,2,6,7,8,9,10)]

Pred2$Odd_F=round(100/(Pred2$pred*106),2)
Pred2$Odd_O=round(100/((1-Pred2$pred)*106),2)
Pred2$RatioF=round((Pred2$`Fav Odd`/Pred2$Odd_F),2)
Pred2$RatioO=round((Pred2$`Out Odd`/Pred2$Odd_O),2)

bet=Pred2[Pred2$pred>=0.55|Pred2$pred<=0.45,]
bet=bet[bet$`Fav Odd`>1.35&bet$`Out Odd`<=4.3&bet$`Out Odd`>1.5&bet$`Fav Odd`<2.6,]
#bet=bet[bet$`Fav Odd`>1.35&bet$`Out Odd`<=4.3&bet$`Out Odd`>2.4&bet$`Fav Odd`<1.75,]

bet=bet[bet$RatioF>=1.05&bet$RatioF<=1.25|bet$RatioO>=1.05&bet$RatioO<=1.25,]
nrow(bet)/nrow(Pred2)

####### Test  #######
C=10
bet$Cash=-C
#bet$Cash[bet$RatioO>1]=-5
for (i in 1:nrow(bet)) {
  bet$Cash[i][bet$RatioF[i]>1&bet$Fav_W[i]==1]=C*bet$`Fav Odd`[i]-C
  bet$Cash[i][bet$RatioO[i]>1&bet$Fav_W[i]==0]=(C)*bet$`Out Odd`[i]-(C)
  
}
table(bet$Cash>0)

table(bet$Cash>0)[2]/(table(bet$Cash>0)[2]+table(bet$Cash>0)[1])*100

#BK
BK1=100

bet$BK=BK1+bet$Cash[1]

for (i in 2:nrow(bet)){
  bet$BK[i]=bet$BK[i-1]+bet$Cash[i]
}

#ROI 

bet$ROI2[1]=0
bet$M2=C
bet$M_cum2=bet$M2
for (i in 2:nrow(bet)){
  bet$M_cum2[i]=bet$M2[i]+bet$M_cum2[i-1]
}


bet$ROI2=(bet$BK-100)/bet$M_cum2

par(mfrow=c(1,2))
plot(bet$BK,type='l',ylim=c(min(bet$BK),max(bet$BK)))
abline(100,0,lty=2,col='red')

plot(bet$ROI2,type='l',col='black')
abline(0,0,lty=2,col='red')

bet$Odd_Moyenne=0

for (i in 1:nrow(bet)){
  bet$Odd_Moyenne[i][bet$RatioF[i]>1]=bet$`Fav Odd`[i]
  bet$Odd_Moyenne[i][bet$RatioO[i]>1]=bet$`Out Odd`[i]
}

round(mean(bet$Odd_Moyenne),2)
