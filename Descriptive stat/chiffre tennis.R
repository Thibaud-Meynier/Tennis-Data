data=read.csv2("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2021.csv",sep=",")

data2=read.csv2("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2021.csv",sep=",")
# ATP qualif 1286

# 2,6,10,11,12,13,14,15,18,19,20,21,22,23,27,47,49

data_atp=data[,c(2,6,10,11,12,13,14,15,18,19,20,21,22,23,27,47,49)]

data_atp2=data2[,c(2,6,10,11,12,13,14,15,18,19,20,21,22,23,27,47,49)][c(1:1286),]

data_chall=data2[,c(2,6,10,11,12,13,14,15,18,19,20,21,22,23,27,47,49)][c(1287:7112),]

write.table(data,"data_atp.csv",sep=";",row.names = F)
write.table(data_atp2,"data_atp2.csv",sep=";",row.names = F)
write.table(data_chall,"data_chall.csv",sep=";",row.names = F)

setwd("C:/Users/Thiti/Desktop/data_tennis/ATP matches")

library(readxl)

path="C:/Users/Thiti/Desktop/data_tennis/data_atp_brut.xlsx"
data=read_xlsx(path = path,sheet=1)

data2=data[data$minutes>40,]

data3=data2[data2$Cat=="GC",]

data2=data2[data2$Cat!="Exib"&data2$Cat!="GC"&data2$Cat!="Olympics",]

# Referenciel match BO3
m=mean(data2$m_ace)
md=median(data2$m_ace)

# Referenciel match BO5
m_GC=mean(data3$m_ace)
md_GC=median(data3$m_ace)

data2=data[data$minutes>40,]
data2=data2[data2$Cat!="Exib",]

library(ggpubr)
library(gridExtra)
library(grid)
library(tidyr)
library(tidyverse)
library(cowplot)

list=list(data2$Cat,data2$surface,data2$tourney_name)
#
df=aggregate(data2$m_ace,list, median)

colnames(df)[1]="Category"
colnames(df)[2]="Surface"
colnames(df)[3]="Tournament"
colnames(df)[4]="Ace_md"

df$Ace_m=round(aggregate(data2$m_ace, list(data2$Cat,data2$surface,data2$tourney_name), mean)[4],2)

# On compare la valeur de la moyenne/médiane du tournoi par rapport à l'ensemble des tournois
df$Index1=round(df$Ace_md/md,2)
df$Index2=round(df$Ace_m/m,2)

df_1000=df[df$Category=="ATP 1000",]
df_500=df[df$Category=="ATP 500",]
df_250=df[df$Category=="ATP 250",]
df_GC=df[df$Category=="GC",]

# On recalcul les index pour les GC car pas les mêmes stats
df_GC$Index1=round(df_GC$Ace_md/md_GC,2)
df_GC$Index2=round(df_GC$Ace_m/m_GC,2) 

df$Index1[df$Tournament=="Australian Open"]=df_GC$Index1[df_GC$Tournament=="Australian Open"]
df$Index1[df$Tournament=="Roland Garros"]=df_GC$Index1[df_GC$Tournament=="Roland Garros"]
df$Index1[df$Tournament=="Wimbledon"]=df_GC$Index1[df_GC$Tournament=="Wimbledon"]
df$Index1[df$Tournament=="Us Open"]=df_GC$Index1[df_GC$Tournament=="Us Open"]

df_1000$Tournament=str_replace(df_1000$Tournament,"Masters","")
df_500$Tournament=str_replace(df_500$Tournament,"Club","")
df_250$Tournament=str_replace(df_250$Tournament,"Murray River Open","Melbourne 2")
df_250$Tournament=str_replace(df_250$Tournament,"Great Ocean Road Open","Melbourne 1")

GC=ggplot(df_GC, aes(x=Tournament, y=Ace_md/md_GC, fill=Surface)) +
  geom_bar(stat="identity",width=0.3)+
  facet_grid(~Category)+
  scale_fill_manual(values=c("Chocolate","chartreuse4","Steelblue"))+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        axis.text.x=element_text(angle=0))+
  ylab("")

ATP_1000=ggplot(df_1000, aes(x=Tournament, y=Ace_md/md, fill=Surface)) +
  geom_bar(stat="identity",width=0.3)+
  facet_grid(~Category)+
  scale_fill_manual(values=c("Chocolate","Steelblue"))+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=0))+
  ylab("")

ATP_500=ggplot(df_500, aes(x=Tournament, y=Ace_md/md, fill=Surface)) +
  geom_bar(stat="identity",width=0.3)+
  facet_grid(~Category)+
  scale_fill_manual(values=c("Chocolate","chartreuse4","Steelblue"))+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=0))+
  #ylim(0,17)+
  ylab("")

ATP_250=ggplot(df_250, aes(x=Tournament, y=Ace_md/md, fill=Surface)) +
  geom_bar(stat="identity",width=0.5)+
  facet_grid(~Category)+
  scale_fill_manual(values=c("Chocolate","chartreuse4","Steelblue"))+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90))+
  ylim(0,2.9)+
  ylab("")


fig=ggarrange(GC, ATP_1000, ATP_500,ATP_250,
          ncol = 2,nrow = 2,common.legend = TRUE,legend="bottom")

annotate_figure(fig,
                top = text_grob("Aces Index per surface and tournament in 2021", color = "black", size = 16),
                bottom = text_grob("Data source: raw.githubusercontent.com/JeffSackman", color = "black",
                                   hjust = 2, vjust = 0, face = "italic", size = 10),
                left = text_grob("Index",color = "Black",size=14,rot = 90))


plot(density(data2$winner_rank_points))
lines(density(data2$loser_rank_points))

# Faire le même index avec la durée des matchs et le nbr de TB

