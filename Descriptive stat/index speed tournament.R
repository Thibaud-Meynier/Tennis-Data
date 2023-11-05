# Calcul de la rapidité des courts

library(ggpubr)
library(gridExtra)
library(grid)
library(tidyr)
library(tidyverse)
library(cowplot)
library(sqldf)

year=2003

url="https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches"

link=paste(url,'_',year,'.csv',sep='')

data=read.csv2(link,sep=',')

# on enlève les matchs finis avant terme (ab, wo ou disqualificaton)

patern=c('RET','W/O','Def.','DEF') #patern à detecter dans la variable score
patern2=c('Davis Cup','Olympics','Laver Cup')

#filter for rows where status column contains one of several strings

new_df <- filter(data, grepl(paste(patern, collapse='|'), data$score),.preserve=TRUE)

new_df2 <- filter(data, grepl(paste(patern2, collapse='|'), data$tourney_name),.preserve=TRUE)

data$test=grepl(paste(patern, collapse='|'), data$score)

data$test2=data$tourney_name %in% new_df2$tourney_name # is in

# clean data

data2=data[data$test==FALSE&data$test2==FALSE,]

# Clean the score with TB

data2$game=NA

for (i in 1:nrow(data2)){
  # on nettoi la ièeme ligne
  x=strsplit(gsub("\\s*\\([^\\)]+\\)","",data2$score[i]),"-| ",fixed=F)
  data2$game[i]=sum(as.numeric(as.character(x[[1]])))
  print(i)
}

data2=select(data2,-c("test","test2"))

data2$ace_m=data2$l_ace+data2$w_ace

data2$ace_m_h=data2$ace_m/(data2$minutes/60)

data2$ace_m_g=data2$ace_m/(data2$game)

sqldf("select distinct tourney_name,surface from data2")

indoor=c('Antwerp','Metz','Montpellier','NextGen Finals','Nur-Sultan',
         'Paris Masters','Rotterdam','Singapore','Sofia','St. Petersburg','Stockholm',
         'Tour Finals','Vienna','Astana','Dallas','Basel','Tel Aviv','Florence',
         'Giron','Moscow','New York','Cologne 1','Cologne 2','Kuala Lumpur','Bangkok',
         'Memphis','San Jose','Zagreb','Valencia','Warsaw','Lyon','Milan',"Marseille")

# adding indoor surface
data2$surface=as.character(data2$surface)
data2$surface2=data2$surface
data2$surface2[data2$tourney_name%in% indoor]=c("Indoor")

table(data2$surface2)

patern_1000=c('Masters','Tour Finals','Masters Cup')
patern_gc=c("Australian Open","Roland Garros","Wimbledon","US Open","Us Open")
patern_500=c("ATP Cup","Acapulco","Dubai","Rio","Vienna","Halle","Rotterdam",
             "Barcelona","Queen's Club","Washington","Hamburg","Basel","Astana",
             "Tokyo","Beijing","Memphis")

data2$cat=ifelse(grepl(paste(patern_gc, collapse='|'),data2$tourney_name),"GC",
       ifelse(grepl(paste(patern_1000, collapse='|'),data2$tourney_name),"ATP 1000",
              ifelse(grepl(paste(patern_500, collapse='|'),data2$tourney_name),"ATP 500",
               "ATP 250")))
      

table(data2$cat)

list=list(data2$cat,data2$surface2,data2$tourney_name)

df=aggregate(data2$ace_m_h,list, mean,na.rm=T) # calcul de l'index de vitesse

colnames(df)[1]="Category"
colnames(df)[2]="Surface"
colnames(df)[3]="Tournament"
colnames(df)[4]="index"

n=which(is.na(df[,4]))
n
t=df$Tournament[n]
t

#for (i in 1:length(n)){
  z=n[i]
  t=df$Tournament[z]
  df[z,4]=mean(data2$ace_m_h[data2$tourney_name==t],na.rm=T)
  print(i)
}

for (i in 1:length(n)){ 
  
if (length(which(is.na(df[,4])))>0){
  z=n[i]
  t=df$Tournament[z]
  s=df$Surface[z]
  df[z,4]=mean(data2$ace_m_h[data2$tourney_name==t],na.rm=T)
  df[n,4]=mean(data2$ace_m_h[data2$surface2==s&data2$tourney_name!=t],na.rm=T)
  print(i)
}
else {
    print("aucuns changements")}

}

table(is.na(df$index))

# Ranking des 10 tournois les plus rapides
head(df[order(df$index,decreasing=T),],10)

# Ranking des 10 tournois les plus rapides
head(df[order(df$index,decreasing=F),],10)

quantile(df$index)

# un DF par catégorie de tournoi (pour le plot ensuite)

df_gc=df[df$Category=='GC',]
df_1000=df[df$Category=='ATP 1000',]
df_500=df[df$Category=='ATP 500',]
df_250=df[df$Category=='ATP 250',]

# plotting

# code couleur selon la surface
col_code=data.frame('col'=c("Chocolate","chartreuse4","Steelblue","Purple"),'Surface'=c('Clay','Grass','Hard','Indoor'))

# on selectionne les couleurs had hoc en fonction des surfaces à plot
col=col=as.character(col_code$col[col_code$Surface%in%df_gc$Surface])

GC=ggplot(df_gc, aes(x=Tournament, y=index, fill=Surface)) +
  geom_bar(stat="identity",width=0.3,colour='black')+
  #geom_text(aes(label=index),vjust=1.5)+
  facet_grid(~Category)+
  scale_fill_manual(values=col)+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        axis.text.x=element_text(angle=60,vjust=0.5))+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,max(df_gc$index),2))

GC

col=as.character(col_code$col[col_code$Surface%in%df_1000$Surface])
col

ATP1000=ggplot(df_1000, aes(x=Tournament, y=index, fill=Surface)) +
  geom_bar(stat="identity",width=0.3,colour='black')+
  #geom_text(aes(label=index),vjust=1.5)+
  facet_grid(~Category)+
  scale_fill_manual(values=col)+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        axis.text.x=element_text(angle=60,vjust=0.5))+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,max(df_1000$index),2))

ATP1000

col=as.character(col_code$col[col_code$Surface%in%df_500$Surface])

ATP500=ggplot(df_500, aes(x=Tournament, y=index, fill=Surface)) +
  geom_bar(stat="identity",width=0.3,colour='black')+
  #geom_text(aes(label=index),vjust=1.5)+
  facet_grid(~Category)+
  scale_fill_manual(values=col)+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        axis.text.x=element_text(angle=60,vjust=0.5))+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,max(df_500$index),2))

ATP500

col=as.character(col_code$col[col_code$Surface%in%df_250$Surface])

ATP250=ggplot(df_250, aes(x=Tournament, y=index, fill=Surface)) +
  geom_bar(stat="identity",width=0.3,colour='black')+
  #geom_text(aes(label=index),vjust=1.5)+
  facet_grid(~Category)+
  scale_fill_manual(values=col)+#))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.3, linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(colour="black", fill="darkgrey", 
                                        size=0.3, linetype="solid"),
        axis.text.x=element_text(angle=90))+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,max(df_250$index),2))

ATP250

prow <- plot_grid(GC + theme(legend.position="none"),
                   ATP1000 + theme(legend.position="none"),
                   ATP500 + theme(legend.position="none"),
                   #ATP250 + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1,
                  ncol=3
)

prow
# extract the legend from one of the plots

legend_b <- get_legend(ATP250 + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid(prow,ATP250 + theme(legend.position="bottom"),nrow=2,ncol = 1)
p

#fig=ggarrange(GC, ATP1000, ATP500,ATP250,
              #ncol = 2,nrow = 2,common.legend = F,legend="bottom")
#fig

text=paste("Data source: raw.githubusercontent.com/JeffSackman",
      "Index=Tournament mean ace per hour"
      ,sep="                                                                                                                                                                                  ")

title=paste("Aces Index per surface and tournament in",year,sep=" ")
# on ajoute un titre aux axes et au graph + les sources et le calcul

annotate_figure(p,
                top = text_grob(title, color = "black", size = 16),
                bottom = text_grob(text, color = "black",
                                   hjust = 0.5, vjust = 0, face = "italic", size = 10),
                left = text_grob("Index",color = "Black",
                                 size=14,rot = 90))

hist(df$index,breaks=15)

boxplot(df$index)

