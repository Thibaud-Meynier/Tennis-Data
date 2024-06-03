library(rvest)
library(xml2)
library(tidyverse)
library(sjmisc)

year=2020

list_tournament=function(year){
  
  # URL de la page a scraper
  url <- paste0("https://www.tennisexplorer.com/calendar/atp-men/",year,"/")
  
  # Fonction pour extraire le mois et le nom des tournois
  
  page <- read_html(url)
  
  list=data.frame("tournament"=html_text(elements <- page %>%
                                           html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody th.t-name")))
  
  # On extrait les noms des tournois de la page
  elements <- page %>%
    html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody tr th a")
  
  x=xml_attrs(elements)
  
  list[,2]=as.data.frame(matrix(nrow=length(elements),NA))
  n=length(elements)
  
  for(i in 1:n){
    
    y=as.data.frame(x[[i]])
    list[i,2]=as.character(y[,1])
    #print(i)
    
  }
  
  list=list %>% 
    mutate("Valid"=grepl("UTR Pro Tennis Series",list$tournament)) %>% 
    filter(Valid==F) %>% 
    select(1,2)
  
  list=list %>% filter(!tournament %in% exclusion) %>% 
    rename("URL"=V1)
  
  table=page %>% html_nodes("table#tournamentList") %>% html_table()
  table=table[[1]] %>% select(1,2)
  colnames(table)=table[1,]
  table=table[-1,]
  
  list=list %>% 
    left_join(table,by=c("tournament"="Tournament"))
  
  list$Started=dmy(gsub("['^.^']", "-", substr(list$Started,1,10)))
  
  list=list %>% rename("Date"=Started)
  
  list=unique(list) %>% arrange(Date)
  
  list$Categorie=NA
  
  for (i in 1:nrow(list)){
    
    list$Categorie[i]=ifelse(str_contains(list$tournament[i],c("chall.","challenger"),logic="or")==TRUE,
                          "Challenger","ATP")
  }
  
  list$URL=paste0("https://www.tennisexplorer.com",list$URL)
  
  return(list)
}


tournament_list=list_tournament(year)

info_tournament=function(tournament_list){
  
calendar_info=data.frame()

for (i in tournament_list$tournament){
  
  url=tournament_list %>% filter(tournament==i) %>% select(URL) %>% as.character()
  
  page=read_html(url)
  
  tournament_info=page %>% html_nodes("#center > div:nth-child(7) > div > div > table") %>% html_table()
  
if (length(tournament_info)>0){
    
    tournament_info=tournament_info[[1]]
  
  if (nrow(tournament_info)>9|nrow(tournament_info)<3){
    tournament_info=data.frame("Round"=NA,"Prize money"=NA,"Ranking points"=NA)
    tournament_info$tournament=i
    tournament_info=tournament_info %>% rename("Prize money"=Prize.money,
                                               "Ranking points"=Ranking.points)
  }else{
    
    colnames(tournament_info)=tournament_info[1,]
    
    tournament_info=tournament_info[-1,]
    
    tournament_info$tournament=i
    
    tournament_info=tournament_info %>% 
      mutate(Round=case_when(Round=="1. round"~"1R",
                             Round=="2. round"~"2R",
                             Round=="3. round"~"3R",
                             Round=="round of 16"~"R16",
                             Round=="quarterfinal"~"QF",
                             Round=="semifinal"~"SF",
                             Round=="final"~"F",
                             TRUE~"Winner"))
    
  }
    calendar_info=rbind(calendar_info,tournament_info)
}else{
  tournament_info=data.frame("Round"=NA,"Prize money"=NA,"Ranking points"=NA)
  tournament_info$tournament=i
  tournament_info=tournament_info %>% rename("Prize money"=Prize.money,
                                             "Ranking points"=Ranking.points)
  
  calendar_info=rbind(calendar_info,tournament_info)
}

}

return(calendar_info)

}

calendar_info=info_tournament(list)

list=list %>% 
  left_join(calendar_info %>% 
                          group_by(tournament) %>% 
                          mutate(Points=max(as.numeric(`Ranking points`))) %>% 
                          select(tournament,Points) %>% 
              unique(),by=c("tournament"="tournament"))

# Categorie Points Race

# GS

# Qualif

#Q1 = 0
#Q2 = 8
#Q3 = 16
#Q-F=25

# Main Draw

#R1 = 10
#R2 = 45
#R3 = 90
#R4 = 180
#QF = 360
#SF = 720
#Final = 1200
#Winner = 2000

# M1000 (IW, Miami)

# Qualif

#Q1 = 0
#Q2 = 8
#Q-F=16

# Main Draw

#R1 = 10
#R2 = 25
#R3 = 45
#R4 = 90
#QF = 180
#SF = 360
#Final = 600
#Winner = 1000

# MC, Bercy, Cinci

# Qualif

#Q1 = 0
#Q2 = 8
#Q-F=25

# Main Draw

#R2 = 10
#R3 = 45
#R4 = 90
#QF = 180
#SF = 360
#Final = 600
#Winner = 1000

# ATP 500 (48 players)

# Qualif

#Q1 = 0
#Q2 = 4
#Q-F=10

# Main Draw

#R3 = 20
#R4 = 45
#QF = 90
#SF = 180
#Final = 300
#Winner = 500

# ATP 500 (32 players)

# Qualif

#Q1 = 0
#Q2 = 10
#Q-F=20

# Main Draw

#R4 = 45
#QF = 90
#SF = 180
#Final = 300
#Winner = 500

# ATP 250 (48 players) Winston Salem

# Qualif

#Q1 = 0
#Q2 = 3
#Q-F=5

# Main Draw

#R3 = 10
#R4 = 20
#QF = 45
#SF = 90
#Final = 150
#Winner = 250


# ATP 250 (32 players)

# Qualif

#Q1 = 0
#Q2 = 6
#Q-F=12

# Main Draw

#R4 = 20
#QF = 45
#SF = 90
#Final = 150
#Winner = 250

# ATP 125

#R3 = 5
#R4 = 10
#QF = 25
#SF = 45
#Final = 75
#Winner = 125

# ATP 110

#R3 = 5
#R4 = 9
#QF = 20
#SF = 40
#Final = 65
#Winner = 110

# ATP 100

#R3 = 5
#R4 = 8
#QF = 18
#SF = 35
#Final = 60
#Winner = 100

# ATP 90

#R3 = 5
#R4 = 8
#QF = 17
#SF = 33
#Final = 55
#Winner = 90

# ATP 80

#R3 = 3
#R4 = 7
#QF = 15
#SF = 29
#Final = 48
#Winner = 80

##### Table technique calcul classement Race #####

QualifGS=data.frame("Phase"="Qualification",
                    "Round"=c("Q-Q1","Q-Q2","Q-Q3","Q-QF"),
                    "Points"=c(0,8,16,25))

MainDrawGS=data.frame("Phase"="Main Draw",
                      "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                      "Points"=c(10,45,90,180,360,720,1200,2000))