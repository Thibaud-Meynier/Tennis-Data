library(tidyverse)

load(paste0(getwd(),"/Scrapping tennis data/info_players/V_PLAYERS.RData"))


miss=V_PLAYERS %>% filter(is.na(Size)|is.na(Weight)) %>% arrange(Best_Rank)


V_PLAYERS$Size=as.numeric(V_PLAYERS$Size)
V_PLAYERS$Weight=as.numeric(V_PLAYERS$Weight)
V_PLAYERS$Birth_date=as.Date(V_PLAYERS$Birth_date)

V_PLAYERS[V_PLAYERS$Player_name == "Federer Roger", c("Size")] <- list(185)
V_PLAYERS[V_PLAYERS$Player_name == "Davydenko Nikolay", c("Size")] <- list(178)
V_PLAYERS[V_PLAYERS$Player_name == "Djokovic Novak", c("Size")] <- list(188)
V_PLAYERS[V_PLAYERS$Player_name == "Murray Andy", c("Size")] <- list(191)
V_PLAYERS[V_PLAYERS$Player_name == "Roddick Andy", c("Size")] <- list(188)
V_PLAYERS[V_PLAYERS$Player_name == "Nalbandian David", c("Birth_date")] <- list(as.Date("1982-01-01"))
V_PLAYERS[V_PLAYERS$Player_name == "Gonzalez Fernando", c("Size","Weight")] <- list(183,82)
V_PLAYERS[V_PLAYERS$Player_name == "Wawrinka Stan", c("Size","Weight")] <- list(183,81)
V_PLAYERS[V_PLAYERS$Player_name == "Soderling Robin", c("Size","Weight")] <- list(193,87)
V_PLAYERS[V_PLAYERS$Player_name == "Almagro Nicolas", c("Size","Weight")] <- list(183,86)
V_PLAYERS[V_PLAYERS$Player_name == "Berdych Tomas", c("Size","Weight")] <- list(196,91)
V_PLAYERS[V_PLAYERS$Player_name == "Fish Mardy", c("Size")] <- list(188)
V_PLAYERS[V_PLAYERS$Player_name == "Berdych Tomas", c("Size","Weight")] <- list(196,91)
V_PLAYERS[V_PLAYERS$Player_name == "Melzer Jurgen", c("Size","Weight")] <- list(184,80)
V_PLAYERS[V_PLAYERS$Player_name == "Ancic Mario", c("Weight")] <- list(82)
V_PLAYERS[V_PLAYERS$Player_name == "Youzhny Mikhail", c("Size","Weight")] <- list(183,73)
V_PLAYERS[V_PLAYERS$Player_name == "Fritz Taylor", c("Size")] <- list(196)
V_PLAYERS[V_PLAYERS$Player_name == "Alcaraz Carlos",  c("Size","Weight")] <- list(183,78)
V_PLAYERS[V_PLAYERS$Player_name == "Rune Holger",  c("Weight")] <- list(77)
V_PLAYERS[V_PLAYERS$Player_name == "Ferrero Juan Carlos",  c("Size","Weight")] <- list(183,73)
V_PLAYERS[V_PLAYERS$Player_name == "Ljubicic Ivan",  c("Weight")] <- list(92)
V_PLAYERS[V_PLAYERS$Player_name == "Haas Tommy",  c("Size")] <- list(188)
V_PLAYERS[V_PLAYERS$Player_name == "Cuevas Pablo",  c("Birth_date")] <- list(as.Date("1986-01-01"))
#
V_PLAYERS[V_PLAYERS$Player_name == "Shelton Ben",  c("Size","Weight")] <- list(193,88)
V_PLAYERS[V_PLAYERS$Player_name == "Fils Arthur",  c("Size","Weight")] <- list(185,83)
V_PLAYERS[V_PLAYERS$Player_name == "Navone Mariano",  c("Size","Weight")] <- list(178,73)
V_PLAYERS[V_PLAYERS$Player_name == "Cobolli Flavio",  c("Size","Weight")] <- list(183,74)
V_PLAYERS[V_PLAYERS$Player_name == "Arnaldi Matteo",  c("Size","Weight")] <- list(185,71)
V_PLAYERS[V_PLAYERS$Player_name == "Mpetshi Perricard Giovanni",  c("Size","Weight")] <- list(203,98)
V_PLAYERS[V_PLAYERS$Player_name == "Darderi Luciano",  c("Size","Weight")] <- list(183,82)
V_PLAYERS[V_PLAYERS$Player_name == "Marozsan Fabian",  c("Size","Weight")] <- list(193,75)
V_PLAYERS[V_PLAYERS$Player_name == "Michelsen Alex",  c("Size","Weight")] <- list(193,79)
V_PLAYERS[V_PLAYERS$Player_name == "Shang Juncheng",  c("Size","Weight")] <- list(180,73)
V_PLAYERS[V_PLAYERS$Player_name == "Mensik Jakub",  c("Size","Weight")] <- list(193,83)
V_PLAYERS[V_PLAYERS$Player_name == "Cazaux Arthur",  c("Size","Weight")] <- list(183,74)
V_PLAYERS[V_PLAYERS$Player_name == "Van Assche Luca",  c("Size","Weight")] <- list(178,72)
V_PLAYERS[V_PLAYERS$Player_name == "Yunchaokete Bu",  c("Size","Weight")] <- list(185,82)
V_PLAYERS[V_PLAYERS$Player_name == "Nardi Luca",  c("Size","Weight")] <- list(185,80)
V_PLAYERS[V_PLAYERS$Player_name == "Crivoi Victor-Valentin",  c("Size","Weight")] <- list(185,75)
V_PLAYERS[V_PLAYERS$Player_name == "Comesana Francisco",  c("Size","Weight")] <- list(178,72)
V_PLAYERS[V_PLAYERS$Player_name == "Diallo Gabriel",  c("Size","Weight")] <- list(203,90)
V_PLAYERS[V_PLAYERS$Player_name == "Stricker Dominic",  c("Size","Weight")] <- list(183,80)
V_PLAYERS[V_PLAYERS$Player_name == "Fearnley Jacob",  c("Size","Weight")] <- list(183,80)
V_PLAYERS[V_PLAYERS$Player_name == "Virtanen Otto",  c("Size","Weight")] <- list(193,82)
V_PLAYERS[V_PLAYERS$Player_name == "Bellucci Mattia",  c("Size","Weight")] <- list(175,77)
V_PLAYERS[V_PLAYERS$Player_name == "Medjedovic Hamad",  c("Size","Weight")] <- list(188,86)
V_PLAYERS[V_PLAYERS$Player_name == "Passaro Francesco",  c("Size","Weight")] <- list(180,83)
V_PLAYERS[V_PLAYERS$Player_name == "Vacherot Valentin",  c("Size","Weight")] <- list(193,83)
V_PLAYERS[V_PLAYERS$Player_name == "Tien Learner",  c("Size","Weight")] <- list(180,73)
V_PLAYERS[V_PLAYERS$Player_name == "Faria Jaime",  c("Size","Weight")] <- list(188,78)
V_PLAYERS[V_PLAYERS$Player_name == "Evans Brendan",  c("Size","Weight")] <- list(188,91)
V_PLAYERS[V_PLAYERS$Player_name == "Riedi Leandro",  c("Size","Weight")] <- list(191,78)
V_PLAYERS[V_PLAYERS$Player_name == "Atmane Terence",  c("Size","Weight")] <- list(193,80)
V_PLAYERS[V_PLAYERS$Player_name == "Collignon Raphael",  c("Size","Weight")] <- list(191,86)
V_PLAYERS[V_PLAYERS$Player_name == "Burruchaga Roman Andres",  c("Size","Weight")] <- list(180,83)
V_PLAYERS[V_PLAYERS$Player_name == "Recouderc Laurent",  c("Size","Weight")] <- list(183,78)
V_PLAYERS[V_PLAYERS$Player_name == "Misolic Filip",  c("Size","Weight")] <- list(180,75)
V_PLAYERS[V_PLAYERS$Player_name == "Wong Coleman",  c("Size","Weight")] <- list(191,80)
V_PLAYERS[V_PLAYERS$Player_name == "Mochizuki Shintaro",  c("Size","Weight")] <- list(175,70)
V_PLAYERS[V_PLAYERS$Player_name == "Droguet Titouan",  c("Size","Weight")] <- list(191,80)
V_PLAYERS[V_PLAYERS$Player_name == "Boyer Tristan",  c("Size","Weight")] <- list(188,84)
V_PLAYERS[V_PLAYERS$Player_name == "Llamas Ruiz Pablo",  c("Size","Weight")] <- list(188,80)
V_PLAYERS[V_PLAYERS$Player_name == "Gigante Matteo",  c("Size","Weight")] <- list(180,68)
V_PLAYERS[V_PLAYERS$Player_name == "Gomez Federico Agustin",  c("Size","Weight")] <- list(191,95)
V_PLAYERS[V_PLAYERS$Player_name == "Basavareddy Nishesh",  c("Size","Weight")] <- list(180,70)
V_PLAYERS[V_PLAYERS$Player_name == "Gensse Augustin",  c("Size","Weight")] <- list(180,73)
V_PLAYERS[V_PLAYERS$Player_name == "Blanchet Ugo",  c("Size","Weight")] <- list(175,71)
V_PLAYERS[V_PLAYERS$Player_name == "Moro Canas Alejandro",  c("Size","Weight")] <- list(183,78)
V_PLAYERS[V_PLAYERS$Player_name == "Heide Gustavo",  c("Size","Weight")] <- list(191,83)
V_PLAYERS[V_PLAYERS$Player_name == "Maestrelli Francesco",  c("Size","Weight")] <- list(196,78)
V_PLAYERS[V_PLAYERS$Player_name == "Landaluce Martin",  c("Size","Weight")] <- list(193,80)
V_PLAYERS[V_PLAYERS$Player_name == "Teixeira Maxime",  c("Size","Weight")] <- list(188,75)
V_PLAYERS[V_PLAYERS$Player_name == "Prizmic Dino",  c("Size","Weight")] <- list(188,80)
V_PLAYERS[V_PLAYERS$Player_name == "Rincon Daniel",  c("Size","Weight")] <- list(185,85)
V_PLAYERS[V_PLAYERS$Player_name == "Shelbayh Abedallah",  c("Size","Weight")] <- list(180,73)
V_PLAYERS[V_PLAYERS$Player_name == "El Aynaoui Younes",  c("Size","Weight")] <- list(193,86)
V_PLAYERS[V_PLAYERS$Player_name == "Lajal Mark",  c("Size","Weight")] <- list(191,80)
V_PLAYERS[V_PLAYERS$Player_name == "Svajda Zachary",  c("Birth_date")] <- list(as.Date("2002-11-29"))


METRIC_SIZE=V_PLAYERS %>% 
  group_by(Country) %>% 
  summarise(SD_Size=sd(Size,na.rm=T),
            Min_Size=min(Size,na.rm=T),
            Max_Size=max(Size,na.rm=T),
            Mean_Size=mean(Size,na.rm = T),
            Median_Size=median(Size,na.rm=T),
            N=n_distinct(Player_name)
            ) %>% 
  na.omit() %>% 
  filter(N>=10)
  

METRIC_WEIGHT=V_PLAYERS %>% 
  group_by(Country) %>% 
  summarise(SD_Weight=sd(Weight,na.rm=T),
            Min_Weight=min(Weight,na.rm=T),
            Max_Weight=max(Weight,na.rm=T),
            Mean_Weight=mean(Weight,na.rm = T),
            Median_Weight=median(Weight,na.rm=T),
            N=n_distinct(Player_name)
  ) %>% 
  na.omit() %>% 
  filter(N>=10)


# V_PLAYERS %>% pull(Size) %>% mean(na.rm=T) %>% round()
# V_PLAYERS %>% pull(Size) %>% sd(na.rm=T) %>% round()
# 
# V_PLAYERS %>% pull(Weight) %>% mean(na.rm=T) %>% round()
# V_PLAYERS %>% pull(Weight) %>% sd(na.rm=T) %>% round()

#rnorm(1,mean=182,sd=6)

imput_size=function(country){
  
  if (country %in% METRIC_SIZE$Country){
    
    value=METRIC_SIZE %>% 
      filter(Country==country)
    
    imput_value_size=rnorm(1,value$Mean_Size,sd=value$SD_Size)
    
  }else{
    
    imput_value_size=rnorm(1,mean=184,sd=7)
  }
  
  return(round(imput_value_size))
}

imput_weight=function(country){
  
  if (country %in% METRIC_SIZE$Country){
  
  value=METRIC_WEIGHT %>% 
    filter(Country==country)
  
  imput_value_size=rnorm(1,value$Mean_Weight,sd=value$SD_Weight)
  
  }else{
    
    imput_value_size=rnorm(1,mean=79,sd=7)
    
  }
  return(round(imput_value_size))
}

# Imputation des valeurs manquantes 

for (i in 1:nrow(V_PLAYERS)){
  
 V_PLAYERS$Size[i]=ifelse(is.na(V_PLAYERS$Size[i])==T,imput_size(V_PLAYERS$Country[i]),V_PLAYERS$Size[i])
 
 V_PLAYERS$Weight[i]=ifelse(is.na(V_PLAYERS$Weight[i])==T,imput_weight(V_PLAYERS$Country[i]),V_PLAYERS$Weight[i])
 
 print(i)
  
}


save(V_PLAYERS,file=paste0(getwd(),"/Scrapping tennis data/info_players/V_PLAYERS_RED.RData"))


