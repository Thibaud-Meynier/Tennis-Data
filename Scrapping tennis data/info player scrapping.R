# Fichier de fonction permettant l'extraction des informations sur les joueurs 

# Fonction pour extraire les informations
infos_players_scrap <- function(chaine) {
  # Extraire la date de naissance après "Age:"
  age_match <- regexpr("Age: [0-9]+ \\(([0-9]+\\. [0-9]+\\. [0-9]+)\\)", chaine)
  if (age_match[1] != -1) {
    date_naissance <- regmatches(chaine, age_match)[[1]]
    date_naissance <- sub("Age: [0-9]+ \\(", "", date_naissance)
    date_naissance <- sub("\\)", "", date_naissance)
    date_naissance <- as.Date(date_naissance, format = "%d. %m. %Y")
  } else {
    date_naissance <- NA
  }
  
  # Extraire la main de jeu après "Plays:"
  plays_match <- regexpr("Plays: ([a-zA-Z]+)", chaine)
  if (plays_match[1] != -1) {
    plays <- regmatches(chaine, plays_match)[[1]]
    plays <- sub("Plays: ", "", plays)
  } else {
    plays <- NA
  }
  
  # Extraire la taille et le poids après "Height / Weight:"
  size_weight_match <- regexpr("Height / Weight: ([0-9]+) cm / ([0-9]+) kg", chaine)
  if (size_weight_match[1] != -1) {
    size_weight <- regmatches(chaine, size_weight_match)[[1]]
    size <- sub("Height / Weight: ([0-9]+) cm / ([0-9]+) kg", "\\1", size_weight)
    weight <- sub("Height / Weight: ([0-9]+) cm / ([0-9]+) kg", "\\2", size_weight)
  } else {
    size <- NA
    weight <- NA
  }
  
  name_match <- regexpr("^(.*)Country:", chaine)
  if (name_match[1] != -1) {
    name <- regmatches(chaine, name_match)[[1]]
    name <- sub("Country:", "", name)
    name <- trimws(name)  # Supprimer les espaces en trop
  } else {
    name <- NULL
  }
  # Retourner une liste avec les informations extraites
  list(
    date_naissance = date_naissance,
    plays = plays,
    size = size,
    weight = weight,
    name = name
  )
}


players_info_scrap=function(date){
  
  infos_players_scrap_end=data.frame()
  
  for (i in 1:20){
    url=paste0("https://www.tennisexplorer.com/ranking/atp-men/?date=",date,"&page=",i)
    
    page_info=read_html(url)
    
    info_players=page_info %>%
      html_nodes("td.t-name>a") %>% 
      html_attr("href") %>% 
      as.data.frame() %>% 
      head(50) %>% 
      rename("URL_players"=".") %>% 
      mutate(URL_players=paste0("https://www.tennisexplorer.com",URL_players))
    
    info_players$Size=NA
    info_players$Weight=NA
    info_players$Birth_date=NA
    info_players$Hand=NA
    info_players$Name=NA
    
    
    infos_players_scrap_end=rbind(infos_players_scrap_end,info_players)
  }
  
  for (i in 1:nrow(infos_players_scrap_end)){
    
    url=infos_players_scrap_end$URL_players[i]
    page=read_html(url)
    
    data=page %>% html_nodes("#center > div.box.boxBasic.lGray > table > tbody > tr ") %>% 
      html_text()
    
    data_players=infos_players_scrap(data)
    
    # On stock les infos
    infos_players_scrap_end$Size[i]=data_players$size
    infos_players_scrap_end$Weight[i]=data_players$weight
    infos_players_scrap_end$Birth_date[i]=as.character(data_players$date_naissance)
    infos_players_scrap_end$Hand[i]=data_players$plays
    infos_players_scrap_end$Name[i]=data_players$name
    
  }
  
  return(infos_players_scrap_end)
  
}

