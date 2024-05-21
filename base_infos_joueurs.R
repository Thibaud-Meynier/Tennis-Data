player=read.csv2("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv",sep=",")

player$dob=as.Date(paste0(substr(player$dob,1,4),"-",
                          substr(player$dob,5,6),"-",
                          substr(player$dob,7,8)))

player$full_name=paste0(player$name_last," ",player$name_first)

player=player %>% filter(year(dob)>=1970)

players_test=rank %>% 
  left_join(player %>% select(-wikidata_id),by=c("Player name"="full_name"))

list_red=players_test %>% filter(is.na(player_id))
# Modifier le nom Agut et Ramos vinolas dans les bases

player[player$player_id==105077,]$name_last="Ramos-Vinolas"

player[player$player_id==105138,]$name_last="Bautista-Agut"

player[player$player_id==111456,]$name_last="McDonald"

player[player$player_id==106331,]$name_last="O'Connell"

player[player$player_id==105526,]$name_first="Jan-Lennard"

player[player$player_id==144817,]$name_first="Marc-Andrea"

player[player$player_id==200670,]$name_first="Jeffrey John"

player[player$player_id==105807,]$name_last="Carreno-Busta"

player[player$player_id==144642,]$name_last="Barrios Vera Marcelo"

player[player$player_id==200335,]$name_last="Meligeni Rodrigues Alves"

player$full_name=paste0(player$name_last," ",player$name_first)
