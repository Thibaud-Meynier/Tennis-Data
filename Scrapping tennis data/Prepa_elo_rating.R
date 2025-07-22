
elo_novak=Elo_player(player_name="Djokovic Novak") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_rafa=Elo_player(player_name="Nadal Rafael") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_roger=Elo_player(player_name="Federer Roger") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_andy=Elo_player(player_name="Murray Andy") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

plot(elo_roger$Date,elo_roger$Elo_player,type='l',col='green',ylim=c(1500,2700),xlim = c(as.Date('2003-01-01'),as.Date('2024-12-31')))
lines(elo_rafa$Date,elo_rafa$Elo_player,type='l',col="brown")
lines(elo_novak$Date,elo_novak$Elo_player,type='l',col="steelblue")
lines(elo_andy$Date,elo_andy$Elo_player,type='l',col="orange")


elo_roger %>% slice(which.max(elo_roger$Elo_player)) %>% pull(Elo_player,Date)

elo_rafa %>% slice(which.max(elo_rafa$Elo_player)) %>% pull(Elo_player,Date)

elo_novak %>% slice(which.max(elo_novak$Elo_player)) %>% pull(Elo_player,Date)

elo_andy %>% slice(which.max(elo_andy$Elo_player)) %>% pull(Elo_player,Date)
