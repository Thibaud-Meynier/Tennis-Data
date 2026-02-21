# Backing the underdog 

n=2

Strat_50_50=TABLE_MOMENTUM %>% 
  filter(Diff_Rank_Class==n) %>% 
  select(tournament,Categorie,Round,Season,Favori,Outsider,Odd_F,Odd_O,Issue) %>% 
  mutate(
    
    Odd_S=case_when(Odd_F>=Odd_O~1,TRUE~0),
    
    Player_Backed=case_when(Odd_F>Odd_O~"Fav_W",
                         Odd_O>=Odd_F~"Out_W"),
    
    Odd_played=case_when(Odd_F>Odd_O~Odd_F,
                       Odd_O>=Odd_F~Odd_O),
    
    Result=case_when(Player_Backed=="Fav_W" & Issue=="Fav_W"~Odd_played*1-1,
                     Player_Backed=="Out_W" & Issue=="Out_W"~Odd_played*1-1,
                     TRUE~-1),
    nbet=row_number())


Strat_50_50 %>%
  group_by(Categorie,Player_Backed) %>%
  mutate(BK = cumsum(Result),
         nbet=row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = nbet, y = BK, group = Player_Backed,color=Player_Backed)) +
  
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Categorie, scales = "free") +   # "free" sur les deux axes
  theme_classic()+
  ggtitle("BK en jouant l'underdog du marché")


# Backing the favorite 

Strat_50_50=TABLE_MOMENTUM %>% 
  filter(Diff_Rank_Class==n) %>% 
  select(tournament,Categorie,Round,Season,Favori,Outsider,Odd_F,Odd_O,Issue) %>% 
  mutate(
    
    Odd_S=case_when(Odd_F>=Odd_O~1,TRUE~0),
    
    Player_Backed=case_when(Odd_F>Odd_O~"Out_W",
                            Odd_O>=Odd_F~"Fav_W"),
    
    Odd_played=case_when(Odd_F>Odd_O~Odd_O,
                         Odd_O>=Odd_F~Odd_F),
    
    Result=case_when(Player_Backed=="Fav_W" & Issue=="Fav_W"~Odd_played*1-1,
                     Player_Backed=="Out_W" & Issue=="Out_W"~Odd_played*1-1,
                     TRUE~-1),
    nbet=row_number())

Strat_50_50 %>%
  group_by(Categorie,Player_Backed) %>%
  mutate(BK = cumsum(Result),
         nbet=row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = nbet, y = BK, group = Player_Backed,color=Player_Backed)) +
  
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Categorie, scales = "free") +   # "free" sur les deux axes
  theme_classic()+
  ggtitle("BK en jouant le favori du marché")
