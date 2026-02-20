

is_value=function(pred,margin=0,U=1){
  
  
  value=test_pred %>% select(1,2,3,4,5,6,7,8,9,"P_F_market","Issue")
  
  value=value %>% 
    mutate(P_F_pred=test_pred %>% pull(pred),
           
           Odd_F_pred=round(100/(P_F_pred*(100+margin)),2),
           Odd_O_pred=round(100/((1-P_F_pred)*(100+margin)),2),
           
           Ratio_Odd_F=round(Odd_F/Odd_F_pred,2),
           Ratio_Odd_O=round(Odd_O/Odd_O_pred,2),
           
           Signal = case_when(P_F_market>0.5 & P_F_pred>0.5~"Fav_W",
                                P_F_market<0.5 & P_F_pred<0.5~"Out_W",
                                (P_F_market<0.5 & P_F_pred>0.5)|(P_F_market>0.5 & P_F_pred<0.5)~"Divergence",
                                TRUE~"No Value"),
           
           Value = case_when(Signal=="Fav_W" & Odd_F/Odd_F_pred>1 ~"Value Fav1",
                             Signal=="Fav_W" & Odd_O/Odd_O_pred>1 ~"Value Out1",
                             Signal=="Out_W" & Odd_F/Odd_F_pred>1 ~"Value Fav2",
                             Signal=="Out_W" & Odd_O/Odd_O_pred>1 ~"Value Out2"),
           
           
           Odd_played = case_when(Value %in% c("Value Fav1","Value Fav2")~Odd_F,
                                  Value %in% c("Value Out1","Value Out2")~Odd_O,
                                  TRUE~NA),
  
           Ratio_Value = case_when(Value %in% c("Value Fav1","Value Fav2")~Ratio_Odd_F,
                                   Value %in% c("Value Out1","Value Out2")~Ratio_Odd_O,
                                   TRUE~NA),
           
           Result = case_when((Value %in% c("Value Fav1","Value Fav2") & Issue=="Fav_W")|(Value %in% c("Value Out1","Value Out2") & Issue=="Out_W")~Odd_played*U-U,
                              (Value %in% c("Value Fav1","Value Fav2") & Issue=="Out_W")|(Value %in% c("Value Out1","Value Out2") & Issue=="Fav_W")~-U,
                              TRUE~NA),
           
           Bet_W = ifelse(Result>0,1,0)
           )
  
  
  return(value)
  
}


Fav_1=is_value("XGB",margin=5) %>% 
  select(-c(P_F_pred,P_F_market,Ratio_Odd_O,Ratio_Odd_F,Signal)) %>% 
  filter(!is.na(Value)) %>% 
  filter(Value=="Value Out1") %>% 
  mutate(
    Ratio_Bin = cut(
      Ratio_Value,
      breaks = c(0, 1.05, 1.10, 1.25, Inf),
      labels = c("Faible (0-5%)","Moyen (5-10%)", "Intéréssant (10-25%)", "Fort (25%+)"),
      right          = FALSE,
      include.lowest = TRUE
    ),
    
    Odd_Bin = cut(
      Odd_played,
      breaks = c(1,1.10, 1.25, 1.35, 1.55, 1.70, 1.85, Inf),
      labels = c("1-1.10","1.10-1.25", "1.25-1.35", "1.35-1.55",
                 "1.55-1.70", "1.70-1.85", "1.85+"),
      right          = FALSE,
      include.lowest = TRUE
    )) %>% 
  group_by(Categorie, Ratio_Bin) %>%
  mutate(
    n_bet=row_number(),
    
   BK = cumsum(Result)) 


ggplot(Fav_1,aes(x = n_bet, y = BK, group = Ratio_Bin, color = Ratio_Bin)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Categorie, scales = "free") +       # "free" = axes adaptés par catégorie
  scale_color_viridis_d(option = "turbo") +         # rouge→vert naturel pour les tranches de value
  labs(
    title  = "Bankroll cumulée par catégorie et tranche de ratio de value",
    x      = "Nombre de paris",
    y      = "Bankroll cumulée (u.)",
    color  = "Ratio Value"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


