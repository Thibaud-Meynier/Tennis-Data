library(glue)

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

value_performance <- function(pred, margin = 0, U = 1,
                              min_bets = 10,
                              ratio_breaks = c(0, 1.05, 1.10, 1.25, Inf),
                              ratio_labels = c("Faible (0-5%)", "Moyen (5-10%)",
                                               "Intéressant (10-25%)", "Fort (25%+)"),
                              focus=TRUE) {
  
  # Récupérer les données via is_value
  data <- is_value(pred, margin, U) %>%
    filter(!is.na(Value)) %>%
    filter(between(Odd_played,1.35,4.5)) %>% 
    mutate(
      Ratio_Bin = cut(
        Ratio_Value,
        breaks         = ratio_breaks,
        labels         = ratio_labels,
        right          = FALSE,
        include.lowest = TRUE
      )
    )
  
  # ── 5. Par type de Value x Catégorie x Ratio_Bin (le plus fin) ───────────────
  
  if (focus==FALSE){
    
    by_value_cat_ratio <- data %>%
      group_by(Value, Categorie, Ratio_Bin) %>%
      summarise(
        N          = n(),
        N_Win      = sum(Bet_W, na.rm = TRUE),
        WinRate    = round(N_Win / N * 100, 1),
        Profit     = round(sum(Result, na.rm = TRUE), 2),
        ROI        = round(sum(Result, na.rm = TRUE) / N * 100, 1),
        Odd_Median = round(median(Odd_played, na.rm = TRUE), 2),
        .groups    = "drop"
      ) %>%
      rename(Value_Type = Value)
    
    heatmap_cat_ratio_facet <- by_value_cat_ratio %>%
      ggplot(aes(x = Ratio_Bin, y = Categorie, fill = Profit)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Profit, "U\nn=", N)), size = 2.8) +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      facet_wrap(~ Value_Type, ncol = 2) +
      labs(title = paste("Profit Catégorie x Ratio x Value —", pred),
           x     = "Niveau de Value",
           y     = "Catégorie") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "grey90"))
    
    print(heatmap_cat_ratio_facet)
    
    list(
      data = by_value_cat_ratio
    )
    
  }else{
    
    by_value_ratio <- data %>%
      group_by(Value, Ratio_Bin) %>%
      summarise(
        N          = n(),
        N_Win      = sum(Bet_W, na.rm = TRUE),
        WinRate    = round(N_Win / N * 100, 1),
        Profit     = round(sum(Result, na.rm = TRUE), 2),
        ROI        = round(sum(Result, na.rm = TRUE) / N * 100, 1),
        Odd_Median = round(median(Odd_played, na.rm = TRUE), 2),
        .groups    = "drop"
      ) %>%
      rename(Value_Type = Value)
    
    heatmap_ratio_facet <- by_value_ratio %>%
      ggplot(aes(x = Ratio_Bin, y = Value_Type, fill = Profit)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Profit, "U\nn=", N)), size = 2.8) +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      labs(title = paste("Profit Catégorie x Ratio x Value —", pred),
           x     = "Niveau de Value",
           y     = "Catégorie") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "grey90"))
    
    print(heatmap_ratio_facet)
    
    list(
      data = by_value_ratio
    )
    
  }
  


}

model_diag=value_performance("NN1", margin = 0,focus = F)

model_diag$by_value_cat_ratio

model_list=c("NN1","Random_Forest","Naive_Bayes",
             "Ridge","LASSO","Elastic","LDA","KNN",
             "XGB","LGB","GBM","P_F_comb","Ensemble_Pred","Naive_Bayes")

value=data.frame()

for (i in model_list){
  
  temp=is_value(i,margin=0) %>% 
    select(-c(P_F_pred,P_F_market,Ratio_Odd_O,Ratio_Odd_F,Signal)) %>% 
    filter(!is.na(Value)) %>% 
    filter(between(Odd_played,1.35,4.5)) %>% 
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
        breaks = c(1, 1.10, 1.25, 1.35, 1.55, 1.70, 1.85, 2, 2.15, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 4, 4.5, 5, Inf),
        labels = c("1-1.10", "1.10-1.25", "1.25-1.35", "1.35-1.55",
                   "1.55-1.70", "1.70-1.85", "1.85-2", "2-2.15",
                   "2.15-2.25", "2.25-2.50", "2.50-2.75", "2.75-3",
                   "3-3.25", "3.25-3.50", "3.50-4", "4-4.50",
                   "4.50-5", "5+"),
        right          = FALSE,
        include.lowest = TRUE
      )) %>% 
    group_by(Value,Categorie,Ratio_Bin) %>% 
    mutate(
      n_bet=row_number(),
      
      BK = cumsum(Result),
      
      Pred = i) 
  
  value=rbind(value,temp)
  
  print(i)
  
}


p=ggplot(value %>% 
           filter(Categorie=="Grand Slam" & Value=="Value Out1"),
         aes(x = n_bet, y = BK, group = Pred, color = Pred)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Ratio_Bin, scales = "free") +       # "free" = axes adaptés par catégorie
  scale_color_viridis_d(option = "turbo") +         # rouge→vert naturel pour les tranches de value
  labs(
    title  = "Bankroll cumulée par catégorie et tranche de ratio de value",
    x      = "Nombre de paris",
    y      = "Bankroll cumulée (u.)",
    color  = "Ratio Value"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggplotly(p)


##### ANALYSE DES DIVERGENCES PAR MODELES #####

divergence <- data.frame()

for (i in model_list){
  
  print(i)
  
  diverge_model <- test_pred %>% 
    filter((P_F_market >= 0.5 & get(i) < 0.5) | (P_F_market < 0.5 & get(i) >= 0.5)) %>% 
    select(tournament, Surface_tournament, Categorie, Season, Round, Favori, Outsider,
           Odd_F, Odd_O, Issue, all_of(i),Vote_F) %>%   # ← fix ici
    rename(Prob_Pred = all_of(i)) %>%  
    mutate(
      Odd_Played = ifelse(Prob_Pred >= 0.5, Odd_F, Odd_O),
      Result     = ifelse(
        (Prob_Pred >= 0.5 & Issue == "Fav_W") | (Prob_Pred < 0.5 & Issue == "Out_W"),
        Odd_Played - 1,
        -1
      ), 
      Odd_Played_Reverse = ifelse(Prob_Pred >= 0.5, Odd_O, Odd_F),
      Result_Reverse     = ifelse(
        (Prob_Pred >= 0.5 & Issue == "Out_W") | (Prob_Pred < 0.5 & Issue == "Fav_W"),
        Odd_Played_Reverse - 1,
        -1
      ),
      
      n_bet      = row_number(),
      Pred       = i
    )
  
  divergence <- rbind(divergence, diverge_model)

  }

divergence_plot <- divergence %>% filter(Prob_Pred<0.5) %>% 
  group_by(Pred,Categorie) %>%
  mutate(
    n_bet  = row_number(),        # Recrée n_bet par modèle
    BK     = cumsum(Result),
    BK_Reverse     = cumsum(Result_Reverse)
  ) %>%
  ungroup()

p <- ggplot(divergence_plot, aes(x = n_bet, y = BK, group = Pred, color = Pred)) +
  geom_line()+ 
  facet_wrap(~Categorie,scale="free")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_classic()

ggplotly(p)

