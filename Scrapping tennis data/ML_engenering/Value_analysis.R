
is_value=function(df,pred,margin=5,U=1){
    
  value=test_pred %>% select(1,2,3,4,5,6,7,8,9,10,11,"P_F_market","Issue")
  
  value=value %>% 
    mutate(P_F_pred=test_pred %>% pull(pred),
           
           Odd_F_pred=round(100/(P_F_pred*(100+margin)),2),
           Odd_O_pred=round(100/((1-P_F_pred)*(100+margin)),2),
           
           Ratio_Odd_F=round(Odd_F/Odd_F_pred,2),
           Ratio_Odd_O=round(Odd_O/Odd_O_pred,2),
           
           Signal = case_when(P_F_market>=0.5 & P_F_pred>=0.5~"Fav_W",
                                P_F_market<0.5 & P_F_pred<0.5~"Out_W",
                                (P_F_market<0.5 & P_F_pred>=0.5)|(P_F_market>=0.5 & P_F_pred<0.5)~"Divergence",
                                TRUE~"No Value"),
           
           Value = case_when(Signal=="Fav_W" & Odd_F/Odd_F_pred>1 ~"Value Fav1",
                             Signal=="Fav_W" & Odd_O/Odd_O_pred>1 ~"Value Out1",
                             Signal=="Out_W" & Odd_F/Odd_F_pred>1 ~"Value Fav2",
                             Signal=="Out_W" & Odd_O/Odd_O_pred>1 ~"Value Out2",
                             Signal=="Divergence" & P_F_pred>=0.5~"Divergence Fav",
                             Signal=="Divergence" & P_F_pred<0.5~"Divergence Out"),
           
           
           Odd_played = case_when(Value %in% c("Value Fav1","Value Fav2","Divergence Fav")~Odd_F,
                                  Value %in% c("Value Out1","Value Out2","Divergence Out")~Odd_O,
                                  TRUE~NA),
           
           Odd_pred = case_when(Value %in% c("Value Fav1","Value Fav2","Divergence Fav")~Odd_F_pred,
                                  Value %in% c("Value Out1","Value Out2","Divergence Out")~Odd_O_pred,
                                  TRUE~NA),
  
           Ratio_Value = case_when(Value %in% c("Value Fav1","Value Fav2")~Ratio_Odd_F,
                                   Value %in% c("Value Out1","Value Out2")~Ratio_Odd_O,
                                   TRUE~NA),
           
           Result = case_when((Value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Fav_W")|(Value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Out_W")~Odd_played*U-U,
                              (Value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Out_W")|(Value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Fav_W")~-U,
                              TRUE~NA),
           
           Bet_W = ifelse(Result>0,1,0),
           
           # -------- Reverse strategie ------------- #
           
           Odd_played_reverse = case_when(                                           
               Value %in% c("Value Fav1", "Value Fav2", "Divergence Fav") ~ Odd_O,
               Value %in% c("Value Out1", "Value Out2", "Divergence Out") ~ Odd_F,
               TRUE ~ NA_real_
             ),
           
           Result_reverse = case_when((Value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Out_W")|(Value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Fav_W")~Odd_played_reverse*U-U,
                              (Value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Fav_W")|(Value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Out_W")~-U,
                              TRUE~NA),
           
           Bet_W_reverse = case_when(
               Value %in% c("Value Fav1", "Value Fav2", "Divergence Fav") ~ as.integer(Issue == "Out_W"),
               Value %in% c("Value Out1", "Value Out2", "Divergence Out") ~ as.integer(Issue == "Fav_W"),
               TRUE ~ NA_integer_)
           )
  
  
  return(value)
  
}

# value_performance <- function(pred, margin = 5, U = 1,
#                               min_bets = 10,
#                               ratio_breaks = c(0, 1.05, 1.10, 1.25, Inf),
#                               ratio_labels = c("Faible (0-5%)", "Moyen (5-10%)",
#                                                "Intéressant (10-25%)", "Fort (25%+)"),
#                               focus = TRUE) {  # ← ajout du dataframe en argument
#   
#   # ── 1. Données Value ──────────────────────────────────────────────────────────
#   data <- is_value(pred, margin, U) %>%
#     filter(!is.na(Value)) %>%
#     filter(between(Odd_played, 1.35, 4.5)) %>%
#     mutate(
#       Ratio_Bin = cut(
#         Ratio_Value,
#         breaks         = ratio_breaks,
#         labels         = ratio_labels,
#         right          = FALSE,
#         include.lowest = TRUE
#       )
#     )
#   # ── 3. Analyse Value (focus ou non) ──────────────────────────────────────────
#   if (focus == FALSE) {
#     
#     by_value_cat_ratio <- data %>%
#       group_by(Value, Categorie, Ratio_Bin) %>%
#       summarise(
#         N          = n(),
#         N_Win      = sum(Bet_W, na.rm = TRUE),
#         WinRate    = round(N_Win / N * 100, 1),
#         Profit     = round(sum(Result, na.rm = TRUE), 2),
#         ROI        = round(sum(Result, na.rm = TRUE) / N * 100, 1),
#         Odd_Median = round(median(Odd_played, na.rm = TRUE), 2),
#         .groups    = "drop"
#       ) %>%
#       rename(Value_Type = Value)
#     
#     heatmap_cat_ratio_facet <- by_value_cat_ratio %>%
#       ggplot(aes(x = Ratio_Bin, y = Categorie, fill = Profit)) +
#       geom_tile(color = "white", linewidth = 0.8) +
#       geom_text(aes(label = paste0(Profit, "U\nn=", N)), size = 2.8) +
#       scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
#       facet_wrap(~ Value_Type, ncol = 2) +
#       labs(title = paste("Profit Catégorie x Ratio x Value —", pred),
#            x = "Niveau de Value", y = "Catégorie") +
#       theme_classic() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1),
#             strip.background = element_rect(fill = "grey90"))
#     
#     print(heatmap_cat_ratio_facet)
# 
#     
#   } else {
#     
#     by_value_ratio <- data %>%
#       group_by(Value, Ratio_Bin) %>%
#       summarise(
#         N          = n(),
#         N_Win      = sum(Bet_W, na.rm = TRUE),
#         WinRate    = round(N_Win / N * 100, 1),
#         Profit     = round(sum(Result, na.rm = TRUE), 2),
#         ROI        = round(sum(Result, na.rm = TRUE) / N * 100, 1),
#         Odd_Median = round(median(Odd_played, na.rm = TRUE), 2),
#         .groups    = "drop"
#       ) %>%
#       rename(Value_Type = Value)
#     
#     heatmap_ratio_facet <- by_value_ratio %>%
#       ggplot(aes(x = Ratio_Bin, y = Value_Type, fill = Profit)) +
#       geom_tile(color = "white", linewidth = 0.8) +
#       geom_text(aes(label = paste0(Profit, "U\nn=", N)), size = 2.8) +
#       scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
#       labs(title = paste("Profit Catégorie x Ratio x Value —", pred),
#            x = "Niveau de Value", y = "Catégorie") +
#       theme_classic() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1),
#             strip.background = element_rect(fill = "grey90"))
#     
#     print(heatmap_ratio_facet)
#     
#     invisible(list(data = by_value_ratio, diverge = diverge_results))
#   }
# }
# 
# 
# value_performance("LDA",focus=F)


##### DETAIL PAR TYPE DE VALUE #####

model_list=c( "random_forest","xgboost", "lightgbm", "knn","nnet",
              "lasso", "ridge", "elasticnet","lda","naive_bayes","logistic")

model_list=paste0("P_F_",toupper(model_list))

model_list=c(model_list,"P_F_comb","Ensemble_Pred")

value=data.frame()

for (i in model_list){
  
  temp=is_value(test_pred,i,margin=0) %>% 
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
        breaks = c(1, 1.35, 1.55, 1.70, 1.85, 2.00, 2.25, 2.60, 3.00, 3.50, 4.00, 4.50),
        labels = c("1.01-1.35","1.35-1.55","1.55-1.70","1.70-1.85","1.85-2.00",
                   "2.00-2.25","2.25-2.60","2.60-3.00","3.00-3.50","3.50-4.00","4.00-4.50"),
        right          = FALSE,
        include.lowest = TRUE
      )) %>% 
    group_by(Value) %>% 
    mutate(
      n_bet=row_number(),
      
      BK = cumsum(Result),
      
      BK_reverse = cumsum(Result_reverse),
      
      Pred = i) 
  
  value=rbind(value,temp)
  
  print(i)
  
}


##### CLASSEMENT DES MODELES PAR CATEGORIES #####


value_summary = value %>% 
  mutate(Odd_Bin = case_when(Value %in% c("Divergence Fav","Divergence Out")~"No Odd",
                             TRUE~Odd_Bin),
         Ratio_Bin = case_when(Value %in% c("Divergence Fav","Divergence Out")~"No Ratio",
                               TRUE~Ratio_Bin)) %>% 
  group_by(Pred, Value, Categorie2) %>% 
  summarise(
    N_bet        = n(),
    Accuracy_Normal = mean(Bet_W==1,na.rm=T),
    BK_Normal    = sum(Result),
    ROI_Normal = (BK_Normal/N_bet)*100,
    Median_Odd = median(Odd_played),
    Accuracy_reverse = mean(Bet_W_reverse==1,na.rm=T),
    BK_Reverse   = sum(Result_reverse),
    ROI_Reverse = (BK_Reverse/N_bet)*100,
    Median_Odd_Reverse = median(Odd_played_reverse)
  ) %>% 
  mutate(
  Strategie_Select = case_when(BK_Normal>=BK_Reverse~"Normal",
                               TRUE~"Reverse"),
  BK = case_when(BK_Normal>=BK_Reverse~BK_Normal,
               TRUE~BK_Reverse),
  
  ROI = case_when(BK_Normal>=BK_Reverse~ROI_Normal,
                  TRUE~ROI_Reverse),
  
  Accuracy = case_when(BK_Normal>=BK_Reverse~Accuracy_Normal,
                       TRUE~Accuracy_reverse)) %>% 
  select(Pred,Value,Categorie=Categorie2,Strategie_Select,N_bet,BK,ROI,Accuracy) %>% 
  group_by(Categorie,Value) %>% 
  mutate(Rank = rank(desc(ROI)))


start_selected = value_summary %>% 
  filter(Rank==1) %>% 
  arrange(Categorie)


##### Analyse par consensus #####

get_consensus_value_complete <- function(df, pred_cols, threshold = 9, margin = 0, U=1) {
  
  # 1. Initialisation des compteurs pour les 6 cas
  v_fav1 <- rep(0, nrow(df))
  v_out1 <- rep(0, nrow(df))
  v_fav2 <- rep(0, nrow(df))
  v_out2 <- rep(0, nrow(df))
  v_div_fav <- rep(0, nrow(df))
  v_div_out <- rep(0, nrow(df))
  
  # 2. Boucle sur les modèles
  for (col in pred_cols) {
    pred_vals <- df[[col]]
    p_market  <- df[["P_F_market"]]
    odd_f     <- df[["Odd_F"]]
    odd_o     <- df[["Odd_O"]]
    
    # Calcul des ratios
    odd_f_pred <- 100 / (pred_vals * (100 + margin))
    odd_o_pred <- 100 / ((1 - pred_vals) * (100 + margin))
    ratio_f    <- odd_f / odd_f_pred
    ratio_o    <- odd_o / odd_o_pred
    
    # Logique des Signaux
    sig_fav_w <- p_market >= 0.5 & pred_vals >= 0.5
    sig_out_w <- p_market <  0.5 & pred_vals <  0.5
    div_fav   <- p_market <  0.5 & pred_vals >= 0.5
    div_out   <- p_market >= 0.5 & pred_vals <  0.5
    
    # Incrémentation des votes
    v_fav1    <- v_fav1 + as.integer(sig_fav_w & ratio_f > 1 & between(odd_f, 1.35, 4.5))
    v_out1    <- v_out1 + as.integer(sig_fav_w & ratio_o > 1 & between(odd_o, 1.35, 4.5))
    v_fav2    <- v_fav2 + as.integer(sig_out_w & ratio_f > 1 & between(odd_f, 1.35, 4.5))
    v_out2    <- v_out2 + as.integer(sig_out_w & ratio_o > 1 & between(odd_o, 1.35, 4.5))
    v_div_fav <- v_div_fav + as.integer(div_fav)
    v_div_out <- v_div_out + as.integer(div_out)
  }
  
  # 3. Attribution du label final (ordre de priorité : Values > Divergences)
  df <- df %>%
    mutate(
      consensus_value = case_when(
        v_fav1 >= threshold ~ "Value Fav1",
        v_out1 >= threshold ~ "Value Out1",
        v_fav2 >= threshold ~ "Value Fav2",
        v_out2 >= threshold ~ "Value Out2",
        v_div_fav >= threshold ~ "Divergence Fav",
        v_div_out >= threshold ~ "Divergence Out",
        TRUE ~ NA_character_
      ),
      n_votes_consensus = pmax(v_fav1, v_out1, v_fav2, v_out2, v_div_fav, v_div_out),
      
      Odd_played =  case_when(consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav")~Odd_F,
                              consensus_value %in% c("Value Out1","Value Out2","Divergence Out")~Odd_O,
                              TRUE~NA),
      
      Result = case_when((consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Fav_W")|(consensus_value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Out_W")~Odd_played*U-U,
                         (consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Out_W")|(consensus_value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Fav_W")~-U,
                         TRUE~NA),
      
      Odd_played_reverse =  case_when(consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav")~Odd_O,
                                      consensus_value %in% c("Value Out1","Value Out2","Divergence Out")~Odd_F,
                              TRUE~NA),
      
      Result_reverse = case_when((consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Out_W")|(consensus_value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Fav_W")~Odd_played_reverse*U-U,
                                 (consensus_value %in% c("Value Fav1","Value Fav2","Divergence Fav") & Issue=="Fav_W")|(consensus_value %in% c("Value Out1","Value Out2","Divergence Out") & Issue=="Out_W")~-U,
                                 TRUE~NA)
    )
  
  return(df)
}

value_consensus = get_consensus_value_complete(test_pred, pred_cols = model_list, threshold = 12, margin = 0)

summary_consensus=value_consensus %>% 
  filter(!is.na(consensus_value)) %>% 
  group_by(consensus_value,Categorie2) %>% 
  summarise(N=n(),
           Win_Rate = mean(Result>=0,na.rm=T),
           Odd_Med = median(Odd_played,na.rm = T),
           BK = sum(Result),
           ROI = (BK/N)*100,
           BK_Rev = sum(Result_reverse),
           Win_Rate_Rev = mean(Result_reverse>=0,na.rm=T),
           ROI_Rev = (BK_Rev/N)*100) %>% 
  mutate(Strategie = case_when(BK>=BK_Rev~"Normal",TRUE~"Reverse"),
         BK = case_when(Strategie=="Normal"~round(BK,3),TRUE~round(BK_Rev,3)),
         ROI = case_when(Strategie=="Normal"~round(ROI,3),TRUE~round(ROI_Rev,3)),
         Accuracy = case_when(Strategie=="Normal"~round(Win_Rate,3),TRUE~round(Win_Rate_Rev,3))) %>% 
  select(consensus_value,Categorie=Categorie2,N,Odd_Med,BK,ROI,Accuracy,Strategie)


