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
                              focus = TRUE,
                              divergence = NULL) {  # ← ajout du dataframe en argument
  
  # ── 1. Données Value ──────────────────────────────────────────────────────────
  data <- is_value(pred, margin, U) %>%
    filter(!is.na(Value)) %>%
    filter(between(Odd_played, 1.35, 4.5) & Ratio_Value < 1.25) %>%
    mutate(
      Ratio_Bin = cut(
        Ratio_Value,
        breaks         = ratio_breaks,
        labels         = ratio_labels,
        right          = FALSE,
        include.lowest = TRUE
      )
    )
  
  # ── 2. Analyse des prédictions divergentes ────────────────────────────────────
  diverge_results <- NULL
  plot_diverge    <- NULL
  
  if (!is.null(divergence)) {
    
    i <- pred
    
    diverge_model <- test_pred %>%
      filter((P_F_market >= 0.5 & get(i) < 0.5) | (P_F_market < 0.5 & get(i) >= 0.5)) %>%
      select(tournament, Surface_tournament, Categorie, Season, Round, Favori, Outsider,
             Odd_F, Odd_O, Issue, all_of(i), Vote_F,P_F_market) %>%
      rename(Prob_Pred = all_of(i)) %>%
      mutate(
        # ← Nouveau : type de divergence
        Divergence_Type = ifelse(
          Prob_Pred >= 0.5 & P_F_market < 0.5,
          "Modèle → Favori / Marché → Outsider",
          "Modèle → Outsider / Marché → Favori"
        ),
        Odd_Played         = ifelse(Prob_Pred >= 0.5, Odd_F, Odd_O),
        Result             = ifelse(
          (Prob_Pred >= 0.5 & Issue == "Fav_W") | (Prob_Pred < 0.5 & Issue == "Out_W"),
          Odd_Played - 1, -1
        ),
        Odd_Played_Reverse = ifelse(Prob_Pred >= 0.5, Odd_O, Odd_F),
        Result_Reverse     = ifelse(
          (Prob_Pred >= 0.5 & Issue == "Out_W") | (Prob_Pred < 0.5 & Issue == "Fav_W"),
          Odd_Played_Reverse - 1, -1
        ),
        n_bet = row_number(),
        Pred  = i
      )
    
    # ── 2a. Stats globales par type de divergence ────────────────────────────────
    diverge_global <- diverge_model %>%
      group_by(Divergence_Type) %>%
      summarise(
        N              = n(),
        N_Win          = sum(Result > 0),
        WinRate        = round(N_Win / N * 100, 1),
        Profit         = round(sum(Result), 2),
        ROI            = round(sum(Result) / N * 100, 1),
        Odd_Median     = round(median(Odd_Played), 2),
        Profit_Market  = round(sum(Result_Reverse), 2),
        ROI_Market     = round(sum(Result_Reverse) / N * 100, 1),
        .groups        = "drop"
      )
    
    # ── 2b. Stats par Catégorie x Type de divergence ─────────────────────────────
    diverge_by_cat <- diverge_model %>%
      group_by(Divergence_Type, Categorie) %>%
      summarise(
        N       = n(),
        N_Win   = sum(Result > 0),
        WinRate = round(N_Win / N * 100, 1),
        Profit  = round(sum(Result), 2),
        ROI     = round(sum(Result) / N * 100, 1),
        .groups = "drop"
      ) %>%
      filter(N >= min_bets)
    
    # ── 2c. Courbe de profit cumulé par type de divergence ───────────────────────
    diverge_cumul <- diverge_model %>%
      arrange(Divergence_Type, n_bet) %>%
      group_by(Divergence_Type) %>%
      mutate(
        Cumul_Model  = cumsum(Result),
        Cumul_Market = cumsum(Result_Reverse)
      ) %>%
      ungroup() %>%
      select(n_bet, Divergence_Type, Cumul_Model, Cumul_Market) %>%
      pivot_longer(cols = c(Cumul_Model, Cumul_Market),
                   names_to = "Source", values_to = "Cumul") %>%
      mutate(Source = recode(Source,
                             Cumul_Model  = paste0("Modèle (", pred, ")"),
                             Cumul_Market = "Marché"))
    
    plot_diverge <- ggplot(diverge_cumul, aes(x = n_bet, y = Cumul, color = Source)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = c("steelblue", "tomato")) +
      facet_wrap(~ Divergence_Type, ncol = 1) +   # ← facet par type
      labs(
        title    = paste("Profit cumulé — Prédictions divergentes —", pred),
        x = "N paris", y = "Profit cumulé (U)", color = ""
      ) +
      theme_classic() +
      theme(strip.background = element_rect(fill = "grey90"))
    
    print(plot_diverge)
    
    # ── 2d. Heatmap divergences par Catégorie x Type ─────────────────────────────
    if (nrow(diverge_by_cat) > 0) {
      plot_diverge_cat <- diverge_by_cat %>%
        ggplot(aes(x = "Divergent", y = Categorie, fill = Profit)) +
        geom_tile(color = "white", linewidth = 0.8) +
        geom_text(aes(label = paste0(Profit, "U\nn=", N)), size = 2.8) +
        scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
        facet_wrap(~ Divergence_Type, ncol = 2) +   # ← facet par type
        labs(title = paste("Profit divergences par Catégorie —", pred),
             x = "", y = "Catégorie") +
        theme_classic() +
        theme(strip.background = element_rect(fill = "grey90"))
      
      print(plot_diverge_cat)
    }
    
    # ── Résumé console par type ──────────────────────────────────────────────────
    # cat("\n── Prédictions divergentes (modèle vs marché) ──────────────────\n")
    # for (k in seq_len(nrow(diverge_global))) {
    #   row <- diverge_global[k, ]
    #   cat(sprintf("\n  [%s]\n", row$Divergence_Type))
    #   cat(sprintf("  N        : %d\n",     row$N))
    #   cat(sprintf("  WinRate  : %.1f%%\n", row$WinRate))
    #   cat(sprintf("  Profit   : %.2fU  (ROI: %.1f%%)\n", row$Profit,        row$ROI))
    #   cat(sprintf("  Si marché: %.2fU  (ROI: %.1f%%)\n", row$Profit_Market, row$ROI_Market))
    # }
    # cat("────────────────────────────────────────────────────────────────\n\n")
    
    invisible(diverge_results <- list(
      global = diverge_global,
      by_cat = diverge_by_cat,
      raw    = diverge_model
    ))
  }
  
  # ── 3. Analyse Value (focus ou non) ──────────────────────────────────────────
  if (focus == FALSE) {
    
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
           x = "Niveau de Value", y = "Catégorie") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "grey90"))
    
    print(heatmap_cat_ratio_facet)
    
    invisible(list(data = by_value_cat_ratio, diverge = diverge_results))
    
  } else {
    
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
           x = "Niveau de Value", y = "Catégorie") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "grey90"))
    
    print(heatmap_ratio_facet)
    
    invisible(list(data = by_value_ratio, diverge = diverge_results))
  }
}


value_performance("NN1",divergence = T,focus=F)
