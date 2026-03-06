
conflicts_prefer(dplyr::filter)

tournoi="Rio De Janeiro"

Year=2010

simulate_tournament_strategy <- function(tournoi,Year,Favorite_Side="Market",Diff_Rank=Inf) {
  
  if(Favorite_Side=="Market"){
    
    # Préparation des données
    data <- V_MATCH_t %>%
      filter(tournament %in% tournoi & year(Date) >= Year & info == "Completed" & abs(Rank_W-Rank_L)<=Diff_Rank) %>%
      select(Date, Winner_id, Loser_id, Round, Odd_W, Odd_L, Rank_W, Rank_L) %>%
      na.omit() %>% 
      mutate(
        # Identification favori/outsider par la cote
        Favorite_id       = ifelse(Odd_W <= Odd_L, Winner_id, Loser_id),
        Outsider_id       = ifelse(Odd_W <= Odd_L, Loser_id, Winner_id),
        Favorite_Odd      = ifelse(Odd_W <= Odd_L, Odd_W, Odd_L),
        Outsider_Odd      = ifelse(Odd_W <= Odd_L, Odd_L, Odd_W),
        
        # Est-ce que le favori a gagné ?
        Favorite_won      = ifelse(Odd_W <= Odd_L, TRUE, FALSE),
        
        # Gain net si on mise 1 unité
        # Si on gagne : on récupère la cote - 1 (mise)
        # Si on perd  : on perd 1
        Profit_Favorite   = ifelse(Favorite_won,  Favorite_Odd - 1, -1),
        Profit_Outsider   = ifelse(!Favorite_won, Outsider_Odd - 1, -1)
      )
    
  }else{
    
    data <- V_MATCH_t %>%
      filter(tournament %in% tournoi & year(Date) >= Year & info == "Completed" & abs(Rank_W-Rank_L)<=Diff_Rank) %>%
      select(Date, Winner_id, Loser_id, Round, Odd_W, Odd_L, Rank_W, Rank_L) %>%
      na.omit() %>% 
      mutate(
        # Identification favori/outsider par le classement (rang le plus bas = meilleur)
        Favorite_id       = ifelse(Rank_W <= Rank_L, Winner_id, Loser_id),
        Outsider_id       = ifelse(Rank_W <= Rank_L, Loser_id, Winner_id),
        Favorite_Rank     = ifelse(Rank_W <= Rank_L, Rank_W, Rank_L),
        Outsider_Rank     = ifelse(Rank_W <= Rank_L, Rank_L, Rank_W),
        Favorite_Odd      = ifelse(Rank_W <= Rank_L, Odd_W, Odd_L),
        Outsider_Odd      = ifelse(Rank_W <= Rank_L, Odd_L, Odd_W),
        
        # Est-ce que le favori (mieux classé) a gagné ?
        Favorite_won      = ifelse(Rank_W <= Rank_L, TRUE, FALSE),
        
        # Gain net si on mise 1 unité
        Profit_Favorite   = ifelse(Favorite_won,  Favorite_Odd - 1, -1),
        Profit_Outsider   = ifelse(!Favorite_won, Outsider_Odd - 1, -1)
      )
    
  }
  
  
  n_matches <- nrow(data)
  
  # ── Stratégie FAVORI ──────────────────────────────────────────────
  fav_wins    <- sum(data$Favorite_won,na.rm=T)
  fav_roi     <- sum(data$Profit_Favorite,na.rm=T) / n_matches * 100
  fav_winrate <- fav_wins / n_matches * 100
  fav_avg_odd <- mean(data$Favorite_Odd,na.rm=T)
  fav_cumul   <- cumsum(na.omit(data$Profit_Favorite))
  
  # ── Stratégie OUTSIDER ────────────────────────────────────────────
  out_wins    <- sum(!data$Favorite_won,na.rm=T)
  out_roi     <- sum(data$Profit_Outsider,na.rm=T) / n_matches * 100
  out_winrate <- out_wins / n_matches * 100
  out_avg_odd <- mean(data$Outsider_Odd,,na.rm=T)
  out_cumul   <- cumsum(na.omit(data$Profit_Outsider))
  
  # ── Résumé console ───────────────────────────────────────────────
  cat("═══════════════════════════════════════════════════\n")
  cat(sprintf("  Tournoi : %s  |  %d matchs analysés\n", tournoi, n_matches))
  cat("═══════════════════════════════════════════════════\n\n")
  
  cat("  STRATÉGIE          │  FAVORI   │  OUTSIDER\n")
  cat("  ─────────────────────────────────────────\n")
  cat(sprintf("  Victoires          │  %5.1f%%  │  %5.1f%%\n",  fav_winrate, out_winrate))
  cat(sprintf("  Cote moyenne       │  %6.2f  │  %6.2f\n",     fav_avg_odd, out_avg_odd))
  cat(sprintf("  ROI                │  %+5.1f%%  │  %+5.1f%%\n", fav_roi,    out_roi))
  cat(sprintf("  P&L total (unités) │  %+6.2f  │  %+6.2f\n",sum(na.omit(data$Profit_Favorite)), sum(na.omit(data$Profit_Outsider))))
  cat("\n")
  
  # ── Graphique P&L cumulé ─────────────────────────────────────────
  plot_data <- data %>%
    na.omit() %>% 
    select(Date) %>%
    mutate(
      Match        = row_number(),
      PnL_Favorite = fav_cumul,
      PnL_Outsider = out_cumul
    ) %>%
    pivot_longer(cols = c(PnL_Favorite, PnL_Outsider),
                 names_to  = "Strategie",
                 values_to = "PnL") %>%
    mutate(Strategie = recode(Strategie,
                              PnL_Favorite = "Favori",
                              PnL_Outsider = "Outsider"))
  
  p <- ggplot(plot_data, aes(x = Match, y = PnL, color = Strategie)) +
    geom_line(linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = c("Favori" = "#2196F3", "Outsider" = "#FF5722")) +
    labs(
      title    = paste("P&L cumulé –", tournoi),
      subtitle = sprintf("Favori ROI: %+.1f%%  |  Outsider ROI: %+.1f%%", fav_roi, out_roi),
      x        = "N° de match",
      y        = "P&L cumulé (unités)",
      color    = "Stratégie"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
  
  # ── Retour invisible des données ─────────────────────────────────
  invisible(list(
    data        = data,
    summary = tibble(
      strategie   = c("Favori", "Outsider"),
      n_matchs    = n_matches,
      winrate_pct = c(fav_winrate, out_winrate),
      avg_odd     = c(fav_avg_odd, out_avg_odd),
      roi_pct     = c(fav_roi, out_roi),
      pnl_total   = c(sum(na.omit(data$Profit_Favorite)), sum(na.omit(data$Profit_Outsider)))
    )
  ))
}

# Appel :
result <- simulate_tournament_strategy("Indian Wells",2010,Favorite_Side = "Market")

result$summary  # accéder au tableau récap


