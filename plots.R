source('~/GitHub/football-data/wrangling.R')

plot_2vars <- function(x_metric = "sotf_per_game", y_metric = "sota_per_game", 
                       chosen_league = "Championship", chosen_season = 2016) {
  
  summary_df <- summary_stats(chosen_league, chosen_season)
  
  relevance_fills <- c("A" = "deepskyblue", "B" = "lightgreen","C" = "khaki2","D" = "firebrick")
  
  p <- summary_df %>% 
    ggplot(aes_string(x = x_metric, y = y_metric)) +
    geom_point(alpha = 0.6, size = 5, pch = 21,
               aes_string(fill = "relevance")) +
    geom_text(aes(label = team), vjust=2, size=3, check_overlap = TRUE) +
    scale_fill_manual(values = relevance_fills) +
    xlab(gsub("_", " ", x_metric)) + ylab(gsub("_", " ", y_metric)) +
    theme(panel.grid.major = element_line(color="gray50", linetype = "dotted"),
          panel.grid.minor = element_line(color="gray80", linetype = "dotted"),
          plot.title = element_text(color="black"),
          axis.title = element_text(color="gray50"),
          axis.text = element_text(color="gray50"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray50")) +
    theme(legend.position="none") +
    ggtitle(paste(gsub("_", " ", x_metric), "vs", gsub("_", " ", y_metric)),
            paste0(chosen_league, " - ", chosen_season-1, "/", chosen_season-2000))
  
  return(p)
}

plot_bar <- function(y_metric = "sotd_per_game", 
                     chosen_league = "Championship", chosen_season = 2016) {
  stats_df <- summary_stats(chosen_league, chosen_season)
  stats_df[ , "order"] <- c(unlist(stats_df[ , y_metric]) %>% as.numeric %>% rank)
  
  relevance_fills <- c("A" = "deepskyblue", "B" = "lightgreen","C" = "khaki2","D" = "firebrick")
  
  p <- stats_df %>% 
    ggplot(aes_string(y = y_metric)) +
    geom_bar(aes(x = reorder(team, order),
                 fill = relevance),
             alpha = 0.6, size = 0.2,
             stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = relevance_fills) +
    xlab("") + ylab("") +
    theme(panel.grid.major = element_line(color="gray80", linetype = "dotted"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color="black"),
          axis.title = element_text(color="gray50"),
          axis.text = element_text(color="gray50"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "gray80")) +
    theme(legend.position="none") +
    ggtitle(gsub("_", " ", y_metric),
            paste0(chosen_league, " - ", chosen_season-1, "/", chosen_season-2000))
  
  return(p)
}

plot_cann <- function(chosen_league = "Championship", chosen_season = 2016) {
  stats_df <- summary_stats(chosen_league, chosen_season) %>% 
    mutate(x_shift = 1) %>%
    arrange(desc(points), (gd))
  
  for (i in 1:12) {
    stats_df[stats_df[1:(nrow(stats_df)-1),"points"] == stats_df[2:nrow(stats_df),"points"] & stats_df[1:(nrow(stats_df)-1),"x_shift"] == stats_df[2:nrow(stats_df),"x_shift"], "x_shift"] %<>% add(1) 
  }
  
  p <- ggplot(aes(x = x_shift, y = points), data = stats_df) +
    geom_text(aes(label = team), size=3.5) +
    xlim(0, max(stats_df$x) + 1) +
    scale_y_continuous(limits  = c(min(stats_df$points), max(stats_df$points)),
                       breaks = seq(0, 100, 5), minor_breaks = seq(1, 100, 1)) +
    ylab("points") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "gray90"),
          panel.grid.minor.y = element_line(colour = "gray90"),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y = element_text(colour= "gray60"), axis.ticks.y = element_blank(),
          axis.line.y = element_line(colour = "black"),
          panel.background = element_blank(), panel.border = element_blank(),
          axis.line.x = element_blank()) +
    theme(legend.position="none") +
    theme(plot.margin = unit(c(0.8,4,0.8,4), "cm")) +
    ggtitle(chosen_league, paste0(chosen_season-1, "/", chosen_season-2000))
  
  return(p)
}

plot_table <- function(chosen_team, chosen_league = "Championship", chosen_season = 2016) {
  summary_df <- summary_stats(chosen_league, chosen_season)
  
  plot_df <- paste0("~/Documents/Champstats/football-data/", chosen_league, "/", chosen_season, ".csv") %>% 
    read.csv(stringsAsFactors=FALSE) %>%
    filter(HomeTeam == chosen_team | AwayTeam == chosen_team) %>%
    mutate(scoreString = paste0(FTHG, "-", FTAG), 
           homeX = ifelse((HomeTeam == chosen_team), 0, 1),
           opposition = ifelse(HomeTeam == chosen_team, AwayTeam, HomeTeam),
           result =  ifelse(HomeTeam == chosen_team, 
                            ifelse(FTR == "H", 1, ifelse(FTR == "A", -1, 0)),
                            ifelse(FTR == "A", 1, ifelse(FTR == "H", -1, 0)))) %>%
    merge(summary_df, 
          by.x = c("opposition"), 
          by.y = c("team"),
          all.y = TRUE)
  
  plot_df$opposition <- factor(plot_df$opposition, levels = summary_df$team[order(-summary_df$position)])
  
  p <- ggplot(aes(x=homeX,y=opposition), data=plot_df) +
    geom_tile(aes(fill=result, alpha = 0.4), colour = "white") +
    scale_fill_gradient2(low = "red", mid = "gray", high = "green") +
    geom_text(aes(label = scoreString), size=5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_line(colour = "black"),
          panel.background = element_blank(), panel.border = element_blank(),
          axis.line.x = element_blank()) +
    theme(legend.position="none") +
    theme(plot.margin = unit(c(1,4,1.2,4), "cm")) +
    ggtitle(paste0(chosen_team, " results"),
            paste0(chosen_league, " - ", chosen_season-1, "/", chosen_season-2000))
  
  return(p)
}
