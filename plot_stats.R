# graphs stats by hero
library(stringr)
library(ggplot2)
library(plotly)
library(data.table)
library(stringi)
library(scales)
library(crosstalk)
played_heroes = unique(na.omit(table_stats_by_player_hero$name))
suppor_heroes = c("brigitte","moira","zenyatta","ana","lucio","mercy")
stats_vars = str_subset(names(table_stats_by_player_hero),"_per_1m")
hero_name = "brigitte"
play_time_min = 600 #seconds
stats_var = "eliminations_avg_per_1m"

get_single_plot <- function(stats_var,hero_name,play_time_min){
  subset_stats <- table_stats_by_player_hero %>% 
    filter(name == hero_name & time_played_total>play_time_min) 
  
  subset_stats$rank <- as.factor(frankv(subset_stats[[stats_var]],order=-1))
  subset_stats$team.primaryColor <- as.factor(subset_stats$team.primaryColor)
  subset_stats_ordered <- arrange(subset_stats,rank)
  table_team_color <- unique(arrange(subset_stats,rank)[c("team.abbreviatedName","team.primaryColor")])
  color_str <- paste0("#",table_team_color$team.primaryColor)
  names(color_str) <-table_team_color$team.abbreviatedName
  
  ytitle <- stri_trans_totitle(gsub("_"," ",str_remove(stats_var,"_avg_per_1m")))
  
  subset_stats_plot<- data.frame(subset_stats_ordered[c("id","player_name","rank","team.abbreviatedName","time_played_total")],
                                 Stats=subset_stats_ordered[[stats_var]]*10)
  names(subset_stats_plot) <- c("ID","Player","Rank","Team","time_played_total","Stats")
  subset_stats_plot$Player <- factor(subset_stats_plot$Player,levels=subset_stats_plot$Player)
  
  p <- ggplot(subset_stats_plot,
              aes_string(x = "Player", y = "Stats",fill="Team"))+
    geom_bar(stat = "identity")+
    scale_y_continuous(expand = c(0,0),label=comma)+
    ylab(ytitle)+ xlab("")+
    ggtitle(paste(stri_trans_totitle(hero_name),": Stats Average Per 10 Min (Play Time >", round(play_time_min/60),"Min)"))+
    scale_fill_manual(values=color_str)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
          axis.text.y=element_text(size = 8),
          panel.grid.major.x=element_blank(),panel.grid.minor=element_blank())
  p
}


get_subplot<- function(hero_name,play_time_min){
  if(hero_name %in% suppor_heroes) {
    stats_vars <- stats_vars
  } else{
    stats_vars <-stats_vars[!str_detect(stats_vars,"heal")]
  }
  ply_hero<-lapply(stats_vars,get_single_plot,hero_name=hero_name,play_time_min=play_time_min)
  
  pll <- lapply(1:length(ply_hero),function(p) {
    
    ply <- ggplotly(ply_hero[[p]]) %>% config(displayModeBar = F) 
    if(p < length(ply_hero)){
      ply<-style(ply, traces = (1:length(ply[["x"]][["data"]])), showlegend = FALSE)
      ply <- hide_legend(ply)
    } 
    return(ply)
  })
  
  sp<-subplot(pll,nrows=3,titleY = TRUE,margin = 0.05) 
}

sp1<-get_subplot("winston",play_time_min=600)
sp1
