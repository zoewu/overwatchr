### Get API ###########################################

library(jsonlite)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(crosstalk)
library(scales)
root = "https://api.overwatchleague.com"
node = "player"
expand = "stats"
path =  paste(paste(root,node,sep = "/"),
              ifelse(is.null(expand), NULL,paste0("expand=",expand)),sep="?")
flatJSON <- fromJSON(path,simplifyDataFrame=T,flatten=T)
tableJSON <- data.table(flatJSON)

### Trasfer dataset to flat and long file #############

get_heroes_stats <- function(x){
  if(!is.null(x)) unnest(x,stats) %>% select(-id) %>%
    spread(key=name1,value=value,fill=NA)
}

merge_player_info <- function(id){
  vars = c("id","name","homeLocation","familyName","givenName","nationality","teams",
           "attributes.role","stats.tournament_type","stats.season_id")
  get_heroes_stats(tableJSON)
}


tableJSON$row <- 1:nrow(tableJSON)
table_hero <- tableJSON[,rbindlist(stats.heroes, fill = T, id = "row")
                        ][,`:=`(player_name=tableJSON$name[row],hero_row =.I)]


# Get hero stats table
table_hero_stats <- table_hero[,rbindlist(stats, fill = T, id = "hero_row")
                               ][,`:=`(id=NULL,
                                       stats_10m = ifelse(str_detect(name,"_1m"),value*10,value))][]

### Get team info table
table_team <- tableJSON[,rbindlist(teams, fill = T, id = "row")][,player_name:=tableJSON$name[row]]

### Get player name plus hero stats long file
setnames(table_hero,"name","hero_name")
setkey(table_hero,hero_row)
setkey(table_hero_stats,hero_row)
table1 <- table_hero_stats[table_hero,]

### Get hero player info file, uid by player_name
vars <- c("id","player_name","homeLocation","familyName","givenName","nationality","teams",
          "attributes.role","stats.tournament_type","stats.season_id")
table2 <- tableJSON[,c("player_name"):=name][,vars,with=F]


### Merge to get player info and hero info long file
setkey(table2,player_name)
setkey(table1,player_name)
table3 <- table1[table2,]

### Merge with team table to get player info, team info, and hero stats long file
table_stats_by_player_hero <- table3[table_team[,.(player_name,team.id,team.name,team.primaryColor,team.abbreviatedName)],]



### Prepare barchart dataset #############
### Default parameters
played_heroes = unique(na.omit(table_stats_by_player_hero$hero_name))
suppor_heroes = c("brigitte","moira","zenyatta","ana","lucio","mercy")
stats_vars = str_subset(unique(table_stats_by_player_hero$name),"_per_1m")
play_time_min = 3600 #seconds


### Plot barchart #############
library(plotly)
library(ggplot2)
library(data.table)
#scale_x_continuous()
hero = "reinhardt"

### filter minimum play time
setkey(table_stats_by_player_hero,player_name,hero_name)
filter_stats <- table_stats_by_player_hero[
  table_stats_by_player_hero[name=="time_played_total" & stats_10m>play_time_min, player_name,keyby=.(player_name,hero_name)],
  ][name %in% stats_vars,]

### calculate player rank for barchart sorting, by stats name
filter_stats <- filter_stats[,rank:=paste(name,sprintf("%03i", frankv(stats_10m,order=-1,ties.method = "first"))),by=name]
setorder(filter_stats,name,rank)

### set up color parameters
table_team_color <- unique(filter_stats[,c("team.abbreviatedName","team.primaryColor")])
color_str <- paste0("#",table_team_color$team.primaryColor)
names(color_str) <-table_team_color$team.abbreviatedName

#ytitle <- stri_trans_totitle(gsub("_"," ",str_remove(stats_var,"_avg_per_1m")))

filter_stats_plot<- filter_stats[,c("id","player_name","team.abbreviatedName","hero_name","rank","name","stats_10m")]
filter_stats_plot$Stats_name_formatted <- paste0(stri_trans_totitle(gsub("_"," ",str_remove(filter_stats_plot$name,"_avg_per_1m"))),
                                                 "/10 Min")
names(filter_stats_plot) <- c("ID","Player","Team","Hero","Rank","Stats_name","Value","Stats_name_formatted")

sub <- SharedData$new(filter_stats_plot[Hero==hero,],key="Player",group="hero_stats_subset")

# full <- SharedData$new(filter_stats_plot,key="Player",group="hero_stats_subset")
# sub <- SharedData$new(filter_stats_plot[Hero=="winston",],key="Player",group="hero_stats_subset")
# sub2 <-SharedData$new(filter_stats_plot[Hero=="reinhardt",],key="Player",group="hero_stats_subset")

  p <- ggplot(sub,
              aes(x = Rank, y =  Value, fill = Team))+
    geom_col()+
    scale_y_continuous(expand = c(0,0),label=comma)+
    scale_x_discrete(labels=sub$data()[, setNames(as.character(Player), Rank)])+
    xlab("")+ylab("")+
    facet_wrap(~Stats_name_formatted,scales="free",drop=T)+
    ggtitle(paste(stri_trans_totitle(hero),": Stats Average Per 10 Min (Play Time >", round(play_time_min/60),"Min)"))+
    scale_fill_manual(values=color_str)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
          axis.text.y=element_text(size = 8),
          panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),
          panel.spacing.x=unit(0.25, "lines"),
          panel.spacing.y=unit(0.75, "lines"))

sp <- ggplotly(p) %>% config(displayModeBar = F)


sp







p2 <-ggplot(sub2,
            aes(x = Rank, y =  Value, fill = Team))+
  geom_col()+
  scale_y_continuous(expand = c(0,0),label=comma)+
  scale_x_discrete(labels=filter_stats_plot[Hero==hero,][, setNames(as.character(Player), Rank)])+
  xlab("")+ylab("")+
  facet_wrap(~Stats_name,scales="free",drop=T)+
  ggtitle(paste(stri_trans_totitle(hero),": Stats Average Per 10 Min (Play Time >", round(play_time_min/60),"Min)"))+
  scale_fill_manual(values=color_str)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
        axis.text.y=element_text(size = 8),
        panel.grid.major.x=element_blank(),panel.grid.minor=element_blank())


