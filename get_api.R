# method to get api

library(jsonlite)
library(data.table)
library(dplyr)
library(tidyr)
root = "https://api.overwatchleague.com"
node = "player"
expand = "stats"
path =  paste(paste(root,node,sep = "/"),
              ifelse(is.null(expand), NULL,paste0("expand=",expand)),sep="?")
flatJSON <- fromJSON(path,simplifyDataFrame=T,flatten=T)
tableJSON <- data.table(flatJSON)

get_heroes_stats <- function(x){
    if(!is.null(x)) unnest(x,stats) %>% select(-id) %>%
      spread(key=name1,value=value,fill=NA)
}

merge_player_info <- function(id){
  vars = c("id","name","homeLocation","familyName","givenName","nationality","teams",
           "attributes.role","stats.tournament_type","stats.season_id")
  get_heroes_stats(tableJSON)
}

#test2<- do.call(rbind,lapply(tableJSON$stats.heroes,get_heroes_stats)) 

tableJSON$row <- 1:nrow(tableJSON)
table_hero <- tableJSON[,rbindlist(stats.heroes, fill = T, id = "row")][,player_name:=tableJSON$name[row]]
table_hero_stats <- table_hero[,rbindlist(stats, fill = T, id = "hero_row")] %>% select(-id) %>%
                    spread(key=name,value=value,fill=NA)
table_team <- tableJSON[,rbindlist(teams, fill = T, id = "row")][,player_name:=tableJSON$name[row]]
vars <- c("id","player_name","homeLocation","familyName","givenName","nationality","teams",
         "attributes.role","stats.tournament_type","stats.season_id")
table1 <- cbind(table_hero,table_hero_stats)
table2 <- tableJSON[,c("player_name"):=name][,vars,with=F]
setkey(table2,player_name)
table3 <- left_join(table2,table1,by="player_name")
table_stats_by_player_hero <- left_join(
  table3,table_team[,.(player_name,team.id,team.name,team.primaryColor,team.abbreviatedName)],on="player_name")

                                     