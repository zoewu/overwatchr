install.packages("tidyverse")
install.packages("DT")
library(jsonlite)
library(DT)
library(data.table)
library(tidyr)


# refernce: https://www.reddit.com/r/Competitiveoverwatch/comments/8zmpyr/api_overwatch_league_is_anyone_creating_a_forum/
?jsonlite
root = "https://api.overwatchleague.com"
stat = "stats/players"
path = paste(root,stat,sep = "/")
flatJSON <- fromJSON(path)
data <- flatJSON$data
datatable(data)

# match info
matches <- fromJSON(paste(root,"matches",sep = "/"),simplifyDataFrame=T,flatten=T)
data_matches <-data.table(matches$content)

competitor_table <- data_matches[,unlist(competitors,recursive=FALSE)]
game_table <-  data_matches[,unlist(games,recursive=FALSE)]

match_id = data_matches$id[1]
map_id = (1:nrow(game_table))[1]
stats_match_path <- paste(root,"stats","matches",match_id,"maps",map_id,sep = "/")
stats_JSON <- fromJSON(stats_match_path)

stats_table <- data.table(stats_JSON$teams$players)

unnest(stats_table) %>% unnest(stats)  unnest(stats_table) %>% unnest(heroes)


str(stats_table$players)
str(stats_JSON)
# match stats



# Match Stats : https://api.overwatchleague.com/stats/matches/#####/maps/#
#   
# Overall Player Stats https://api.overwatchleague.com/stats/players
# 
# Regular Season Player Stats https://api.overwatchleague.com/stats/players?stage_id=regular_season
# 
# Postseason Player Stats https://api.overwatchleague.com/stats/players?stage_id=postseason
# 
# Player stats for a particular season https://api.overwatchleague.com/stats/players?season_id=YYYY
# 
# Player stats for a particular postseason https://api.overwatchleague.com/stats/players?season_id=2017&stage_id=postseason
# 
# Upcoming live matches https://api.overwatchleague.com/live-match?expand=team.content&locale=en-us
# 
# Team data/social information https://api.overwatchleague.com/teams?expand=team.content&locale=en_US
# 
# Team schedule https://api.overwatchleague.com/schedule?expand=team.content&locale=en_US
# 
# Team standings https://api.overwatchleague.com/v2/standings?locale=en_US
# 
# Team rankings https://api.overwatchleague.com/ranking
# 
# Maps https://api.overwatchleague.com/maps

# player hero stats: https://api.overwatchleague.com/players?expand=stats
# single player hero stats: https://api.overwatchleague.com/players/8236?expand=stats
# player hero stats rank: https://api.overwatchleague.com/players/8236?expand=stats.rank