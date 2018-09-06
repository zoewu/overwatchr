install.packages("tidyverse")
library(jsonlite)

# refernce: https://www.reddit.com/r/Competitiveoverwatch/comments/8zmpyr/api_overwatch_league_is_anyone_creating_a_forum/
?jsonlite
root = "https://api.overwatchleague.com/stats"
stat = "players"
path = paste(root,stat,sep = "/")
flatJSON <- fromJSON(path)

