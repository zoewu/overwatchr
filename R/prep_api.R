install.packages("tidyverse")
install.packages("DT")
library(jsonlite)
library(DT)

# refernce: https://www.reddit.com/r/Competitiveoverwatch/comments/8zmpyr/api_overwatch_league_is_anyone_creating_a_forum/
?jsonlite
root = "https://api.overwatchleague.com/stats"
stat = "players"
path = paste(root,stat,sep = "/")
flatJSON <- fromJSON(path)
data <- flatJSON$data
datatable(data)
data[-c(1,2)]
stringi::stri_trans_totitle(gsub("_"," ",names(data)[-1:2]))
