install.packages("highcharter")
install.packages("plotly")
library(plotly)
library(ggplot2)
library(data.table)

root = "https://api.overwatchleague.com/stats"
stat = "players"
path = paste(root,stat,sep = "/")
flatJSON <- fromJSON(path)
data_overall <- flatJSON$data
name_list <- names(data_overall)
stat_pos <- 6:11
new_name_list <- paste(name_list[stat_pos],"rank",sep="_")
data_table <-as.data.table(data_overall[,-c(1,2)]) 
data_table_rank <-data_table[, (new_name_list):= lapply(.SD,function(x) (x-min(x))/(max(x)-min(x))),.SDcols=name_list[stat_pos]]
data_table_radar <- melt(data_table_rank, id.vars=c(1:3,10),measure.vars=name_list[stat_pos],
                         variable.name="stats_cat",value.name="stats",na.rm=F)

data_table_radar$name <- as.factor(data_table_radar$name)

flex_support_name <- c("AimGod","Bdosin","Boombox","Dogman","Elk","Gido","IZaYaKI","JJONAK",
                       "Kodak","Kyo","Luffy","Neko","RAPEL","Rawkus","Revenge","ryujehong","Shaz","shu",
                       "Twilight","uNKOE","Viol2t")


data_table_gg <- melt(data_table, id.vars=c(1:3,10),measure.vars=name_list[stat_pos],
                         variable.name="stats_cat",value.name="stats",na.rm=F)

stat_name_list <- name_list[stat_pos]
data_table_ordered <- data_table[role=="support",]
data_table_ordered$name <- reorder(data_table_ordered$name,-data_table_ordered[[stat_name_list[1]]])
p <- ggplot(data_table_ordered,
            aes_string(x = "name", y = stat_name_list[1],fill="team"))+
  geom_bar(stat = "identity")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ply <- ggplotly(p) 



