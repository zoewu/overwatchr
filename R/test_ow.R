library(jsonlite)
namelist_raw<- unlist(fromJSON("http://overwatch.gamepedia.com//api.php?action=categorytree&format=json&category=Heroes"))


library(stringr)
pattern <- '(?<=Category:)(.*)(?=\")'
namelist <- unlist(str_extract_all(namelist_raw,pattern))
get_data <- function(name){
  cat(paste(name,"\n"))
  title <- name
  ability_path <- paste0("http://overwatch.gamepedia.com//api.php?action=parse&format=json&page=",title,"&prop=wikitext")
  raw <- unlist(fromJSON(ability_path))[[3]]
  pattern1 <- stringr::regex('== *Abilities *==(.*?)(?=\n==)',dotall = T)
  ability_raw <- unlist(str_split(str_extract(raw,pattern1),"\\}\\} *\\n\\{\\{"))
  ability_list <- lapply(ability_raw, function(x) {
     m <- str_split(str_replace(unlist(str_split(x,"\\n")),"\\| ","")," *= *",simplify = T)
     data.frame(param=m[,1],full=m[,2],ability=m[str_detect(m[,1],"ability_name"),2],
                ability_type=m[str_detect(m[,1],"ability_type"),2])
    })
  ability <- do.call(rbind,ability_list)
  ability$name <- name
  
  return(ability)
}

test <- lapply(namelist,get_data)
big <- do.call(rbind,test)

valuepath <- "\\b[+-]*[[:digit:]]+\\.*[[:digit:]]*\\b"
value <- str_extract(m[,2],valuepath)
unit <-str_match(m[,2],"\\b[+-]*[[:digit:]]+\\.*[[:digit:]]* +(\\w+)\\b")[,2]