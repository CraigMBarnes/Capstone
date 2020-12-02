# Load libraries
library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(plyr)
#
timein <- Sys.time()
#
y2015 <- "https://www.pro-football-reference.com/years/2015/games.htm"
y2016 <- "https://www.pro-football-reference.com/years/2016/games.htm"
y2017 <- "https://www.pro-football-reference.com/years/2017/games.htm"
y2018 <- "https://www.pro-football-reference.com/years/2018/games.htm"
y2019 <- "https://www.pro-football-reference.com/years/2019/games.htm"
y2020 <- "https://www.pro-football-reference.com/years/2020/games.htm"
#
L <- list(y2015,y2016,y2017,y2018,y2019,y2020)
#
pullgames <- function(url){
  a <- read_html(url)
  b <- html_nodes(a, 'td') %>% html_nodes('a') 
  c <- as.data.frame(as.character(b[]))
  colnames(c) <- c("linkName")
  d <- separate(data = c, col = linkName, sep = "\"", into = c("x1","x2"))
  e <- d %>% filter( grepl("/boxscores/",x2 )) %>% select(x2)
  e
}
#
df <- sapply(L,pullgames)
df1 <- rbind(as.data.frame(df[1]),as.data.frame(df[2]),as.data.frame(df[3]),as.data.frame(df[4]),as.data.frame(df[5]),as.data.frame(df[6]))
#
df1$fullURL <- paste("https://www.pro-football-reference.com",df1$x2,sep="")
#
dfx <- df1 %>% select(fullURL)
#
boxscore <- function(y){
  t <- as.data.frame(readLines(con = y))
  colnames(t) <- c('x1')
  #
  final <- t$x1[14] # pts for/vs
  final <- gsub('    <meta name=\"Description\" content=\"',"",final)
  final <- gsub('- Full team and player stats and box score\">','',final)
  finals <- as.data.frame(final)
  finals <- gsub(','," ",finals)
  finals <- gsub(' at ',",",finals)
  finals <- gsub(' vs. ',",",finals)
  finals <- gsub(' on ',",",finals) %>% as.data.frame()
  finalscore <- separate(data = finals, col = ".", sep = ",", into = c("x1","x2","x3"))
  colnames(finalscore) <- c("away", "home", "date")
  #
  d1 <- filter(t,grepl(as.character('Vegas Line'),x1))
  d1 <- gsub('"',"",d1)
  d1 <- gsub("<tr ><th scope=row class=center  data-stat=info >","",d1)
  d1 <- gsub("</th><td class=center  data-stat=stat >"," ",d1)
  d1 <- gsub("</td></tr>","",d1)
  d1 <- gsub("Vegas Line ","",d1) %>% as.data.frame()
  d1 <- gsub("Pick","0",d1) %>% as.data.frame()
  colnames(d1) <- c("VegasLine")
  #
  d2 <- filter(t,grepl(as.character('Over/Under'),x1))
  d2 <- gsub('"',"",d2)
  d2 <- gsub("<tr ><th scope=row class=center  data-stat=info >","",d2)
  d2 <- gsub("</th><td class=center  data-stat=stat >"," ",d2)
  d2 <- gsub("<b>","",d2)
  d2 <- gsub("</b></td></tr>","",d2)
  d2 <- gsub("Over/Under ","",d2) %>% as.data.frame()
  d2 <- gsub("(over)","",d2) %>% as.data.frame()
  d2 <- gsub("(under)","",d2) %>% as.data.frame()
  d2 <- gsub(" push","",d2) %>% as.data.frame()
  d2 <- gsub("\\(","",d2) %>% as.data.frame()
  d2 <- gsub("\\)","",d2) %>% as.data.frame()
  colnames(d2) <- c("OverUnder")
  #
  d3 <- filter(t,grepl(as.character('>First Downs<'),x1))
  d3 <- gsub('"',"",d3)
  d3 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d3)
  d3 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d3)
  d3 <- gsub('</td><td class=center  data-stat=home_stat >',",",d3)
  d3 <- gsub('</td></tr>',"",d3)
  d3 <- gsub('First Downs,',"",d3) %>% as.data.frame()
  d3x <- separate(data = d3, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d3x) <- c("FirstDowns_Away","FirstDowns_Home")
  #
  d4 <- filter(t,grepl(as.character('>Rush-Yds-TDs<'),x1))
  d4 <- gsub('"',"",d4)
  d4 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d4)
  d4 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d4)
  d4 <- gsub('</td><td class=center  data-stat=home_stat >',",",d4)
  d4 <- gsub('</td></tr>',"",d4)
  d4 <- gsub('Rush-Yds-TDs,',"",d4) %>% as.data.frame()
  d4x <- separate(data = d4, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d4x) <- c("Rushes_RushYards_RushTDs_Away","Rushes_RushYards_RushTDs_Home")
  #
  d5 <- filter(t,grepl(as.character('>Cmp-Att-Yd-TD-INT<'),x1))
  d5 <- gsub('"',"",d5)
  d5 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d5)
  d5 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d5)
  d5 <- gsub('</td><td class=center  data-stat=home_stat >',",",d5)
  d5 <- gsub('</td></tr>',"",d5)
  d5 <- gsub('Cmp-Att-Yd-TD-INT,',"",d5) %>% as.data.frame()
  d5x <- separate(data = d5, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d5x) <- c("Completions_PassYards_PassTDs_INTs_Away","Completions_PassYards_PassTDs_INTs_Home")
  #
  d6 <- filter(t,grepl(as.character('>Sacked-Yards<'),x1))
  d6 <- gsub('"',"",d6)
  d6 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d6)
  d6 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d6)
  d6 <- gsub('</td><td class=center  data-stat=home_stat >',",",d6)
  d6 <- gsub('</td></tr>',"",d6)
  d6 <- gsub('Sacked-Yards,',"",d6) %>% as.data.frame()
  d6x <- separate(data = d6, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d6x) <- c("Sacks_Yards_Away","Sacks_Yards_Home")
  #
  d7 <- filter(t,grepl(as.character('>Fumbles-Lost<'),x1))
  d7 <- gsub('"',"",d7)
  d7 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d7)
  d7 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d7)
  d7 <- gsub('</td><td class=center  data-stat=home_stat >',",",d7)
  d7 <- gsub('</td></tr>',"",d7) 
  d7 <- gsub('Fumbles-Lost,',"",d7) %>% as.data.frame()
  d7x <- separate(data = d7, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d7x) <- c("Fumbles_Lost_Away","Fumbles_Lost_Home")
  #
  d8 <- filter(t,grepl(as.character('>Turnovers<'),x1))
  d8 <- gsub('"',"",d8)
  d8 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d8)
  d8 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d8)
  d8 <- gsub('</th><td class=center iz data-stat=vis_stat >',",",d8)
  d8 <- gsub('</td><td class=center iz data-stat=home_stat >',",",d8)
  d8 <- gsub('</td><td class=center  data-stat=home_stat >',",",d8)
  d8 <- gsub('</td></tr>',"",d8)
  d8 <- gsub('Turnovers,',"",d8) %>% as.data.frame()
  d8x <- separate(data = d8, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d8x) <- c("Turnovers_Away","Turnovers_Home")
  #
  d9 <- filter(t,grepl(as.character('>Penalties-Yards<'),x1))
  d9 <- gsub('"',"",d9)
  d9 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d9)
  d9 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d9)
  d9 <- gsub('</td><td class=center  data-stat=home_stat >',",",d9)
  d9 <- gsub('</td></tr>',"",d9)
  d9 <- gsub('Penalties-Yards,',"",d9) %>% as.data.frame()
  d9x <- separate(data = d9, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d9x) <- c("Penalties_Yards_Away","Penalties_Yards_Home")
  #
  d10 <- filter(t,grepl(as.character('>Third Down Conv.<'),x1))
  d10 <- gsub('"',"",d10)
  d10 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d10)
  d10 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d10)
  d10 <- gsub('</td><td class=center  data-stat=home_stat >',",",d10)
  d10 <- gsub('</td></tr>',"",d10)
  d10 <- gsub('Third Down Conv.,',"",d10) %>% as.data.frame()
  d10x <- separate(data = d10, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d10x) <- c("ThirdDowns_Away","ThirdDowns_Home")
  #
  d11 <- filter(t,grepl(as.character('>Time of Possession<'),x1))
  d11 <- gsub('"',"",d11)
  d11 <- gsub('<tr ><th scope=row class=right  data-stat=stat >',"",d11)
  d11 <- gsub('</th><td class=center  data-stat=vis_stat >',",",d11)
  d11 <- gsub('</td><td class=center  data-stat=home_stat >',",",d11)
  d11 <- gsub('</td></tr>',"",d11)
  d11 <- gsub('Time of Possession,',"",d11) %>% as.data.frame()
  d11x <- separate(data = d11, col = ".", sep = ",", into = c("x1","x2"))
  colnames(d11x) <- c("TOP_Away","TOP_Home")
  #
  d13 <- t %>% filter(grepl(as.character('xpm'),x1) & grepl(as.character("<tr >"),x1))
  d13$x1 <- gsub('"',"",d13$x1)
  series = paste("x",seq(1,100,1),sep="")
  d13f <- separate(data = d13, col = x1, sep = ">", into =c(series))
  d13fx <- d13f %>% select(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
  d13fx$x7 <- gsub('</td',"",d13fx$x7)
  #
  away <- d13fx$x7[1]
  #
  i = 2
  while (i <= nrow(d13fx)){
    d13fx$x7[i] <- ifelse(d13fx$x7[i]==d13fx$x7[1],"away","home")
    i=i+1
  }
  d13fx$x7[1] <- "away"
  d13fx$x8 <- gsub('<td class=right iz data-stat=',"",d13fx$x8)
  d13fx$x8 <- gsub('<td class=right  data-stat=',"",d13fx$x8)
  d13fx$x9 <- gsub('</td',"",d13fx$x9)
  d13fx$x10 <- gsub('<td class=right iz data-stat=',"",d13fx$x10)
  d13fx$x10 <- gsub('<td class=right  data-stat=',"",d13fx$x10)
  d13fx$x11 <- gsub('</td',"",d13fx$x11)
  d13fx$x12 <- gsub('<td class=right iz data-stat=',"",d13fx$x12)
  d13fx$x12 <- gsub('<td class=right  data-stat=',"",d13fx$x12)
  d13fx$x13 <- gsub('</td',"",d13fx$x13)
  d13fx$x14 <- gsub('<td class=right iz data-stat=',"",d13fx$x14)
  d13fx$x14 <- gsub('<td class=right  data-stat=',"",d13fx$x14)
  d13fx$x15 <- gsub('</td',"",d13fx$x15)
  d13fx$x16 <- gsub('<td class=right iz data-stat=',"",d13fx$x16)
  d13fx$x16 <- gsub('<td class=right  data-stat=',"",d13fx$x16)
  d13fx$x17 <- gsub('</td',"",d13fx$x17)
  d13fx$x18 <- gsub('<td class=right iz data-stat=',"",d13fx$x18)
  d13fx$x18 <- gsub('<td class=right  data-stat=',"",d13fx$x18)
  d13fx$x19 <- gsub('</td',"",d13fx$x19)
  d13fx$x20 <- gsub('<td class=right iz data-stat=',"",d13fx$x20)
  d13fx$x20 <- gsub('<td class=right  data-stat=',"",d13fx$x20)
  d13fx$x21 <- gsub('</td',"",d13fx$x21)
  d13fx$x22 <- gsub('<td class=right iz data-stat=',"",d13fx$x22)
  d13fx$x22 <- gsub('<td class=right  data-stat=',"",d13fx$x22)
  d13fx$x23 <- gsub('</td',"",d13fx$x23)
  cols2 <- c("team",d13fx[1,2],d13fx[1,4],d13fx[1,6],d13fx[1,8],d13fx[1,10],d13fx[1,12],d13fx[1,14],d13fx[1,16])
  d13z <- d13fx %>% select(1,3,5,7,9,11,13,15,17)
  i = 2
  while (i <= ncol(d13z)){
    d13z[,i] <- as.numeric(d13z[,i])
    i = i+1
  }
  cols2 <- trimws(cols2, which = "right")
  colnames(d13z) <- cols2
  d13z[is.na(d13z)] <- 0
  d13a <- d13z %>% pivot_wider(names_from = team, values_from = c("xpm","xpa","fgm","fga","punt","punt_yds","punt_yds_per_punt","punt_long"), values_fn = sum) %>% as.data.frame(.)
  #
  d12 <- t %>% filter(grepl(as.character('kick_ret'),x1) & grepl(as.character("<tr >"),x1))
  d12$x1 <- gsub('"',"",d12$x1)
  series = paste("x",seq(1,100,1),sep="")
  d12f <- separate(data = d12, col = x1, sep = ">", into =c(series))
  d12fx <- d12f %>% select(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
  d12fx$x7 <- gsub('</td',"",d12fx$x7)
  i = 1
  while (i <= nrow(d12fx)){
    d12fx$x7[i] <- ifelse(d12fx$x7[i]==away,"away","home")
    i=i+1
  }
  d12fx$x8 <- gsub('<td class=right iz data-stat=',"",d12fx$x8)
  d12fx$x8 <- gsub('<td class=right  data-stat=',"",d12fx$x8)
  d12fx$x9 <- gsub('</td',"",d12fx$x9)
  d12fx$x10 <- gsub('<td class=right iz data-stat=',"",d12fx$x10)
  d12fx$x10 <- gsub('<td class=right  data-stat=',"",d12fx$x10)
  d12fx$x11 <- gsub('</td',"",d12fx$x11)
  d12fx$x12 <- gsub('<td class=right iz data-stat=',"",d12fx$x12)
  d12fx$x12 <- gsub('<td class=right  data-stat=',"",d12fx$x12)
  d12fx$x13 <- gsub('</td',"",d12fx$x13)
  d12fx$x14 <- gsub('<td class=right iz data-stat=',"",d12fx$x14)
  d12fx$x14 <- gsub('<td class=right  data-stat=',"",d12fx$x14)
  d12fx$x15 <- gsub('</td',"",d12fx$x15)
  d12fx$x16 <- gsub('<td class=right iz data-stat=',"",d12fx$x16)
  d12fx$x16 <- gsub('<td class=right  data-stat=',"",d12fx$x16)
  d12fx$x17 <- gsub('</td',"",d12fx$x17)
  d12fx$x18 <- gsub('<td class=right iz data-stat=',"",d12fx$x18)
  d12fx$x18 <- gsub('<td class=right  data-stat=',"",d12fx$x18)
  d12fx$x19 <- gsub('</td',"",d12fx$x19)
  d12fx$x20 <- gsub('<td class=right iz data-stat=',"",d12fx$x20)
  d12fx$x20 <- gsub('<td class=right  data-stat=',"",d12fx$x20)
  d12fx$x21 <- gsub('</td',"",d12fx$x21)
  d12fx$x22 <- gsub('<td class=right iz data-stat=',"",d12fx$x22)
  d12fx$x22 <- gsub('<td class=right  data-stat=',"",d12fx$x22)
  d12fx$x23 <- gsub('</td',"",d12fx$x23)
  d12fx$x24 <- gsub('<td class=right iz data-stat=',"",d12fx$x24)
  d12fx$x24 <- gsub('<td class=right  data-stat=',"",d12fx$x24)
  d12fx$x25 <- gsub('</td',"",d12fx$x25)
  d12fx$x26 <- gsub('<td class=right iz data-stat=',"",d12fx$x26)
  d12fx$x26 <- gsub('<td class=right  data-stat=',"",d12fx$x26)
  d12fx$x27 <- gsub('</td',"",d12fx$x27)
  cols1 <- c("team",d12fx[1,2],d12fx[1,4],d12fx[1,6],d12fx[1,8],d12fx[1,10],d12fx[1,12],d12fx[1,14],d12fx[1,16],d12fx[1,18],d12fx[1,20])
  cols1 <- trimws(cols1, which = "right")
  d12z <- d12fx %>% select(1,3,5,7,9,11,13,15,17,19,21)
  colnames(d12z) <- cols1
  i = 2
  while (i <= ncol(d12z)){
    d12z[,i] <- as.numeric(d12z[,i])
    i = i+1
  }
  d12z[is.na(d12z)] <- 0
  d12a <- d12z %>% pivot_wider(names_from = team, values_from = c("kick_ret","kick_ret_yds","kick_ret_yds_per_ret","kick_ret_td","kick_ret_long","punt_ret","punt_ret_yds","punt_ret_yds_per_ret","punt_ret_td","punt_ret_long"), values_fn = sum) %>% as.data.frame(.)
  #
  dummyvals <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  dummycols <- c("kick_ret_home","kick_ret_yds_home","kick_ret_yds_per_ret_home","kick_ret_td_home","kick_ret_long_home","punt_ret_home","punt_ret_yds_home","punt_ret_yds_per_ret_home","punt_ret_td_home","punt_ret_long_home","kick_ret_away","kick_ret_yds_away","kick_ret_yds_per_ret_away","kick_ret_td_away","kick_ret_long_away","punt_ret_away","punt_ret_yds_away","punt_ret_yds_per_ret_away","punt_ret_td_away","punt_ret_long_away")
  dummy <- data.frame(t(dummyvals))
  colnames(dummy) <- dummycols
  d12tx <- rbind.fill(dummy,d12a) %>% mutate_all(., ~if_else(is.na(.), 0, .)) %>% colSums(.) %>% t(.)#%>% colSums(.) #%>% as.data.frame(t(.))

  x <- data.frame(finalscore, d1, d2, d3x, d4x, d5x, d6x, d7x, d8x, d9x, d10x, d11x, d12tx, d13a)
  #
  x
}
#
finaldf <- data.frame()
i = 1
while(i<=nrow(dfx)){
  finaldf <- rbind(finaldf,boxscore(dfx$fullURL[i]))
  i = i+1
}
#
write.csv(finaldf,"NFLdf.csv",row.names=F)
#
timeout <- Sys.time()
amountoftime <- timeout - timein
amountoftime
# Time difference of 14.53 mins 12/1/2020
# clear R environment
# rm(list = ls(all.names = TRUE))
# last run on 12/1/2020