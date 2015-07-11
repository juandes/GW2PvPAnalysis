setwd("~/Development/R/GW2PvPAnalysis")
df <- read.csv("~/Development/R/GW2PvPAnalysis/GW2_PvP_data.csv", stringsAsFactors=FALSE)

#
df$X <- NULL
df$X.1 <- NULL
df$X.2 <- NULL
colnames(df) <- c('map', 'red_score', 'blue_score', 'winner', 'red_1', 'red_2', 'red_3',
                  'red_4', 'red_5', 'blue_1', 'blue_2', 'blue_3', 'blue_4', 'blue_5')
df$winner[df$red_score > df$blue_score] <- 0
df$winner[df$red_score < df$blue_score] <- 1
df$RED_1[df$RED_1 == 'ran'] <- 6
