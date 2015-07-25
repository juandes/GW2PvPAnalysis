require(ggplot2)
require(reshape2)


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
df[5:14][df[5:14] == 'ele'] <- 1
df[5:14][df[5:14] == 'engi'] <- 2
df[5:14][df[5:14] == 'g'] <- 3
df[5:14][df[5:14] == 'mes'] <- 4
df[5:14][df[5:14] == 'necro'] <- 5
df[5:14][df[5:14] == 'ran'] <- 6
df[5:14][df[5:14] == 't'] <- 7
df[5:14][df[5:14] == 'war'] <- 8


#
table(df$map)


qplot(map, data=df, geom="histogram", main = "Histogram of map usage", xlab = "Map", ylab="Count",
      fill=I("blue")) +
  scale_x_discrete(breaks = 1:4, labels = c("1" = "Kyhlo","2" = "Niflhel", "3" = "Foefire",
                                            "4" = "Silent Storm"))

mapPercentage <- prop.table(table(df$map)) #percentage of map

#score and winner
winnerdf <- as.data.frame(table(df$winner))
colnames(winnerdf) <- c("Team", "Frequency")
winnerdf[,1] <- c("Red team", "Blue team")

#winner team score
winner_score <- pmax(df$red_score, df$blue_score)
summary(winner_score)
print(sd(winner_score))

#loser team
loser_score <- pmin(df$red_score, df$blue_score)
loser_score
summary(loser_score)
print(sd(loser_score))


hist(loser_score, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(loser_score), col="blue", lwd=2) # add a density estimate with defaults
lines(density(loser_score, adjust=2), lty="dotted", col="darkgreen", lwd=2) #smoother

score_per_map <- data.frame(match_number = seq(1,119), map = df$map, red_score = df$red_score,
                            blue_score = df$blue_score, winner = df$winner)
wins.per.map <- table(df$map, df$winner)
wins.per.map <- data.frame(map = c('Battle of Kyhlo','Forest of Niflhel',
                                   'Legacy of the Foefire','Temple of the Silent Storm'),
                           red_team = wins.per.map[,1], blue_team = wins.per.map[,2],
                           stringsAsFactors=FALSE)
                                
wins.per.map.percentage <- wins.per.map
wins.per.map.percentage$red.team.win.percent <- wins.per.map$red_team/(wins.per.map$red_team+
                                                                       wins.per.map$blue_team)*100
wins.per.map.percentage$blue.team.win.percent <- wins.per.map$blue_team/(wins.per.map$red_team+
                                                                         wins.per.map$blue_team)*100

winners_and_score <- melt(winners_and_score, id='map')
colnames(winners_and_score) <- c('Map', 'Team', 'Frequency')
ggplot(data=winners_and_score, aes(x=Team, y=Frequency, fill=Team))+
  geom_bar(stat='identity') +
  facet_wrap(~Map) +
  ggtitle("Scores of both teams per map")

df$score.difference <- abs(df$red_score - df$blue_score)

