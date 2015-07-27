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


hist(loser_score, prob=TRUE, col="grey")
lines(density(loser_score), col="blue", lwd=2)
lines(density(loser_score, adjust=2), lty="dotted", col="darkgreen", lwd=2) #smoother

score_per_map <- data.frame(match_number = seq(1,119), map = df$map, red_score = df$red_score,
                            blue_score = df$blue_score, winner = df$winner)
wins.per.map <- table(df$map, df$winner)
wins.per.map <- data.frame(map = c('Battle of Kyhlo','Forest of Niflhel',
                                   'Legacy of the Foefire','Temple of the Silent Storm'),
                           red_team = wins.per.map[,1], blue_team = wins.per.map[,2],
                           stringsAsFactors=FALSE)
                                
wins.per.map.percentage <- wins.per.map
wins.per.map.percentage$red.team.win.percent <- wins.per.map$red_team / (wins.per.map$red_team +
                                                                       wins.per.map$blue_team)*100
wins.per.map.percentage$blue.team.win.percent <- wins.per.map$blue_team / (wins.per.map$red_team +
                                                                         wins.per.map$blue_team)*100

winners_and_score <- melt(winners_and_score, id='map')
colnames(winners_and_score) <- c('Map', 'Team', 'Frequency')
ggplot(data=winners_and_score, aes(x=Team, y=Frequency, fill=Team))+
  geom_bar(stat='identity') +
  facet_wrap(~Map) +
  ggtitle("Scores of both teams per map")

df$score.difference <- abs(df$red_score - df$blue_score)

#Team composition
team.composition <- data.frame(class.1 = numeric(0), class.2 = numeric(0), 
                               class.3 = numeric(0), class.4 = numeric(0), 
                               class.5 = numeric(0))
red.team.composition <- df[5:9]
blue.team.composition <- df[10:14]
colnames(red.team.composition) <- c("class.1", "class.2", "class.3", "class.4", "class.5")
colnames(blue.team.composition) <- c("class.1", "class.2", "class.3", "class.4", "class.5")
team.composition <- rbind(team.composition, red.team.composition, blue.team.composition)

#ddply(team.composition,names(team.composition),summarize, Freq = length(class.1))

#team.composition[, apply(team.composition, 2, function(X) any((as.numeric(X)) == 1))]


#apply(team.composition, 1, function(X) any(as.numeric(X) == 1))



#Subset those columns where any column has one of those classes
team.composition[apply(team.composition, 1, function(X) any(as.numeric(X) == 2)),] #WORKS!!!


team.composition[apply(team.composition, 1, function(X) any(table(X) >= 3)),]


ddply(team.composition,names(team.composition),summarize, Freq = length(class.1)) # Frequency

#sapply(1:8, function(class.number) nrow(team.composition[apply(team.composition, 1, function(X) any(as.numeric(X) == class.number)),]))
engis <- team.composition[apply(team.composition, 1, function(X) any(as.numeric(X) == 2)),]
#sapply(1:8, function(class) nrow(team.composition[apply(team.composition, 1, function(X) any(as.numeric(X) == class)),]) / nrow(team.composition) * 100)


lol <- sapply(1:8, function(class.number) team.composition[apply(team.composition, 1, function(X) any(as.numeric(X) == class.number)),])
sum(sapply(engis, function(x) sum(x == 2)))



____



scores.for.red <- df$winner # 1 means red lose, 0 means red won (0 means victory, 1 defeat)
scores.for.blue <- sapply(df$winner, function(x) if (x == 1) { 0 } else { 1 })
scores.for.red <- as.data.frame(scores.for.red)
scores.for.blue <- as.data.frame(scores.for.blue)
colnames(scores.for.red) <- c("Result")
colnames(scores.for.blue) <- c("Result")

team.composition.result <- data.frame(result = numeric(0))
team.composition.result <- rbind(team.composition.result, scores.for.red, scores.for.blue)

match.result <- data.frame(Result = numeric(nrow(scores.for.red) * 2))
match.result[1:nrow(scores.for.red), ] <- scores.for.red
match.result[120:238, ] <- scores.for.blue


_____
Machine learning

team.result <- cbind(team.composition, team.composition.result)
colnames(team.result)[6] <- "result"
team.result$result[team.result$result == 0 ] <- "won"
team.result$result[team.result$result == 1 ] <- "lost"
