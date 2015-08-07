require(ggplot2)
require(reshape2)
require(plyr)
require(dplyr)
require(randomForest)
require(e1071)
set.seed(3392)


# Load dataset and clean it

setwd("~/Development/R/GW2PvPAnalysis")
df <- read.csv("~/Development/R/GW2PvPAnalysis/GW2_PvP_data.csv", 
               stringsAsFactors=FALSE)

#Remove unnecessary columns
df$X <- NULL
df$X.1 <- NULL
df$X.2 <- NULL

#Rename columns
colnames(df) <- c("map", "red.score", "blue.score", "winner", "red_1", "red_2", 
                  "red_3", "red_4", "red_5", "blue_1", "blue_2", "blue_3", "blue_4",
                  "blue_5")

#If the red team won, the value is 0, otherwise 1
df$winner[df$red.score > df$blue.score] <- 0
df$winner[df$red.score < df$blue.score] <- 1

#Change the names of the classes for numbers
df[5:14][df[5:14] == "ele"] <- 1
df[5:14][df[5:14] == "engi"] <- 2
df[5:14][df[5:14] == "g"] <- 3
df[5:14][df[5:14] == "mes"] <- 4
df[5:14][df[5:14] == "necro"] <- 5
df[5:14][df[5:14] == "ran"] <- 6
df[5:14][df[5:14] == "t"] <- 7
df[5:14][df[5:14] == "war"] <- 8


# The maps section

# Create frequency table of map usage
map.frequency.percentage.table <- as.data.frame(table(df$map))
map.frequency.percentage.table <- cbind(map.frequency.percentage.table,
                                        sapply(as.data.frame(prop.table(table(df$map)))[,2],
                                               function(x) round(x * 100, 2)))
colnames(map.frequency.percentage.table) <- c("Map", "Frequency", "Percentage")
rownames(map.frequency.percentage.table) <- c("Battle of Kyhlo", 
                                              "Forest of Niflhel",
                                              "Legacy of the Foefire", 
                                              "Temple of the Silent Storm")
map.frequency.percentage.table

# Histogram of map usage
qplot(map, data = df, geom = "histogram", main = "Histogram of map usage", xlab = "Map",
      ylab = "Frequency", fill = I("blue")) +
  scale_x_discrete(breaks = 1:4, labels = c("1" = "Kyhlo", "2" = "Niflhel",
                                            "3" = "Foefire", "4" = "Silent Storm"))

# Overall view of the score and result

winnerdf <- as.data.frame(table(df$winner))
colnames(winnerdf) <- c("Team", "Frequency")
winnerdf[,1] <- c("Red team", "Blue team")
winnerdf

wins.per.map <- table(df$map, df$winner)

# Create data frame with the teams, their frequency of victories and percentage
wins.per.map <- data.frame(map = c("Battle of Kyhlo","Forest of Niflhel",
                                   "Legacy of the Foefire","Temple of the Silent Storm"),
                           red.team = wins.per.map[,1], blue.team = wins.per.map[,2],
                           stringsAsFactors=FALSE)

wins.per.map.percentage <- wins.per.map
wins.per.map.percentage$red.team.win.percent <-
  round(wins.per.map$red.team / (wins.per.map$red.team + wins.per.map$blue.team)*100, 2)
wins.per.map.percentage$blue.team.win.percent <-
  round(wins.per.map$blue.team / (wins.per.map$red.team + wins.per.map$blue.team)*100, 2)
colnames(wins.per.map.percentage) <- c("Map", "Red team victories", "Blue team victories",
                                       "Red team win %", "Blue team win %")
wins.per.map.percentage

# Final score differences section

df$score.difference <- abs(df$red.score - df$blue.score)
score.difference.summary <- summary(df$score.difference)
score.difference.summary

# Score difference across all the matches section

hist(df$score.difference, prob = TRUE, main = "Gap between final scores (overall)",
     xlab = "Score difference")
rug(df$score.difference)
lines(density(df$score.difference), col="blue", lwd = 2)
lines(density(df$score.difference, adjust = 2), lty="dotted", col="darkgreen",
      lwd = 2)

# Score difference per map
# Battle of Kyhlo
hist(filter(df, map == 1)$score.difference, prob = TRUE,
     main = "Gap between final scores (Battle of Kyhlo)", 
     xlab = "Score difference")
rug(filter(df, map == 1)$score.difference)
lines(density(df$score.difference), col="blue", lwd = 2)
lines(density(df$score.difference, adjust = 2), lty="dotted", col="darkgreen", 
      lwd = 2)

# Forest of Niflhel
hist(filter(df, map == 2)$score.difference, prob = TRUE,
     main = "Gap between final scores (Forest of Niflhel)",
     xlab = "Score difference")
rug(filter(df, map == 2)$score.difference)
lines(density(df$score.difference), col="blue", lwd = 2)
lines(density(df$score.difference, adjust = 2), lty="dotted", col="darkgreen",
      lwd = 2)

# Legacy of the Foefire
hist(filter(df, map == 3)$score.difference, prob = TRUE,
     main = "Gap between final scores (Legacy of the Foefire)", 
     xlab = "Score difference")
rug(filter(df, map == 3)$score.difference)
lines(density(df$score.difference), col="blue", lwd = 2)
lines(density(df$score.difference, adjust = 2), lty="dotted", col="darkgreen",
      lwd = 2)

# Temple of the Silent Storm
hist(filter(df, map == 4)$score.difference, prob = TRUE,
     main = "Gap between final scores (Temple of the Silent Storm)", 
     xlab = "Score difference")
rug(filter(df, map == 4)$score.difference)
lines(density(df$score.difference), col="blue", lwd = 2)
lines(density(df$score.difference, adjust = 2), lty="dotted", col="darkgreen",
      lwd = 2)

# Wins per map section

# Frequency of win per map
winners_and_score <- melt(wins.per.map, id = "map")
colnames(winners_and_score) <- c("Map", "Team", "Frequency")
ggplot(data=winners_and_score, aes(x = Team, y = Frequency, fill = Team)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Map) +
  ggtitle("Wins of both teams per map")

# Scores of both teams
scoresdf <- data.frame(match_number = seq(1,119), red.score = df$red.score, 
                       blue.score = df$blue.score, map = df$map)

qplot(match_number, red.score, data = scoresdf, size = red.score, 
      alpha = red.score, colour = "red", main = "Red team's score",
      xlab = "Match number", ylab = "Score") + theme(legend.position = "none") 

qplot(match_number, blue.score, data = scoresdf, size = blue.score, 
      alpha = blue.score, colour = blue.score, main = "Blue team's score",
      xlab = "Match number", ylab = "Score") + theme(legend.position = "none")

# Final score of both teams
qplot(x = df$red.score, y = df$blue.score, geom = "point", alpha = I(0.5),
      size = I(5), main = "Final score of every match", xlab = "Red score",
      ylab = "Blue score")

# Correlation between the final scores
cor.test(df$red.score, df$blue.score)

# Final scores of the winning team section

# Create a vector with the winner score of each match
winner.score <- pmax(df$red.score, df$blue.score)
summary(winner.score)

# Losers score section
# Create a vector with the score of the defeated team from each match
loser_score <- pmin(df$red.score, df$blue.score)
loser_score
summary(loser_score)
print(sd(loser_score))

# Histogram and boxplot of the defeated team score
hist(loser_score, prob=TRUE, col = "grey")
rug(loser_score)
lines(density(loser_score), col = "blue", lwd=2)
lines(density(loser_score, adjust=2), lty = "dotted", col = "darkgreen", lwd = 2)
boxplot(loser_score, main = "Scores of the defeated team", ylab = "Score")
boxplot.stats(loser_score)$out  # The outliers
boxplot.stats(loser_score)$stats  # Summary of the boxplot

# Composition of the teams section

# What is the frequency and percentage of each professions across the dataset?

# Create a dataset with all the different teams
team.composition <- data.frame(class.1 = numeric(0), class.2 = numeric(0), 
                               class.3 = numeric(0), class.4 = numeric(0), 
                               class.5 = numeric(0))
red.team.composition <- df[5:9]
blue.team.composition <- df[10:14]
colnames(red.team.composition) <- c("class.1", "class.2", "class.3", "class.4", 
                                    "class.5")
colnames(blue.team.composition) <- c("class.1", "class.2", "class.3", "class.4",
                                     "class.5")
# team.composition df has the 238 team compositions
team.composition <- rbind(team.composition, red.team.composition, blue.team.composition)

# Get the frequency of each class
frequency.percentage.classes <- sapply(1:8, function(class.number) 
  sum(sapply(team.composition, function(x) sum(x == class.number))))
frequency.percentage.classes <- as.data.frame(frequency.percentage.classes)
rownames(frequency.percentage.classes) <- c("Elementalist", "Engineer",
                                            "Guardian", "Mesmer",
                                            "Necromancer", "Ranger",
                                            "Thief", "Warrior")
colnames(frequency.percentage.classes) <- c("Frequency")
# Get the percentage of each class
frequency.percentage.classes$Percentage <- 
  round(frequency.percentage.classes$Frequency / 
          (sum(frequency.percentage.classes$Frequency) + 17) * 100, 2)  # 17 missing players
frequency.percentage.classes

# What's the amount of missing players?

frequency.percentage.missing.players <- sum(sapply(team.composition, function(x) sum(x == 0)))
frequency.percentage.missing.players <- as.data.frame(frequency.percentage.missing.players)
rownames(frequency.percentage.missing.players) <- c("Missing players")
colnames(frequency.percentage.missing.players) <- c("Frequency")
frequency.percentage.missing.players$Percentage <-
  round(frequency.percentage.missing.players$Frequency / 
          (sum(frequency.percentage.classes$Frequency) + 17) * 100, 2) # 17 missing players.
frequency.percentage.missing.players

# Is there a repeated team composition?

team.composition.reorder <- data.frame(class.1 = numeric(0), class.2 = numeric(0), 
                                       class.3 = numeric(0), class.4 = numeric(0), 
                                       class.5 = numeric(0))
team.composition.reorder <- as.data.frame(t(apply(team.composition, 1 , sort)),
                                          stringsAsFactors = FALSE)
colnames(team.composition.reorder) <- colnames(team.composition)
frequent.teams <- ddply(team.composition.reorder ,names(team.composition.reorder), summarize, 
                        Frequency = length(class.1))

frequent.teams <- frequent.teams[frequent.teams$Frequency > 1,]
frequent.teams[1:5][frequent.teams[1:5] == 1] <- "Elementalist"
frequent.teams[1:5][frequent.teams[1:5] == 2] <- "Engineer"
frequent.teams[1:5][frequent.teams[1:5] == 3] <- "Guardian"
frequent.teams[1:5][frequent.teams[1:5] == 4] <- "Mesmer"
frequent.teams[1:5][frequent.teams[1:5] == 5] <- "Necromancer"
frequent.teams[1:5][frequent.teams[1:5] == 6] <- "Ranger"
frequent.teams[1:5][frequent.teams[1:5] == 7] <- "Thief"
frequent.teams[1:5][frequent.teams[1:5] == 8] <- "Warrior"

arrange(frequent.teams, desc(Frequency))

# What is the outcome of the match when a team is made of 3 or more of the 
# same profession?

# Find matches that had at least 3 of the same professions
three.or.more <- 
  team.composition[apply(team.composition, 1, function(X) any(table(X) >= 3)),]

scores.for.red <- df$winner  # 1 means red lose, 0 means red won (0 means victory, 1 defeat)
scores.for.blue <- sapply(df$winner, function(x) if (x == 1) { 0 } else { 1 })
scores.for.red <- as.data.frame(scores.for.red)
scores.for.blue <- as.data.frame(scores.for.blue)
colnames(scores.for.red) <- c("Result")
colnames(scores.for.blue) <- c("Result")
team.composition.result <- data.frame(result = numeric(0))
team.composition.result <- 
  rbind(team.composition.result, scores.for.red, scores.for.blue)

# Get the result of the particular matches using the indexes of three.or.more
three.or.more <- cbind(three.or.more, 
                       result = team.composition.result[as.numeric(rownames(three.or.more)),])

# Composition of teams with three or more similar classes that won their matches
three.or.more.victory <- three.or.more[three.or.more$result == 0,]
three.or.more.victory[three.or.more.victory == 1] <- "Elementalist"
three.or.more.victory[three.or.more.victory == 2] <- "Engineer"
three.or.more.victory[three.or.more.victory == 3] <- "Guardian"
three.or.more.victory[three.or.more.victory == 4] <- "Mesmer"
three.or.more.victory[three.or.more.victory == 5] <- "Necromancer"
three.or.more.victory[three.or.more.victory == 6] <- "Ranger"
three.or.more.victory[three.or.more.victory == 7] <- "Thief"
three.or.more.victory[three.or.more.victory == 8] <- "Warrior"
three.or.more.victory

# Is it possible to predict the outcome of a match based on the structure of 
# the team (machine learning section)?

# Prepare the data
team.result <- cbind(team.composition, team.composition.result)
colnames(team.result)[6] <- "result"
team.result$result[team.result$result == 0 ] <- "won"
team.result$result[team.result$result == 1 ] <- "lost"
team.result$result <- as.factor(team.result$result)

# Random forest model
# Create the train and test dataset
index <- sample(2, nrow(team.result), replace=TRUE, prob=c(0.7, 0.3))
train.data <- team.result[index == 1, ]
test.data <- team.result[index == 2, ]

# Create the random forest model and train it.
rf <- randomForest(result ~ ., data = train.data, ntree = 70, proximity = TRUE)
confusion.matrix.rf <- table(predict(rf), train.data$result)
classification.error <- round((confusion.matrix.rf[2, 1] + confusion.matrix.rf[1, 2]) /
                                sum(confusion.matrix.rf) * 100, 2)
rf

# Prediction using the test data
pred <- predict(rf, test.data)
pred.confusion.matrix <- table(pred, test.data$result)
test.classification.error <- round((pred.confusion.matrix[2, 1] + pred.confusion.matrix[1, 2]) /
                                     sum(pred.confusion.matrix) * 100, 2)

# Confusion matrix of the test dataset
pred.confusion.matrix <- cbind(pred.confusion.matrix, 
                               c(pred.confusion.matrix[1, 2] / 
                                   (pred.confusion.matrix[1,1] + pred.confusion.matrix[1,2]), 
                                 pred.confusion.matrix[2, 1] / 
                                   (pred.confusion.matrix[2, 1] + pred.confusion.matrix[2, 2])))
colnames(pred.confusion.matrix) <- c("lost", "won", "class.error")
pred.confusion.matrix

# Naive Bayes classifier model
# Create and train the model
bayes.classifier <- naiveBayes(result ~ ., data = train.data)
train.data <- as.data.frame(cbind(class.1 = as.numeric(train.data$class.1), 
                                  class.2 = as.numeric(train.data$class.2),
                                  class.3 = as.numeric(train.data$class.3), 
                                  class.4 = as.numeric(train.data$class.4),
                                  class.5 = as.numeric(train.data$class.5), 
                                  result = as.numeric(train.data$result)))
pred <- predict(bayes.classifier, train.data)

# Create confusion matrix of the model
bayes.training.confusion.matrix <- table(pred, train.data$result)
bayes.training.confusion.matrix <- 
  cbind(bayes.training.confusion.matrix, 
        c(bayes.training.confusion.matrix[1, 2] /
            (bayes.training.confusion.matrix[1, 1] + bayes.training.confusion.matrix[1, 2]),
          bayes.training.confusion.matrix[2, 1] /
            (bayes.training.confusion.matrix[2, 1] + bayes.training.confusion.matrix[2, 2])))                                       
colnames(bayes.training.confusion.matrix) <- c("lost", "won", "class.error")
bayes.training.confusion.matrix

# Calculate the overall classification error
bayes.training.classification.error <- round((bayes.training.confusion.matrix[2, 1] +
                                                bayes.training.confusion.matrix[1, 2]) /
                                               (bayes.training.confusion.matrix[1,1] +
                                                  bayes.training.confusion.matrix[1,2] +
                                                  bayes.training.confusion.matrix[2,1] +
                                                  bayes.training.confusion.matrix[2,2])
                                             * 100, 2)

# Apply the testing dataset on the model
test.data <- as.data.frame(cbind(class.1 = as.numeric(test.data$class.1), 
                                 class.2 = as.numeric(test.data$class.2),
                                 class.3 = as.numeric(test.data$class.3), 
                                 class.4 = as.numeric(test.data$class.4),
                                 class.5 = as.numeric(test.data$class.5), 
                                 result = as.numeric(test.data$result)))
bayes.prediction <- predict(bayes.classifier, test.data)

# Build the confusion matrix for the testing dataset
bayes.test.confusion.matrix <- table(bayes.prediction, test.data$result)
bayes.test.confusion.matrix <- 
  cbind(bayes.test.confusion.matrix, 
        c(bayes.test.confusion.matrix[1, 2] /
            (bayes.test.confusion.matrix[1, 1] + bayes.test.confusion.matrix[1, 2]),
          bayes.test.confusion.matrix[2, 1] /
            (bayes.test.confusion.matrix[2, 1] + bayes.test.confusion.matrix[2, 2])))
colnames(bayes.test.confusion.matrix) <- c("lost", "won", "class.error")
bayes.test.confusion.matrix

# Calculate the overall classification error
bayes.test.classification.error <- round((bayes.test.confusion.matrix[2, 1] +
                                            bayes.test.confusion.matrix[1, 2]) /
                                           (bayes.test.confusion.matrix[1,1] +
                                              bayes.test.confusion.matrix[1,2] +
                                              bayes.test.confusion.matrix[2,1] +
                                              bayes.test.confusion.matrix[2,2])
                                         * 100, 2)