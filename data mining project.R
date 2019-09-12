library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
library(gains)
#data cleaning
salary <- read.csv("/Users/nithin/Documents/neu/summer 2/case study/NBA_season1718_salary.csv")
stats <- read.csv("/Users/nithin/Documents/neu/summer 2/case study/Seasons_stats.csv")
str(stats)
str(salary)
stats <- 
        stats %>% filter(Year >= 2017) %>% 
        select(Year:G, MP, PER, FG:PTS) %>% 
        distinct(Player, .keep_all = TRUE) %>% 
        mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
               RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
               SPG = STL/G) 
stats_salary <- merge(stats, salary, by.x = "Player", by.y = "Player")
names(stats_salary)[40] <- "salary"
stats_salary <- stats_salary[-39]
#correlation check
corrplot(cor(stats_salary %>% 
                     select(salary, MPG:SPG, 
                            Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "number",type = "upper")
stats_salary_cor <- 
        stats_salary %>% 
        select(salary, PPG, MPG, TOPG, RPG, PER, SPG, APG, BPG, Age, PF)
ggpairs(stats_salary_cor)
names(stats_salary)[5] <- "Team"
#data visualization
library(ggplot2)
plot_ly(data = stats_salary, x = ~salary, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
        layout(
                title = "Salary vs Point Per Game",
                xaxis = list(title = "Salary "),
                yaxis = list(title = "Points Per Game")
        )
plot_ly(data = stats_salary, x = ~salary, y = ~PER, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary, big.mark = ","),"$",
                      "<br>PPG: ", round(PER, digits = 3),
                      "<br>Team: ", Team)) %>% 
        layout(
                title = "Salary vs Player efficiency ratio",
                xaxis = list(title = "Salary "),
                yaxis = list(title = "Points Per Game")
        )
stats_salary %>% 
        ggplot(aes(x = salary, y = PPG)) + 
        geom_point() + 
        geom_smooth(method = "lm") 
#stats_salary %>% 
 #       ggplot(aes(x = salary, y = PER)) + 
  #      geom_point() + 
   #     geom_smooth(method = "lm") 
#models creation
# regression
stats_salary_model <- 
        stats_salary %>% select(salary, MPG:SPG, Age, PF, PER)
#simple linear regression model
lm(salary~., data=stats_salary_model)
summary(stats_salary_model)
train.index<-sample(row.names(stats_salary_model), 0.6*dim(stats_salary_model)[1])
valid.index <- setdiff(row.names(stats_salary_model), train.index)
train.df <- stats_salary_model[train.index,]
valid.df <- stats_salary_model[valid.index,]
regression_model <- lm(formula = salary~ PPG + MPG + TOPG, data = stats_salary_model)
regression_model.pred<- predict(regression_model,valid.df[,2:11])
#knn
library(forecast)
library(caret)
#train model with training set
knn.model <- knnreg(train.df[,c(3,2,6)], train.df[,1], k = 11)
#make prediction for validation set
knn.model.pred <- predict(knn.model,valid.df[,c(3,2,6)])
#random forest
library(randomForest)
## random forest
rf.model <- randomForest(salary ~ PPG + MPG + TOPG, data = train.df, ntree = 500, 
                         mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf.model, type = 1)
#neural network
library(neuralnet)
neural.model<-neuralnet(salary~PPG + MPG + TOPG,data=train.df,linear.output = F, hidden = c(12,9))
plot(neural.model,rep="best")
neural.model.pred<-compute(neural.model,valid.df[,c(2:11)])#for prediction
neural.model.pred$net.result[1:10]


#correlation and accuracy
#regression
#check accuracy
accuracy.reg<-accuracy(valid.df[,1], regression_model.pred)
#check correlation
cor.reg<-cor(regression_model.pred, valid.df[,1])
rss.reg <- sum((regression_model.pred - valid.df[,1]) ^ 2)  ## residual sum of squares
tss.reg <- sum((train.df[,1] - mean(train.df[,1])) ^ 2)  ## total sum of squares
rsq.reg <- 1 - rss.reg/tss.reg
#knn
#check accuracy
accuracy.knn<-accuracy(valid.df[,1], knn.model.pred)
#check correlation
cor.knn<-cor(knn.model.pred, valid.df[,1])
rss.knn <- sum((knn.model.pred - valid.df[,1]) ^ 2)  ## residual sum of squares
tss.knn <- sum((train.df[,1] - mean(train.df[,1])) ^ 2)  ## total sum of squares
rsq.knn <- 1 - rss.knn/tss.knn
#random forest
## confusion matrix
rf.model.pred <- predict(rf.model, valid.df)
accuracy.rf<-accuracy(valid.df[,1], rf.model.pred)
#check correlation
cor.rf<-cor(rf.model.pred, valid.df[,1])

rss.rf <- sum((rf.model.pred - valid.df[,1]) ^ 2)  ## residual sum of squares
tss.rf <- sum((train.df[,1] - mean(train.df[,1])) ^ 2)  ## total sum of squares
rsq.rf <- 1 - rss.rf/tss.rf
#neural networks
cor.nn<-cor(valid.df$salary,neural.model.pred$net.result)
accuracy.nn<-accuracy(valid.df$salary,neural.model.pred$net.result)

#ERROR analysis
#lift charts
#regression
gain.reg <- gains(valid.df$salary[!is.na(regression_model.pred)], regression_model.pred[!is.na(regression_model.pred)])
options(scipen=999)
salary.fut <- valid.df$salary[!is.na(valid.df$salary)]
par(pty="s")
plot(c(0,gain.reg$cume.pct.of.total*sum(salary.fut)/1000000)~c(0,gain.reg$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(valid.df$salary[order(regression_model.pred, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)
#knn
gain.knn <- gains(valid.df$salary[!is.na(knn.model.pred)], knn.model.pred[!is.na(knn.model.pred)])
options(scipen=999)
salary.fut <- valid.df$salary[!is.na(valid.df$salary)]

par(pty="s")
plot(c(0,gain.knn$cume.pct.of.total*sum(salary.fut)/1000000)~c(0,gain.knn$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(valid.df$salary[order(knn.model.pred, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)
#random forest
gain.rf <- gains(valid.df$salary[!is.na(rf.model.pred)],rf.model.pred[!is.na(rf.model.pred)])
options(scipen=999)
salary.fut <- valid.df$salary[!is.na(valid.df$salary)]

par(pty="s")
plot(c(0,gain.rf$cume.pct.of.total*sum(salary.fut)/1000000)~c(0,gain.rf$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(valid.df$salary[order(rf.model.pred, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Expenses (Million)", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(salary.fut)/1000000)~c(0,dim(valid.df)[1]), col = "gray", lty = 2)

#So comparing"
#the correlations between the dependent and independent variables we find most correlation in Random Forest which is "0.8265"
#the accuracy: we get least RMSE in Random Forest "4779607"
#in lift charts a better model for random forest.
#So, we choose Random Forset as our Prediction Model.

#Now we create a fuction for calculating the salary for desired player
salary_prediction <- function(m, point, minutes, turn_over){
        pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over))
        msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
        print(msg)}
#Below i have calculated for the player James Harden
salary_prediction(rf.model, 30.4, 35.4, 4.4)