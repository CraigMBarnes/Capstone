time1 <- Sys.time()
# Intro ----
# "Choose your Own: Predicting NFL Games"
# "Craig Barnes"
#
if (!require(rlang)) install.packages('rlang', repos='http://cran.us.r-project.org')
library(rlang)
if (!require(tidyverse)) install.packages('tidyverse', repos='http://cran.us.r-project.org')
library(tidyverse)
if (!require(caret)) install.packages('caret', repos='http://cran.us.r-project.org')
library(caret)
if (!require(knitr)) install.packages('knitr', repos='http://cran.us.r-project.org')
library(knitr)
if (!require(RCurl)) install.packages('RCurl', repos='http://cran.us.r-project.org')
library(RCurl)
if (!require(e1071)) install.packages('e1071', repos='http://cran.us.r-project.org')
library(e1071)
if (!require(xgboost)) install.packages('xgboost', repos='http://cran.us.r-project.org')
library(xgboost)
if (!require(gridExtra)) install.packages('gridExtra', repos='http://cran.us.r-project.org')
library(gridExtra)
if (!require(gam)) install.packages('gam', repos='http://cran.us.r-project.org')
library(gam)
if (!require(randomForest)) install.packages('randomForest', repos='http://cran.us.r-project.org')
library(randomForest)
options("scipen"=100, "digits"=4)
knitr::opts_chunk$set(echo = TRUE)
#
# download data from github
x <- getURL("https://raw.githubusercontent.com/CraigMBarnes/Capstone/main/NFLdf.csv")
df <- read.csv(text = x)
# take a look at first row of observations and their data types
glimpse(df[1:1,])
kable(df[1,1:3])
kable(df[1,4:5])
kable(df[1,6:7])
kable(df[1,22:23])
# Formatting section ----
df1 <- separate(data = df, col = "away", sep = " ", into = c("x1","x2","x3","x4"))
#
df2 <- separate(data = df1, col = "home", sep = " ", into = c("y1","y2","y3","y4"))
#
df2$AwayTeam <- ifelse(!is.na(df2$x4),paste(df2$x1,df2$x2,df2$x3,sep=" "),paste(df2$x1,df2$x2,sep=" "))
df2$AwayScore <- as.numeric(ifelse(!is.na(df2$x4),df2$x4,df2$x3))
#
df2$HomeTeam <- ifelse(!is.na(df2$y4),paste(df2$y1,df2$y2,df2$y3,sep=" "),paste(df2$y1,df2$y2,sep=" "))
df2$HomeScore <- as.numeric(ifelse(!is.na(df2$y4),df2$y4,df2$y3))
rm(df1)
#
df3 <- separate(data = df2, col = "VegasLine", sep = "-", into = c("VegasFavorite","VegasLine"))
df3$VegasLine <- as.numeric(df3$VegasLine)
df3$VegasLine[is.na(df3$VegasLine)] <- 0
rm(df2)
#
df4 <- separate(data = df3, col = "Rushes_RushYards_RushTDs_Away", sep = "-", 
                into = c("Rushes_Away","RushYards_Away","RushTDs_Away"))
df4$Rushes_Away <- as.numeric(df4$Rushes_Away)
df4$RushYards_Away <- as.numeric(df4$RushYards_Away)
df4$RushTDs_Away <- as.numeric(df4$RushTDs_Away)
rm(df3)
#
df5 <- separate(data = df4, col = "Rushes_RushYards_RushTDs_Home", sep = "-", 
                into = c("Rushes_Home","RushYards_Home","RushTDs_Home"))
df5$Rushes_Home <- as.numeric(df5$Rushes_Home)
df5$RushYards_Home <- as.numeric(df5$RushYards_Home)
df5$RushTDs_Home <- as.numeric(df5$RushTDs_Home)
rm(df4)
#
df6 <- separate(data = df5, col = "Completions_PassYards_PassTDs_INTs_Away", sep = "-", 
                into = c("Completions_Away","PassAttempts_Away","PassYards_Away","PassTDs_Away","INTthrown_Away"))
df6$Completions_Away <- as.numeric(df6$Completions_Away)
df6$PassAttempts_Away <- as.numeric(df6$PassAttempts_Away)
df6$PassYards_Away <- as.numeric(df6$PassYards_Away)
df6$PassTDs_Away <- as.numeric(df6$PassTDs_Away)
df6$INTthrown_Away <- as.numeric(df6$INTthrown_Away)
rm(df5)
#
df7 <- separate(data = df6, col = "Completions_PassYards_PassTDs_INTs_Home", sep = "-", 
                into = c("Completions_Home","PassAttempts_Home","PassYards_Home","PassTDs_Home","INTthrown_Home"))
df7$Completions_Home <- as.numeric(df7$Completions_Home)
df7$PassAttempts_Home <- as.numeric(df7$PassAttempts_Home)
df7$PassYards_Home <- as.numeric(df7$PassYards_Home)
df7$PassTDs_Home <- as.numeric(df7$PassTDs_Home)
df7$INTthrown_Home <- as.numeric(df7$INTthrown_Home)
rm(df6)
#
df8 <- separate(data = df7, col = "Sacks_Yards_Away", sep = "-", 
                into = c("Sacks_Allowed_Away","Sacked_Yards_Away"))
df8$Sacks_Allowed_Away <- as.numeric(df8$Sacks_Allowed_Away)
df8$Sacked_Yards_Away <- as.numeric(df8$Sacked_Yards_Away)
rm(df7)
#
df9 <- separate(data = df8, col = "Sacks_Yards_Home", sep = "-", 
                into = c("Sacks_Allowed_Home","Sacked_Yards_Home"))
df9$Sacks_Allowed_Home <- as.numeric(df9$Sacks_Allowed_Home)
df9$Sacked_Yards_Home <- as.numeric(df9$Sacked_Yards_Home)
rm(df8)
#
df10 <- separate(data = df9, col = "Fumbles_Lost_Away", sep = "-", 
                 into = c("FumblesCommited_Away","FumblesLost_Away"))
df10$FumblesCommited_Away <- as.numeric(df10$FumblesCommited_Away)
df10$FumblesLost_Away <- as.numeric(df10$FumblesLost_Away)
rm(df9)
#
df11 <- separate(data = df10, col = "Fumbles_Lost_Home", sep = "-", 
                 into = c("FumblesComitted_Home","FumblesLost_Home"))
df11$FumblesComitted_Home <- as.numeric(df11$FumblesComitted_Home)
df11$FumblesLost_Home <- as.numeric(df11$FumblesLost_Home)
rm(df10)
#
df12 <- separate(data = df11, col = "Penalties_Yards_Away", sep = "-", 
                 into = c("PenaltiesCommitted_Away","PYards_Away"))
df12$PenaltiesCommitted_Away <- as.numeric(df12$PenaltiesCommitted_Away)
df12$PYards_Away <- as.numeric(df12$PYards_Away)
rm(df11)
#
df13 <- separate(data = df12, col = "Penalties_Yards_Home", sep = "-", 
                 into = c("PenaltiesCommitted_Home","PYards_Home"))
df13$PenaltiesCommitted_Home <- as.numeric(df13$PenaltiesCommitted_Home)
df13$PYards_Home <- as.numeric(df13$PYards_Home)
rm(df12)
#
df14 <- separate(data = df13, col = "ThirdDowns_Away", sep = "-", 
                 into = c("ThirdDownConversions_Away","ThirdDownAttempts_Away"))
df14$ThirdDownConversions_Away <- as.numeric(df14$ThirdDownConversions_Away)
df14$ThirdDownAttempts_Away <- as.numeric(df14$ThirdDownAttempts_Away)
rm(df13)
#
df15 <- separate(data = df14, col = "ThirdDowns_Home", sep = "-", 
                 into = c("ThirdDownConversions_Home","ThirdDownAttempts_Home"))
df15$ThirdDownConversions_Home <- as.numeric(df15$ThirdDownConversions_Home)
df15$ThirdDownAttempts_Home <- as.numeric(df15$ThirdDownAttempts_Home)
rm(df14)
#
df15$VegasLine <- as.numeric(df15$VegasLine)
#
df15$OverUnder <- gsub("push","",df15$OverUnder)
df15$OverUnder <- trimws(df15$OverUnder, which = "both")
df15$OverUnder <- as.numeric(df15$OverUnder)
#
df15$VegasFavorite <- trimws(df15$VegasFavorite, which = "both")
#
df15$date <- gsub("September ","9/",df15$date)
df15$date <- gsub("October ","10/",df15$date)
df15$date <- gsub("November ","11/",df15$date)
df15$date <- gsub("December ","12/",df15$date)
df15$date <- gsub("January ","1/",df15$date)
df15$date <- gsub("February ","2/",df15$date)
df15$date <- gsub("th  ","/",df15$date)  
df15$date <- gsub("st  ","/",df15$date)  
df15$date <- gsub("nd  ","/",df15$date)  
df15$date <- gsub("rd  ","/",df15$date)  
df15$date <- trimws(df15$date, which = "both")
df15$date <- as.Date(df15$date,format = "%m/%d/%Y")
#
df16 <- separate(data = df15, col = "TOP_Away", sep = ":", into = c("TOP_Min_Away","TOP_Sec_Away"))
rm(df15)
#
df17 <- separate(data = df16, col = "TOP_Home", sep = ":", into = c("TOP_Min_Home","TOP_Sec_Home"))
df17$TOP_Min_Away <- as.numeric(df17$TOP_Min_Away)
df17$TOP_Sec_Away <- as.numeric(df17$TOP_Sec_Away)
df17$TOP_Min_Home <- as.numeric(df17$TOP_Min_Home)
df17$TOP_Sec_Home <- as.numeric(df17$TOP_Sec_Home)
rm(df16)
#
df18 <- df17 %>% mutate(TOP_Home = (TOP_Sec_Home+(TOP_Min_Home*60)),TOP_Away = (TOP_Sec_Away+(TOP_Min_Away*60)))
rm(df17)
#
df <- df18 %>% select(9:ncol(df18))
rm(df18)
#
#i = 1
#while(i<=18){
#  v1 <- paste("df",i,sep="")
#  remove(list = c(v1))
#  i=i+1
#}
#
#
df$VegasHome <- ifelse(df$HomeTeam==df$VegasFavorite,1,0)
df$HomeWin <- ifelse(df$HomeScore>df$AwayScore,1,0)
df$FinalDiff <- abs(df$HomeScore-df$AwayScore)
#
df$VegasRight1 <- ifelse(df$VegasHome == 1,
                         ifelse(df$HomeWin == 1,
                                ifelse(df$FinalDiff >= df$VegasLine,1,0),0),0)
#
df$VegasRight2 <- ifelse(df$VegasHome == 0,
                         ifelse(df$HomeWin == 0,
                                ifelse(df$FinalDiff >= df$VegasLine,1,0),0),0)
#
df$VegasRight <- ifelse(df$VegasRight1 ==1 | df$VegasRight2 ==1 ,1,0)
#
df$OverCheck <- ifelse((df$AwayScore+df$HomeScore)>df$OverUnder,1,0)
#
df$xpp_Home <- ifelse(is.na(df$xpm_Home/df$xpa_Home),0,(df$xpm_Home/df$xpa_Home))
df$xpp_Away <- ifelse(is.na(df$xpm_Away/df$xpa_Away),0,(df$xpm_Away/df$xpa_Away))
df$fgp_Home <- ifelse(is.na(df$fgm_Home/df$fga_Home),0,(df$fgm_Home/df$fga_Home))
df$fgp_Away <- ifelse(is.na(df$fgm_Away/df$fga_Away),0,(df$fgm_Away/df$fga_Away))
#
df$kryds_Home <- ifelse(is.na(df$kryds_Home),0,df$kryds_Home)
df$kryds_Away <- ifelse(is.na(df$kryds_Away),0,df$kryds_Away)
df$krtd_Home <- ifelse(is.na(df$krtd_Home),0,df$krtd_Home)
df$krtd_Away <- ifelse(is.na(df$krtd_Away),0,df$krtd_Away)
df$pryds_Home <- ifelse(is.na(df$pryds_Home),0,df$pryds_Home)
df$pryds_Away <- ifelse(is.na(df$pryds_Away),0,df$pryds_Away)
df$prtd_Home <- ifelse(is.na(df$prtd_Home),0,df$prtd_Home)
df$prtd_Away <- ifelse(is.na(df$prtd_Away),0,df$prtd_Away)
#
dfx <- df
#
dfx$d_FirstDowns = (dfx$FirstDowns_Home-dfx$FirstDowns_Away) 
dfx$d_Rushes = (dfx$Rushes_Home - dfx$Rushes_Away) 
dfx$d_RushYards = (dfx$RushYards_Home-dfx$RushYards_Away)
dfx$d_RushTDs = (dfx$RushTDs_Home-dfx$RushTDs_Away)
dfx$d_Completions = (dfx$Completions_Home-dfx$Completions_Away)
dfx$d_PassAttempts = (dfx$PassAttempts_Home-dfx$PassAttempts_Away)
dfx$d_PassYards = (dfx$PassYards_Home-dfx$PassYards_Away)
dfx$d_PassTDs = (dfx$PassTDs_Home-dfx$PassTDs_Away)
dfx$d_Sacks = (dfx$Sacks_Allowed_Away - dfx$Sacks_Allowed_Home)
dfx$d_Turnovers = (dfx$Turnovers_Home-dfx$Turnovers_Away)
dfx$d_Penalties = (dfx$PenaltiesCommitted_Home-dfx$PenaltiesCommitted_Away)
dfx$d_PenaltyYards = (dfx$PYards_Home-dfx$PYards_Away)
dfx$d_ThirdDownpercent = (dfx$ThirdDownConversions_Home/dfx$ThirdDownAttempts_Home)-(dfx$ThirdDownConversions_Away/dfx$ThirdDownAttempts_Away)
#
dfx$d_XPpercent <- (dfx$xpp_Home-dfx$xpp_Away)
dfx$d_KickReturnYards = (dfx$kryds_Home-dfx$kryds_Away)
dfx$d_KickReturnTDs = (dfx$krtd_Home-dfx$krtd_Away)
dfx$d_PuntReturnYards = (dfx$pryds_Home-dfx$pryds_Away)
dfx$d_PuntReturnTDs = (dfx$prtd_Home-dfx$prtd_Away)
dfx$d_FGpercent <- (dfx$fgp_Home-dfx$fgp_Away)
dfx$d_Punts <- (dfx$punt_Home-dfx$punt_Away)
#
dfx$d_TOP <- (dfx$TOP_Home-dfx$TOP_Away)
#
#str(dfx)
#
dfy <- dfx %>% select(date,HomeWin,84:ncol(dfx))
#
rm(df,dfx)
summary(dfy)
#
# Motivation Section ----
df2020 <- dfy %>% filter(date>"2020-7-1")
df <- dfy %>% filter(date<"2020-7-1")
#
ncol(df)
nrow(df)
glimpse(df[1:5,])
summary(df)
#
glimpse(df2020[1:5,])
#
mean(df$HomeWin)
df %>% ggplot(aes(x=HomeWin)) + geom_histogram(bins = 50)
#
i = 3
cormatrix = data.frame()
while (i <= ncol(df)){
  r = cor(df$HomeWin,df[,i])
  x = colnames(df[i])
  y = r
  cormatrixbase = data.frame(x,y)
  cormatrix = rbind(cormatrixbase,cormatrix)
  i = i+1
}
#
cormatrix %>% mutate(r = abs(cormatrix$y)) %>% arrange(., -r) %>% select(x,r)
#
p1 <- df %>% ggplot(aes(x=d_Rushes, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p2 <- df %>% ggplot(aes(x=d_Turnovers, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p3 <- df %>% ggplot(aes(x=d_RushYards, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p4 <- df %>% ggplot(aes(x=d_PassAttempts, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p5 <- df %>% ggplot(aes(x=d_RushTDs, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p6 <- df %>% ggplot(aes(x=d_ThirdDownpercent, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p7 <- df %>% ggplot(aes(x=d_Sacks, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p8 <- df %>% ggplot(aes(x=d_TOP, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p9 <- df %>% ggplot(aes(x=d_PassTDs, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
p10 <- df %>% ggplot(aes(x=d_KickReturnYards, fill = as.factor(HomeWin)))+geom_density()+ theme(legend.position = "none")
#
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3,nrow=3)
#
df %>% ggplot(aes(x=d_Rushes, fill = as.factor(HomeWin)))+geom_histogram(bins=50)
df %>% filter(HomeWin==1) %>% summarise(m=mean(d_Rushes))
df %>% filter(HomeWin==0) %>% summarise(m=mean(d_Rushes))
# Modeling ----
df$HomeWin <- ifelse(df$HomeWin==1,2,1)
df$HomeWin <- as.factor(df$HomeWin)
df <- df %>% select(-date)
#
set.seed(33, sample.kind = "Rounding")
test_index <- createDataPartition(df$HomeWin, times = 1, p = 0.15, list = FALSE)
test_set <- df[test_index, ]
testx <- test_set %>% select(-HomeWin)
train_set <- df[-test_index, ]
trainx <- train_set %>% select(-HomeWin)
# Below is the test set structure
str(test_set)
# Below is the train set structure
str(train_set)
# Below is a confusion matrix of predicting the home team always wins
Homealwayswins <- test_set %>% mutate(yhat=as.factor(2)) 
confusionMatrix(data = Homealwayswins$yhat, reference = Homealwayswins$HomeWin)
#
#
# GLM
train_glm <- train(x=trainx, y=train_set$HomeWin, method = "glm")
glm_preds <- predict(train_glm, testx)
print(paste("The accuracy of GLM model is: ",round(mean(glm_preds==test_set$HomeWin),4),sep=""))
confusionMatrix(data = glm_preds, reference = test_set$HomeWin)
# Loess
train_loess <- train(x=trainx, y=train_set$HomeWin, method = "gamLoess")
loess_preds <- predict(train_loess, testx)
print(paste("The accuracy of LOESS model is: ",round(mean(loess_preds==test_set$HomeWin),4),sep=""))
confusionMatrix(data = loess_preds, reference = test_set$HomeWin)
# SVM
trainSVM <- svm(x = as.matrix(train_set[2:22]), y = as.matrix(train_set[1]), type="C-classification", kernel = "linear")
SVM_preds <- predict(trainSVM, test_set[,2:22], decision.values = TRUE)
print(paste("The accuracy of SVM (classification, linear kernel) is: ",round(mean(SVM_preds == test_set$HomeWin),4),sep=""))
confusionMatrix(data = SVM_preds, reference = test_set$HomeWin)
# random forest
train_rf <- train(x=trainx, y=train_set$HomeWin,  method = "rf", tuneGrid = data.frame(mtry = c(1,3,5,7,9)))
rf_preds <- predict(train_rf, testx)
print(paste("The accuracy of Random Forest is: ",round(mean(rf_preds == test_set$HomeWin),4),sep=""))
print(paste("The best value of mtry for RF is: ",train_rf$finalModel$mtry,sep=""))
v <- varImp(train_rf)
u <- v$importance
print("a look at variable importance...")
u$Measure <- row.names(u)
u$Rank <- rank(-u$Overall)
u  %>% arrange(.,Rank) %>% select(Overall)
# a look at accuracy w/feature selection
plot(train_rf)
#
confusionMatrix(data = rf_preds, reference = test_set$HomeWin)
# random forest w/ cv
control <- trainControl(method = "cv", number = 50, p = .9)
train_rf_cv <- train(trainx, train_set$HomeWin, method = "rf",  tuneGrid = data.frame(mtry = seq(1, 10, 1)), trControl = control)
rf_cv_preds <- predict(train_rf_cv, testx)
print(paste("The accuracy of Random Forest w/CV is: ",round(mean(rf_cv_preds == test_set$HomeWin),4),sep=""))
print(paste("The best value of mtry for RF w/CV is: ",train_rf_cv$finalModel$mtry,sep=""))
confusionMatrix(data = rf_cv_preds, reference = test_set$HomeWin)
# XGBoost
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
train_XGB<- train(HomeWin ~ ., data = train_set, method = "xgbTree", trControl = TrainControl,verbose = FALSE)
XGB_preds <- predict(train_XGB, testx)
print(paste("The accuracy of XGBoost for Classification is: ",round(mean(XGB_preds==test_set$HomeWin),4),sep=""))
confusionMatrix(data = XGB_preds, reference = test_set$HomeWin)
# Recap Accuracy
print(paste("GLM: ",round(mean(glm_preds==test_set$HomeWin),4),sep=""))
print(paste("LOESS: ",round(mean(loess_preds==test_set$HomeWin),4),sep=""))
print(paste("SVM: ",round(mean(SVM_preds == test_set$HomeWin),4),sep=""))
print(paste("Random Forest: ",round(mean(rf_preds == test_set$HomeWin),4),sep=""))
print(paste("Random Forest w/CV: ",round(mean(rf_cv_preds == test_set$HomeWin),4),sep=""))
print(paste("XGBoost: ",round(mean(XGB_preds==test_set$HomeWin),4),sep=""))
# Ensemble Method
ensemble <- as.data.frame(cbind(glm_preds,loess_preds,SVM_preds,rf_preds,rf_cv_preds,XGB_preds))
ensemble$sums <- (as.numeric(ensemble$glm_preds)+as.numeric(ensemble$loess_preds)+as.numeric(ensemble$SVM_preds)+as.numeric(ensemble$rf_preds)+as.numeric(ensemble$rf_cv_preds)+as.numeric(ensemble$XGB_preds))
ensemble$yhat <- as.factor(ifelse(ensemble$sums>=9,2,1))
mean(ensemble$yhat==test_set$HomeWin)
confusionMatrix(data = ensemble$yhat, reference = test_set$HomeWin)
ensemble$truey <- test_set$HomeWin
x <- ensemble %>% filter(yhat != truey)
x %>% select(-sums)
# Holdout ----
holdoutx <- df2020 %>% select(-HomeWin,-date)
holdouty <- df2020 %>% select(HomeWin)
holdouty$HomeWin <- ifelse(holdouty$HomeWin==1,2,1)
holdouty$HomeWin <- as.factor(holdouty$HomeWin)
#
hoglm <- predict(train_glm, holdoutx)
holoess <- predict(train_loess, holdoutx)
hosvm <- predict(trainSVM, holdoutx, decision.values = TRUE)
horf <-  predict(train_rf, holdoutx)
horfcv <-  predict(train_rf_cv, holdoutx)
hoxgb <- predict(train_XGB, holdoutx)
# individual methods on holdout
print(paste("GLM model on holdout: ",round(mean(hoglm==holdouty$HomeWin),4)),sep="")
print(paste("LOESS model on holdout: ",round(mean(holoess==holdouty$HomeWin),4)),sep="")
print(paste("SVM model on holdout: ",round(mean(hosvm==holdouty$HomeWin),4)),sep="")
print(paste("Random Forest model on holdout: ",round(mean(horf==holdouty$HomeWin),4)),sep="")
print(paste("RF w/Cross-Validation model on holdout: ",round(mean(horfcv==holdouty$HomeWin),4)),sep="")
print(paste("XGBoost model on holdout: ",round(mean(hoxgb==holdouty$HomeWin),4)),sep="")
# ensemble on holdout
holdoutdf <- as.data.frame(cbind(hoglm,holoess,hosvm,horf,horfcv,hoxgb))
holdoutdf$sums <- (holdoutdf$hoglm+holdoutdf$holoess+holdoutdf$hosvm+holdoutdf$horf+holdoutdf$horfcv+holdoutdf$hoxgb)
holdoutdf$yhat <- as.factor(ifelse(holdoutdf$sums>=9,2,1))
print(paste("Ensemble model on holdout: ",round(mean(holdoutdf$yhat==holdouty$HomeWin),4)),sep="")
holdoutdf$truey <- holdouty$HomeWin
holdoutdf %>% filter(yhat != truey) %>% select(-sums)
# Results ----
investigate <- cbind(df2020,holdoutdf$yhat)
investigate$`holdoutdf$yhat` <- as.numeric(investigate$`holdoutdf$yhat`)-1
str(investigate)
#
d1 <- investigate %>% filter(HomeWin != `holdoutdf$yhat`)  %>% select(d_Rushes)
d2 <- investigate %>% filter(HomeWin == `holdoutdf$yhat`)  %>% select(d_Rushes)
#
print("You can see that the average d_Rushes between correct and incorrect predictions is ~.5 off...")
paste("Incorrect Predictions: ",round(mean(d1$d_Rushes),3),sep="")
paste("Correct Predictions: ",round(mean(d2$d_Rushes),3),sep="")
#
d3 <- investigate %>% filter(HomeWin != `holdoutdf$yhat`) %>% filter(HomeWin==1) %>% select(d_Rushes)
d4 <- investigate %>% filter(HomeWin == `holdoutdf$yhat`) %>% filter(HomeWin==1) %>% select(d_Rushes)
#
print("The average d_Rushes between correct and incorrect predictions if the Home Team wins...")
paste("Incorrect Predictions: ",round(mean(d3$d_Rushes),3),sep="")
paste("Correct Predictions: ",round(mean(d4$d_Rushes),3),sep="")
#
d5 <- investigate %>% filter(HomeWin != `holdoutdf$yhat`) %>% filter(HomeWin ==0) %>% select(d_Rushes)
d6 <- investigate %>% filter(HomeWin == `holdoutdf$yhat`) %>% filter(HomeWin ==0) %>% select(d_Rushes)
#
print("The average d_Rushes between correct and incorrect predictions if the Home Team loses...")
paste("Incorrect Predictions: ",round(mean(d5$d_Rushes),3),sep="")
paste("Correct Predictions: ",round(mean(d6$d_Rushes),3),sep="")
#
print("Plotting out d_Rushes on the x-axis, we can investigate predictions vs actuals...")
print("triangles are a Home Wins prediction, and Red color means the away team won...")
investigate %>% ggplot(aes(x=d_Rushes,y=`holdoutdf$yhat`, color=as.factor(HomeWin), shape = as.factor(`holdoutdf$yhat`)))+geom_point(position = "jitter",size=3)+scale_y_discrete()
#
investigate %>% ggplot(aes(x=d_Turnovers,y=`holdoutdf$yhat`, color=as.factor(HomeWin), shape = as.factor(`holdoutdf$yhat`)))+geom_point(position = "jitter",size=3)+scale_y_discrete()
#
investigate %>% ggplot(aes(x=d_RushYards,y=`holdoutdf$yhat`, color=as.factor(HomeWin), shape = as.factor(`holdoutdf$yhat`)))+geom_point(position = "jitter",size=3)+scale_y_discrete()
# END ----
time2 <- Sys.time()
as.numeric(time2-time1)