# 5 points: >= .9
# 10 points: 0.86550 <= RMSE <= 0.89999
# 15 points: 0.86500 <= RMSE <= 0.86549
# 20 points: 0.86490 <= RMSE <= 0.86499
# 25 points: RMSE < 0.86490


time1 <- Sys.time()
# load packages ----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
#
options("scipen"=100, "digits"=4)
#
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#
##### Download data
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
#
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
#
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId), title = as.character(title), genres = as.character(genres))
#
movielens <- left_join(ratings, movies, by = "movieId")
#
rm(movies,ratings,dl)
#
#
# Exploration ----
head(movielens)
# unique users
x <- unique(movielens$userId)
paste("Number of Unique Users: ", length(x), sep="")
# unique movies
x <- unique(movielens$movieId)
paste("Number of Unique Movies: ", length(x), sep="")
# genres
x <- movielens %>% select(genres, movieId) %>% distinct(movieId, genres) %>% separate_rows(genres, sep ="\\|")
genre_count <- x %>% group_by(genres) %>% summarize(n = n(), .groups = "drop") %>% arrange(-n)
genre_count[1:10,]
rm(x,genre_count)
#
#
# Feature engineering prior to split
# get a timestamp, add some one-hot encoding for top 10 genres
movielens$timestamp <- as.Date(as.POSIXct(movielens$timestamp, origin = "1970-01-01"))
movielens$Drama1h <- ifelse(str_detect(movielens$genres,"Drama"),1,0)
movielens$Comedy1h <- ifelse(str_detect(movielens$genres,"Comedy"),1,0)
movielens$Thriller1h <- ifelse(str_detect(movielens$genres,"Thriller"),1,0)
movielens$Romance1h <- ifelse(str_detect(movielens$genres,"Romance"),1,0)
movielens$Action1h <- ifelse(str_detect(movielens$genres,"Action"),1,0)
movielens$Crime1h <- ifelse(str_detect(movielens$genres,"Crime"),1,0)
movielens$Adventure1h <- ifelse(str_detect(movielens$genres,"Adventure"),1,0)
movielens$Horror1h <- ifelse(str_detect(movielens$genres,"Horror"),1,0)
movielens$SciFi1h <- ifelse(str_detect(movielens$genres,"Sci-Fi"),1,0)
movielens$Fantasy1h <- ifelse(str_detect(movielens$genres,"Fantasy"),1,0)
#
ml1 <- movielens %>% extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)
ml1$year <- as.Date(paste(ml1$year,"-06-15",sep="")) #using june 15th of the year for every movie since it is the middle of the year
ml1$movieAGE <- as.double((ml1$timestamp - ml1$year)/365) # calculate age of movie relative to each rating
ml1$agegroup <- ifelse(ml1$movieAGE<=1,"1Y",
                                    ifelse(ml1$movieAGE<=3,"3Y",
                                           ifelse(ml1$movieAGE<=5,"5Y",
                                                  ifelse(ml1$movieAGE<=10,"10Y",
                                                         ifelse(ml1$movieAGE<15,"15Y",
                                                                ifelse(ml1$movieAGE<=20,"20Y",
                                                                       ifelse(ml1$movieAGE<=30,"30Y",
                                                                              ifelse(ml1$movieAGE<=50,"50Y","50plus"))))))))
#
movielens <- ml1 %>% select(-title) %>% rename(.,  "Title"=title_tmp)
#
rm(ml1)
head(movielens)
str(movielens)
summary(movielens)
#
# movies by year
movielens %>% select(movieId,year) %>% distinct(movieId,year) %>% group_by(year) %>% summarise(movies_produced=n()) %>% ggplot(aes(x=year,y=movies_produced))+geom_point()
#
# Split into training and validation sets
# Validation set will be 10% of MovieLens data
set.seed(135, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
#
# Make sure userId and movieId in validation set are also in edx set
test <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, test)
train <- rbind(edx, removed)
#
#
rm( test_index, temp, movielens, removed, edx)
#
# split off train set into train, test and use test as true validation set (as intended!)
validation <- test
split <- train
rm(test,train)
#
set.seed(737, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = split$rating, times = 1, p = 0.05, list = FALSE)
temp1 <- split[-test_index,]
temp2 <- split[test_index,]
test <- temp2 %>% semi_join(temp1, by = "movieId") %>% semi_join(temp1, by = "userId")
removed <- anti_join(temp2,test)
train <- rbind(temp1,removed)
#
rm(test_index,removed,split,temp1,temp2)
#
str(train)
#
#
#
# movies bucket
averagerating <- train %>% select(movieId,rating) %>% group_by(movieId) %>% summarise(movie_avgrating=mean(rating), movie_countofrating=n())
hist(averagerating$movie_countofrating, main = "Histogram of Rating Count", xlab = "# of Ratings")
# Lots of movies have a few ratings, a few movies have a lot of ratings!
#
# take count of ratings by movie, arrange in descending order, and run cumulative sum
# this will show us how many movies it takes to x% of total ratings in the data, and then I bucket based on how much the movie is "worth" as a %
x <- averagerating %>% select(movieId,movie_countofrating) %>% unique(.)
x <- x %>% arrange(.,-movie_countofrating)
x$percent <- x$movie_countofrating/sum(x$movie_countofrating)
x <- x %>% mutate(cumulative=cumsum(percent))
x[1:10,]
#
x$movie_countbucket <- ifelse(x$cumulative<.1,"a",
                ifelse(x$cumulative<.2,"b",
                       ifelse(x$cumulative<.3,"c",
                              ifelse(x$cumulative<.4,"d",
                                     ifelse(x$cumulative<.5,"e",
                                            ifelse(x$cumulative<.6,"f",
                                                   ifelse(x$cumulative<.7,"g",
                                                          ifelse(x$cumulative<.8,"h",
                                                                 ifelse(x$cumulative<.9,"i","j")))))))))

#
table(x$movie_countbucket)
# 40 movies make up top decile of ratings, 66 movies make up next decile, and so on
p <- cbind(x %>% select(movie_countbucket) %>% group_by(movie_countbucket) %>% summarise(n()),seq(1,10,1)) %>% as.data.frame()
colnames(p) <- c("MovieCountBucket","N","Seq")
p %>% select(N,Seq) %>% ggplot(aes(x=Seq,y = N)) + geom_point() +geom_line()
rm(p)
# it is pretty easy to see how large of a tail the final few deciles make up
#
x1 <- x %>% select(movieId,movie_countbucket)
#
averagerating <- left_join(averagerating,x1)
# 
# exploring average ratings and count of ratings
averagerating %>% select(movie_countbucket,movie_avgrating) %>% group_by(movie_countbucket) %>% summarise(x=mean(movie_avgrating))
cor(averagerating$movie_avgrating,averagerating$movie_countofrating)
averagerating %>% ggplot(aes(movie_avgrating,movie_countofrating, color=movie_countbucket)) + geom_point()+geom_jitter() + labs(x="Average Rating", y="# of Ratings", title="All Movies")
# the spread of ratings count for bucket A is massive, what does it look like without it?
averagerating %>% filter(movie_countbucket != "a")%>% ggplot(aes(movie_avgrating,movie_countofrating, color=movie_countbucket)) + geom_point() + labs(x="Average Rating", y="# of Ratings", title="Without 'A' Movies")
#
train <- left_join(train, averagerating, by = "movieId") # join average rating by movie, count of ratings by movie, and movie count bucket to the training set.
#
head(train)
#
movielookup <- averagerating
rm(averagerating,x,x1)
#
#
#
# user buckets
usrrating <- train %>% select(userId,rating) %>% group_by(userId) %>% summarise(user_avgrating=mean(rating), usermovcount=n())
hist(usrrating$usermovcount, main = "Histogram of Ratings Given", xlab="# of Movies Rated")
# a few people rate a ton of movies, a ton of people rate a few movies.
summary(usrrating)
# min is 10 movies, max is 6,271 movies (!) and median is 59...
#
x <- usrrating %>% select(userId,usermovcount) %>% unique(.)
x <- x %>% arrange(.,-usermovcount)
x$percent <- x$usermovcount/sum(x$usermovcount)
x <- x %>% mutate(cumulative=cumsum(percent))
# similar to the movie exercise above, I am bucketing out the Users based on # of movies reviewed
# bucketing into deciles, similar to above.
x$user_countbucket <- ifelse(x$cumulative<.1,"a",
                              ifelse(x$cumulative<.2,"b",
                                     ifelse(x$cumulative<.3,"c",
                                            ifelse(x$cumulative<.4,"d",
                                                   ifelse(x$cumulative<.5,"e",
                                                          ifelse(x$cumulative<.6,"f",
                                                                 ifelse(x$cumulative<.7,"g",
                                                                        ifelse(x$cumulative<.8,"h",
                                                                               ifelse(x$cumulative<.9,"i","j")))))))))


#
table(x$user_countbucket)
#
# you can see above that 643 users account for the top decile of ratings. there is a huge tail at the end, and the last two buckets are ~43k users out of 69k total!
p <- cbind(x %>% select(user_countbucket) %>% group_by(user_countbucket) %>% summarise(n()),seq(1,10,1)) %>% as.data.frame()
colnames(p) <- c("UserCountBucket","N","Seq")
p %>% select(N,Seq) %>% ggplot(aes(x=Seq,y = N)) + geom_point() +geom_line()
rm(p)
#
# not quite as drastic as the movie visual, but you can easily see the ramp up of users 
#
x1 <- x %>% select(userId,user_countbucket)
usrrating <- left_join(usrrating,x1)
#
#
# usrrating$user_countbucket <- ifelse(usrrating$usermovcount <= 25, "F",
#                                 ifelse(usrrating$usermovcount <= 50,"E",
#                                        ifelse(usrrating$usermovcount<=100,"D",
#                                               ifelse(usrrating$usermovcount<=250,"C",
#                                                      ifelse(usrrating$usermovcount>1000,"A","B")))))
# #
# table(as.factor(usrrating$user_countbucket))
#
train <- left_join(train, usrrating, by = "userId")
#
str(train)
head(train)
#
userlookup <- usrrating
rm(usrrating,x,x1)
#
#
#
# fitting Genre based on one-hot
#
gfit <- train %>% select(rating,8:17) %>% glm(formula = rating~., family = "gaussian")
genre_coefficients <- gfit$coefficients
#
train$genrefit <- genre_coefficients[1]+
  (train[,8]*genre_coefficients[2])+
  (train[,9]*genre_coefficients[3])+
  (train[,10]*genre_coefficients[4])+
  (train[,11]*genre_coefficients[5])+
  (train[,12]*genre_coefficients[6])+
  (train[,13]*genre_coefficients[7])+
  (train[,14]*genre_coefficients[8])+
  (train[,15]*genre_coefficients[9])+
  (train[,16]*genre_coefficients[10])+
  (train[,17]*genre_coefficients[11])
#
head(train)
rm(gfit)
#
#
# fitting movie age
hist(train$movieAGE)
#
#
avg_rating_by_year <- train %>% select(rating,year) %>% group_by(year) %>% summarise(avg_byyear = mean(rating))
avg_rating_by_year[1:10,]
avg_rating_byagegroup <- train %>% select(rating,agegroup) %>% group_by(agegroup) %>% summarise(avg_byagegroup = mean(rating))
avg_rating_byagegroup
#
train <- left_join(train,avg_rating_byagegroup)
train <- left_join(train,avg_rating_by_year)
#
head(train)
#
#rm(avg_rating_byagegroup)
#
# exploring the time effect
# older movies have a better rating on average...it is somewhat intuitive, if you are going back to watch a 30 year old movie, it is because you have heard of it, because it is good!
train %>% select(year,rating) %>% group_by(year) %>% summarise(avgbyyear=mean(rating)) %>% ggplot(aes(x=year,y=avgbyyear))+geom_point()
train %>% filter(user_countbucket != "a") %>% select(year,rating) %>% group_by(year) %>% summarise(avgbyyear=mean(rating)) %>% ggplot(aes(x=year,y=avgbyyear))+geom_point()
#train %>% select(year,rating, user_countbucket) %>% group_by(year, user_countbucket) %>% summarise(avgbyyear=mean(rating)) %>% ggplot(aes(x=year,y=avgbyyear, col=user_countbucket))+geom_line()
#
# movie count bucket effect
movie_count_bucket_df <- train %>% select(rating,movie_countbucket) %>% group_by(movie_countbucket) %>% summarise(avg_bymovbucket = mean(rating))
movie_count_bucket_df # movies that get reviewed more often are better...
#
train <- left_join(train,movie_count_bucket_df)
#
#rm(movie_count_bucket_df)
#
# user count bucket effect
user_count_bucket_df <- train %>% select(rating, user_countbucket) %>% group_by(user_countbucket) %>% summarise(avg_byuserbucket = mean(rating))
user_count_bucket_df # users that review less movies rate higher.
#
train <- left_join(train,user_count_bucket_df)
#
#rm(user_count_bucket_df)
#
head(train)
head(test)
#
time2 <- Sys.time()
as.numeric(time2-time1)
#
#  function to round ratings to either .0 or .5 -----
roundrating <- function(x){
  x <- ifelse(x<.75,0.5,
         ifelse(x<1.25,1,
                ifelse(x<1.75,1.5,
                       ifelse(x<2.25,2,
                              ifelse(x<2.75,2.5,
                                     ifelse(x<3.25,3,
                                            ifelse(x<3.75,3.5,
                                                   ifelse(x<4.25,4,
                                                          ifelse(x<4.75,4.5,5)))))))))
  x
}
#
# -----
#
#
#

# y is rating
# x's are 1movie_avgrating, 2user_avgrating, 3genrefit, 4avg by year,  5avg_byage, 6avg_bymovbucket, 7avg_byuserbucket
lookup <- train %>% select(rating,movie_avgrating,user_avgrating,genrefit,avg_byyear,avg_byagegroup,avg_bymovbucket,avg_byuserbucket)
fit <- lookup %>% glm(formula=rating~.,family="gaussian")
fit_coefficients <- fit$coefficients
fit_coefficients
#
train$yhat <- 
  fit_coefficients[1]+
  (fit_coefficients[2]*train$movie_avgrating)+
  (fit_coefficients[3]*train$user_avgrating)+
  (fit_coefficients[4]* train$genrefit)+
  (fit_coefficients[5]*train$avg_byyear)+
  (fit_coefficients[6]*train$avg_byagegroup)+
  (fit_coefficients[7]*train$avg_bymovbucket)+
  (fit_coefficients[8]*train$avg_byuserbucket)
#
#
#
head(train)
RMSE(train$rating,train$yhat)
#
#
testing <- left_join(test,movielookup)
testing <- left_join(testing,userlookup)
testing <- left_join(testing,movie_count_bucket_df)
testing <- left_join(testing,user_count_bucket_df)
testing <- left_join(testing,avg_rating_by_year)
testing <- left_join(testing,avg_rating_byagegroup)
head(testing)
testing$genrefit <- genre_coefficients[1]+
  (testing[,8]*genre_coefficients[2])+
  (testing[,9]*genre_coefficients[3])+
  (testing[,10]*genre_coefficients[4])+
  (testing[,11]*genre_coefficients[5])+
  (testing[,12]*genre_coefficients[6])+
  (testing[,13]*genre_coefficients[7])+
  (testing[,14]*genre_coefficients[8])+
  (testing[,15]*genre_coefficients[9])+
  (testing[,16]*genre_coefficients[10])+
  (testing[,17]*genre_coefficients[11])
#
head(testing)
#
testing$yhat <- fit_coefficients[1]+
  (fit_coefficients[2]*testing$movie_avgrating)+
  (fit_coefficients[3]*testing$user_avgrating)+
  (fit_coefficients[4]*testing$genrefit)+
  (fit_coefficients[5]*testing$avg_byyear)+
  (fit_coefficients[6]*testing$avg_byagegroup)+
  (fit_coefficients[7]*testing$avg_bymovbucket)+
  (fit_coefficients[8]*testing$avg_byuserbucket)
#

RMSE(testing$rating,testing$yhat)
#
#
#
#
testing <- left_join(validation,movielookup)
testing <- left_join(testing,userlookup)
testing <- left_join(testing,movie_count_bucket_df)
testing <- left_join(testing,user_count_bucket_df)
testing <- left_join(testing,avg_rating_by_year)
testing <- left_join(testing,avg_rating_byagegroup)
head(testing)
testing$genrefit <- genre_coefficients[1]+
  (testing[,8]*genre_coefficients[2])+
  (testing[,9]*genre_coefficients[3])+
  (testing[,10]*genre_coefficients[4])+
  (testing[,11]*genre_coefficients[5])+
  (testing[,12]*genre_coefficients[6])+
  (testing[,13]*genre_coefficients[7])+
  (testing[,14]*genre_coefficients[8])+
  (testing[,15]*genre_coefficients[9])+
  (testing[,16]*genre_coefficients[10])+
  (testing[,17]*genre_coefficients[11])
#
head(testing)
#
testing$yhat <- fit_coefficients[1]+
  (fit_coefficients[2]*testing$movie_avgrating)+
  (fit_coefficients[3]*testing$user_avgrating)+
  (fit_coefficients[4]*testing$genrefit)+
  (fit_coefficients[5]*testing$avg_byyear)+
  (fit_coefficients[6]*testing$avg_byagegroup)+
  (fit_coefficients[7]*testing$avg_bymovbucket)+
  (fit_coefficients[8]*testing$avg_byuserbucket)
#
RMSE(testing$rating,testing$yhat)
#
time2 <- Sys.time()
as.numeric(time2-time1)