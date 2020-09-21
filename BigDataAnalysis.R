
########### Big Data analysis for IBM, IRobot and Netflix ############# 
############################## For IRobot #############################
# Getting data from Reddit
library("RedditExtractoR")
data <- get_reddit(search_terms = "IRBT", page_threshold = 1, subreddit = 'stocks')

# Converting date from sting to date format
data$comm_date <- as.Date(data$comm_date, "%d-%m-%y")
View(data)

# Getting sentiments from comments on posts
library(syuzhet)
mySentiment <- get_nrc_sentiment(data$comment)
head(mySentiment)
data <- cbind(data, mySentiment)


library(sqldf)
# grouping the data at comment dates and summarizing negative and positive comments
by_date = sqldf("select comm_date, sum(negative) as negative, sum(positive) as positive from data group by 1")


# getting stock data for each day
library(quantmod)
getSymbols(Symbols = "IRBT",
           src = "yahoo",
           from="2017-11-01",
           to="2019-10-01"
)

IRBT_df = data.frame(value=coredata(IRBT),timestamp=index(IRBT))
colnames(IRBT_df) <- c('Open','High','Low','Close','Volume','Adjusted','timestamp')

head(IRBT_df)

# Joining stock data with sentiments for only those days where sentiments are present
new <- sqldf("select a.*, b.positive, b.negative, (positive - negative) as overall
             from IRBT_df a 
             join by_date b on a.timestamp = b.comm_date")

# Cheking the relationship between closing price of stock and sentiments
library(ggplot2)
ggplot(new, aes(x = overall, y = Adjusted)) + geom_point()
ggplot(new, aes(x = positive, y = Adjusted)) + geom_point()
ggplot(new, aes(x = negative, y = Adjusted)) + geom_point()

# getting different corelation value bwteen cloing price and sentiments
cor(new$Close, new$overall)
cor(new$Close, new$positive)
cor(new$Close, new$negative)

# estimating beta coefficients for future prediction
fit <- lm(Close ~ overall, data = new)
summary(fit)


######################## For IBM ########################################
# Getting data from Reddit
data <- get_reddit(search_terms = "IBM", page_threshold = 1, subreddit = 'stocks')

# Converting date from sting to date format
data$comm_date <- as.Date(data$comm_date, "%d-%m-%y")
View(data)

# Getting sentiments from comments on posts
library(syuzhet)
mySentiment <- get_nrc_sentiment(data$comment)
head(mySentiment)
data <- cbind(data, mySentiment)


library(sqldf)
# grouping the data at comment dates and summarizing negative and positive comments
by_date = sqldf("select comm_date, sum(negative) as negative, sum(positive) as positive from data group by 1")

# getting stock data for each day
library(quantmod)
getSymbols(Symbols = "IBM",
           src = "yahoo",
           from="2017-11-01",
           to="2019-10-01"
)

IRBT_df = data.frame(value=coredata(IRBT),timestamp=index(IRBT))
colnames(IRBT_df) <- c('Open','High','Low','Close','Volume','Adjusted','timestamp')

head(IRBT_df)

# Joining stock data with sentiments for only those days where sentiments are present
new <- sqldf("select a.*, b.positive, b.negative, (positive - negative) as overall
             from IRBT_df a 
             join by_date b on a.timestamp = b.comm_date")


# Cheking the relationship between closing price of stock and sentiments
library(ggplot2)
ggplot(new, aes(x = overall, y = Adjusted)) + geom_point()
ggplot(new, aes(x = positive, y = Adjusted)) + geom_point()
ggplot(new, aes(x = negative, y = Adjusted)) + geom_point()

# getting different corelation value bwteen cloing price and sentiments
cor(new$Close, new$overall)
cor(new$Close, new$positive)
cor(new$Close, new$negative)

# estimating beta coefficients for future prediction
fit <- lm(Close ~ overall, data = new)
summary(fit)


######################## For NFLX ########################################
# Getting data from Reddit
data <- get_reddit(search_terms = "NFLX", page_threshold = 1, subreddit = 'stocks')

# Converting date from sting to date format
data$comm_date <- as.Date(data$comm_date, "%d-%m-%y")
View(data)

# Getting sentiments from comments on posts
library(syuzhet)
mySentiment <- get_nrc_sentiment(data$comment)
head(mySentiment)
data <- cbind(data, mySentiment)


library(sqldf)
# grouping the data at comment dates and summarizing negative and positive comments
by_date = sqldf("select comm_date, sum(negative) as negative, sum(positive) as positive from data group by 1")

# getting stock data for each day
library(quantmod)
getSymbols(Symbols = "NFLX",
           src = "yahoo",
           from="2017-11-01",
           to="2019-10-01"
)

IRBT_df = data.frame(value=coredata(IRBT),timestamp=index(IRBT))
colnames(IRBT_df) <- c('Open','High','Low','Close','Volume','Adjusted','timestamp')

head(IRBT_df)

# Joining stock data with sentiments for only those days where sentiments are present
new <- sqldf("select a.*, b.positive, b.negative, (positive - negative) as overall
             from IRBT_df a 
             join by_date b on a.timestamp = b.comm_date")


# Cheking the relationship between closing price of stock and sentiments
library(ggplot2)
ggplot(new, aes(x = overall, y = Adjusted)) + geom_point()
ggplot(new, aes(x = positive, y = Adjusted)) + geom_point()
ggplot(new, aes(x = negative, y = Adjusted)) + geom_point()

# getting different corelation value bwteen cloing price and sentiments
cor(new$Close, new$overall)
cor(new$Close, new$positive)
cor(new$Close, new$negative)

# estimating beta coefficients for future prediction
fit <- lm(Close ~ overall, data = new)
summary(fit)





