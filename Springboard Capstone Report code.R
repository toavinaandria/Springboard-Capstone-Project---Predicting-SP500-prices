### 1. Loading main packages and data =======================================================

# Unquote the below if devtools and the Quandle packages are not yet installed

#install.packages("devtools")
#install_github('quandl/R-package')
#install.packages("quantmod")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.pacakges("xts")
#install.packages("timeSeries")
#install.packages("gridExtra"")
#install.packages("forecast")
#install.packages("GGally")
#install.packages("ggthemes")
#install.packages("rpart")
#install.packages("rpart.plot")

library(devtools)
library(Quandl)
library(dplyr)
library(xts)
library(quantmod)
library(timeSeries)
library(ggplot2)
library(GGally)
library(forecast)
library(zoo)

# Daily SPY price-volume
SPY <- Quandl(
  "YAHOO/INDEX_SPY",
  type = "raw",
  start_date = "2006-11-06",
  end_date = "2016-04-01"
)

# Weekly S&P 500 Index commitment of Traders report - number of positions
COT <- Quandl(
  "CFTC/SP_F_L_ALL",
  type = "raw",
  start_date = "2006-11-06",
  end_date = "2016-04-01"
)

# CBOE Equity Indices Put-Call Volume
PUT_CALL <- Quandl(
  "CBOE/INDEX_PC",
  type = "raw",
  start_date = "2006-11-06",
  end_date = "2016-04-01"
)




### 2. Data wrangling ===========================================================================


# Cleaning up the data to keep relevant features only
# Remove unused columns from COT data
COT <- COT[, c(
  "Date",
  "Noncommercial Long",
  "Noncommercial Short",
  "Commercial Long",
  "Commercial Short",
  "Nonreportable Positions Long",
  "Nonreportable Positions Short"
)]

# Rename columns for ease of use
colnames(COT) <- c(
  "Date",
  "Speculators.Long",
  "Speculators.Short",
  "Commercials.Long",
  "Commercials.Short",
  "Others.Long",
  "Others.Short"
)

# Compute net positions by trader type
COT$Speculators.Net <- COT$Speculators.Long - COT$Speculators.Short
COT$Commercials.Net <- COT$Commercials.Long - COT$Commercials.Short
COT$Others.Net <- COT$Others.Long - COT$Others.Short

# Keep only relevant columns
COT <- COT %>%
  select(Date, Speculators.Net, Commercials.Net, Others.Net)
colnames(PUT_CALL) <-
  c("Date", "Call.Vol", "Put.Vol", "Total.Vol", "Previous.Open")

# Compute Put-Call Ratio and Net Put Volume
PUT_CALL$Put.Call.Ratio <- PUT_CALL$Put.Vol / PUT_CALL$Call.Vol
PUT_CALL$Net.Put.Vol <- PUT_CALL$Put.Vol - PUT_CALL$Call.Vol

# Keep only relevant columns
PUT_CALL <- PUT_CALL %>%
  select(Date, Put.Call.Ratio, Net.Put.Vol)

colnames(SPY) <-
  c("Date", "Open", "High", "Low", "Close", "Volume", "Adj.Close")

# Check distribution of days of the week
count(COT, weekdays(Date))
count(SPY, weekdays(Date))
count(PUT_CALL, weekdays(Date))

# Find data which related to dates besides Tuesdays
outliers_cot <- subset(COT, weekdays(COT$Date) != "Tuesday")["Date"]
outliers_cot$dow <- weekdays(outliers_cot$Date)

outliers_cot

# Amend the dates for COT to the nearest Tuesday
COT$Date <- as.Date(ifelse(weekdays(COT$Date) == "Monday",
                           COT$Date + 1, COT$Date))
COT$Date <- as.Date(ifelse(weekdays(COT$Date) == "Wednesday",
                           COT$Date - 1, COT$Date))

# Check results - there should be no day besides Tuesdays
count(COT, weekdays(Date))

# Shift the COT dates to Friday so as to match other data
# once transformed to weekly data
COT$Date <- COT$Date + 3

# Remove outliers_cot as unused
outliers_cot <- NULL

# Check for missing dates in PUT_CALL data before creating weekly series
All.Weekdays <-
  as.data.frame(seq(as.Date("2006/11/06"), as.Date("2016/04/01"), 1))
colnames(All.Weekdays) <- "Date"
All.Weekdays <-
  subset(All.Weekdays, !(weekdays(All.Weekdays$Date) %in% c("Saturday", "Sunday")))

PUT_CALL <- merge(All.Weekdays, PUT_CALL, all = TRUE)

# Fill in missing Fridays with previous available value
PUT_CALL$Put.Call.Ratio <- na.locf(PUT_CALL$Put.Call.Ratio)
PUT_CALL$Net.Put.Vol <- na.locf(PUT_CALL$Net.Put.Vol)

# Transform PUT_CALL into weekly series
PUT_CALL <- subset(PUT_CALL, weekdays(PUT_CALL$Date) == "Friday")

#Do the same for the price series to account for holidays
SPY <- merge(All.Weekdays, SPY, all = TRUE)

missing.obs <- which(is.na(SPY$Open))
missingminus.obs <- missing.obs - 1
missingplus.obs <- missing.obs + 1

#Fill in missing closes by taking next observation's open
SPY$Close[missing.obs] <- SPY$Open[missingplus.obs]

# Fill in missing open values by taking last observation's closing value
SPY$Open[missing.obs] <- SPY$Close[missingminus.obs]

# Extend other missing values in a smart way
SPY$High[missing.obs] <- pmax(SPY$High[missing.obs - 1],
                              SPY$Open[missing.obs], SPY$Close[missing.obs])
SPY$Low[missing.obs] <- pmin(SPY$Low[missing.obs - 1],
                             SPY$Open[missing.obs], SPY$Close[missing.obs])
SPY$Adj.Close <- na.locf(SPY$Adj.Close)
SPY$Volume <- na.fill(SPY$Volume, "extend")
SPY$Open <- na.locf(SPY$Open)
SPY$Close <- na.locf(SPY$Close)

missing.obs <- which(is.na(SPY$High))
SPY$High[missing.obs] <- pmax(SPY$High[missing.obs - 1],
                              SPY$Open[missing.obs], SPY$Close[missing.obs])
SPY$Low[missing.obs] <- pmin(SPY$Low[missing.obs - 1],
                             SPY$Open[missing.obs], SPY$Close[missing.obs])
SPY$High[missing.obs] <- pmax(SPY$High[missing.obs - 1],
                              SPY$Open[missing.obs], SPY$Close[missing.obs])
SPY$Low[missing.obs] <- pmin(SPY$Low[missing.obs - 1],
                             SPY$Open[missing.obs], SPY$Close[missing.obs])

# Separate the dependent variable into its own series
PRICE <- SPY %>% select(Date, Adj.Close)
colnames(PRICE) <- c("Date", "Price")

# Transform datasets to xts for timeframe compression
SPY <- as.xts(SPY[,-1], order.by = as.Date(SPY$Date))
COT <- as.xts(COT[, -1], order.by = as.Date(COT$Date))
PUT_CALL <- as.xts(PUT_CALL[, -1],
                   order.by = as.Date(PUT_CALL$Date))
PRICE <- as.xts(PRICE[, -1],
                order.by = as.Date(PRICE$Date))

# Convert the SPY data to weekly datasets
SPY <- to.period(
  SPY,
  period = "weeks",
  drop.time = TRUE,
  OHLC = TRUE,
  indexAt = "endof"
)
PRICE <- to.period(
  PRICE,
  period = "weeks",
  drop.time = TRUE,
  OHLC = FALSE,
  indexAt = "endof"
)

colnames(PRICE) <- "Price"

# Merge the datasets into one database
data <- merge(COT, PRICE, join = 'left')
data <- merge(data, PUT_CALL, join = 'left')
data <- merge(data, SPY$SPY.Volume, join = 'left')
names(data)[names(data) == 'SPY.Volume'] <- 'Volume'

# Count missing observations in the merged dataset to
# ensure no NAs
sum(is.na(data$Price))




### 3. Plotting variables ===========================================================================================


## Plotting the variables
library(gridExtra)

data.df <- data.frame(data, Date = index(data))

price_series <- ggplot(data = data.df,
                       aes(x = Date, y = Price)) +
  geom_line(size = 0.8,
            color = "darkcyan",
            alpha = 0.9) +
  ggtitle("SPY Weekly Adjusted Closing Price -
          Nov 2006 to Mar 2016") +
  ylab("$ per Share") +
  geom_hline(yintercept = 50,
             size = 1.2,
             color = 'gray54') +
  ylim(50, 220)


COT_series <- ggplot() +
  geom_line(data = data.df,
            aes(x = Date, y = Speculators.Net, color = "Red")) +
  geom_line(data = data.df,
            aes(x = Date, y = Commercials.Net, color = "Blue")) +
  geom_line(data = data.df,
            aes(x = Date, y = Others.Net, color = "Green")) +
  theme(legend.position = "none") +
  ylab("Net Positions by Type") +
  ggtitle(
    "Net Positions by Type of Investor (Blue - Commercial,
    Red - Large Speculators, Green - Others)"
  ) +
  geom_hline(aes(yintercept = 0))

NetPut_series <- ggplot() +
  geom_line(data = data.df,
            aes(x = Date, y = Net.Put.Vol), color = "darkslategray") +
  ggtitle("Net Put-Call Volume") +
  geom_hline(aes(yintercept = 0))

PutCall_series <- ggplot() +
  geom_line(data = data.df,
            aes(x = Date, y = Put.Call.Ratio)) +
  ggtitle("Put-Call Ratio") +
  geom_hline(aes(yintercept = mean(data.df$Put.Call.Ratio, na.rm = TRUE)), color = "darkslateblue")

all_ts <- grid.arrange(price_series, COT_series, NetPut_series,
                       PutCall_series, ncol = 1)
all_ts

### 4. Look at weekly returns ===========================================================================================

## Look at weekly returns distribution

weekly_returns <-
  as.xts(returns(SPY$SPY.Close, method = "simple", percentage = TRUE))
weekly_returns_df <-
  data.frame(weekly_returns,
             date = index(weekly_returns),
             coredata(weekly_returns))
weekly_returns_df <- select(weekly_returns_df, 1:2)
weekly_returns_df <- weekly_returns_df[c(2, 1)]
weekly_returns_df <- weekly_returns_df[-1,]
weekly_returns <- weekly_returns[-1,]
names(weekly_returns)[names(weekly_returns) == 'SPY.Close'] <-
  'Weekly.Perc.Return'

summary(weekly_returns)

weekly_returns_chart <- ggplot(data = weekly_returns,
                               aes(x = Weekly.Perc.Return)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "darkcyan",
    color = "gray78",
    alpha = 0.75
  ) +
  geom_density(alpha = 0.2) +
  xlab("% Change in SPY weekly closing price") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(-20, 16, 2)) +
  ggtitle("Distribution of returns in % - S&P 500 Index
          between November 2006 and March 2016") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  geom_vline(
    aes(xintercept = mean(
      weekly_returns$Weekly.Perc.Return,
      na.rm = TRUE
    )),
    color = "brown4",
    size = 1,
    linetype = 2
  ) +
  geom_hline(yintercept = 0,
             size = 1.2,
             color = "gray48") +
  annotate(
    geom = "text",
    x = 2.8,
    y = 55,
    label = "Mean % change",
    color = "brown4 "
  )

weekly_returns_chart

# Plot weekly returns over time

ggplot(data = weekly_returns_df,
       aes(x = date, y = SPY.Close)) +
  geom_line(color = "darkslateblue") +
  xlab(" ") +
  ylab("% Change in price") +
  ggtitle("Weekly SPY % Returns") +
  geom_hline(yintercept = 0,
             color = "gray48",
             size = 1.1)





### 5. Feature engineering ===========================================================================================


# Adding returns to the data dataset
data$Price.Perc.Change <-
  ROC(data$Price, n = 1, type = "discrete") * 100
data$Price.Delta <- diff(data$Price, lag = 1)


# Adding change in net positions versus previous week
data$Speculators.Delta <- diff(data$Speculators.Net)
data$Commercials.Delta <- diff(data$Commercials.Net)
data$Others.Delta <- diff(data$Others.Net)

# Adding the change in the put-call ratio
data$Put.Call.Delta <- diff(data$Put.Call.Ratio)
data$Net.Put.Delta <- diff(data$Net.Put.Vol)

# Adding change in volume
data$Volume.Delta <- diff(data$Volume)

# Rearrange column order
data <- data[, c(
  "Price",
  "Price.Delta",
  "Price.Perc.Change",
  "Speculators.Net",
  "Commercials.Net",
  "Others.Net",
  "Speculators.Delta",
  "Commercials.Delta",
  "Others.Delta",
  "Net.Put.Vol",
  "Put.Call.Ratio",
  "Net.Put.Delta",
  "Put.Call.Delta",
  "Volume"
)]

# Shift non-price related data by one observation as data only available ex-post
data.adj <- data
data.adj$Speculators.Net <- lag(data.adj$Speculators.Net)
data.adj$Commercials.Net <- lag(data.adj$Commercials.Net)
data.adj$Others.Net <- lag(data.adj$Others.Net)
data.adj$Speculators.Delta <- diff(data.adj$Speculators.Net)
data.adj$Commercials.Delta <- diff(data.adj$Commercials.Net)
data.adj$Others.Delta <- diff(data.adj$Others.Net)
data.adj$Net.Put.Vol <- lag(data.adj$Net.Put.Vol)
data.adj$Put.Call.Ratio <- lag(data.adj$Put.Call.Ratio)
data.adj$Net.Put.Delta <- diff(data.adj$Net.Put.Vol)
data.adj$Put.Call.Delta <- diff(data.adj$Put.Call.Ratio)
data.adj$Volume <- lag(data.adj$Volume)
data.adj$Volume.Delta <- diff(data.adj$Volume)

#Plot correlations between variables
data.adj.df <- data.frame(data.adj, Date = index(data.adj))
ggscatmat(data.adj.df, columns = 3:15, alpha = 1 / 10)

### 6. Average Approach ===========================================================================================


## Create training and test data

# Add previous change in price as a variable

data.adj$Previous.Price.Perc.Delta <-
  lag(data.adj$Price.Perc.Change)
data$Previous.Price.Perc.Delta <- lag(data$Price.Perc.Change)

training <- data.adj["2006-11-10/2012-11-09"]
test <- data.adj["2012-11-16/2016-03-25"]

# Remove first two rows of training data as contains NAs, which could interfere with models
training <- training[-c(1, 2)]

# Keep the start and end of the training and test sets as variables for reference
training_start_date <- as.Date("2006/11/24")
training_end_date <- as.Date("2012/11/09")
test_start_date <- as.Date("2012/11/16")
test_end_date <- as.Date("2016/03/25")


# Model the average approach on the training set
average_approach <-
  as.data.frame(seq(training_start_date, training_end_date, 7))
colnames(average_approach) <- "Date"
average_approach$Average.Predicted.Perc.Change <-
  mean(training$Price.Perc.Change, na.rm = TRUE)
average_approach$Actual.Perc.Change <- training$Price.Perc.Change
average_approach$Actual.Price <- training$Price
average_approach$Average.Predicted.Price <-
  lag(average_approach$Actual.Price) *
  (1 + average_approach$Average.Predicted.Perc.Change / 100)
average_approach$Perc.Residuals <-
  average_approach$Average.Predicted.Perc.Change - average_approach$Actual.Perc.Change
average_approach$Price.Residuals <-
  average_approach$Average.Predicted.Price - average_approach$Actual.Price

sse.avg.train <-
  sum(average_approach$Perc.Residuals ^ 2, na.rm = TRUE)
rmse.avg.train <-
  sqrt(mean(average_approach$Perc.Residuals ^ 2, na.rm = TRUE))

# Model the average approach on the test set
average_approach_test <-
  as.data.frame(seq(test_start_date, test_end_date, 7))
colnames(average_approach_test) <- "Date"
average_approach_test$Average.Predicted.Perc.Change <-
  mean(test$Price.Perc.Change, na.rm = TRUE)
average_approach_test$Actual.Perc.Change <- test$Price.Perc.Change
average_approach_test$Actual.Price <- test$Price
average_approach_test$Average.Predicted.Price <-
  lag(average_approach_test$Actual.Price) *
  (1 + average_approach_test$Average.Predicted.Perc.Change / 100)
average_approach_test$Perc.Residuals <-
  average_approach_test$Average.Predicted.Perc.Change - average_approach_test$Actual.Perc.Change
average_approach_test$Price.Residuals <-
  average_approach_test$Average.Predicted.Price - average_approach_test$Actual.Price

sse.avg.test <-
  sum(average_approach_test$Perc.Residuals ^ 2, na.rm = TRUE)
rmse.avg.test <-
  sqrt(mean(average_approach_test$Perc.Residuals ^ 2, na.rm = TRUE))

# Merge datasets
average_results <-
  merge(average_approach, average_approach_test, all = TRUE)

# Plot predictions one step ahead vs. actuals
average_chart <- ggplot(data = average_results,
                        aes(x = Date)) +
  geom_line(aes(y = Actual.Price, color = "darkslateblue"),
            alpha = 0.8,
            size = 0.5) +
  geom_line(
    aes(y = Average.Predicted.Price, color = "red"),
    alpha = 0.8,
    size = 0.5,
    linetype = 2
  ) +
  labs(x = "", y = "Price per Share, $") +
  ggtitle("Average Approach - Actual vs. Predicted Prices") +
  scale_colour_manual(
    name = " ",
    values = c('darkslateblue' = 'darkslateblue',
               'red' = 'red'),
    labels = c('Actual Prices', 'Predicted Prices')
  ) +
  theme(legend.position = "bottom") +
  annotate(
    "rect",
    xmin = test_start_date,
    xmax = test_end_date,
    ymin = 50,
    ymax = 220,
    alpha = 0.2,
    fill = "chartreuse4"
  ) +
  annotate(
    "text",
    x = test_start_date + 250,
    y = 210,
    label = "Test Period",
    color = "darkgreen"
  ) +
  geom_hline(yintercept = 50,
             size = 1.2,
             colour = "cornsilk4") +
  xlim(training_start_date, test_end_date)

average_chart

# Subset data into training and test set
avg.train <- subset(average_results, Date < test_start_date)
avg.test <- subset(average_results, Date >= test_start_date)

# Compute percent correct directions
avg.pos.cor <- length(avg.train$Average.Predicted.Perc.Change
                      [avg.train$Average.Predicted.Perc.Change >= 0 &
                        avg.train$Actual.Perc.Change >= 0])

avg.neg.cor <- length(avg.train$Average.Predicted.Perc.Change
                      [avg.train$Average.Predicted.Perc.Change < 0 &
                        avg.train$Actual.Perc.Change < 0])

avg.obs <- nrow(avg.train)

perc.direction.call <- (avg.pos.cor + avg.neg.cor) / avg.obs

avg.cor.train <- perc.direction.call

# Repeat for test data
avg.pos.cor <- length(avg.test$Average.Predicted.Perc.Change
                      [avg.test$Average.Predicted.Perc.Change >= 0 &
                        avg.test$Actual.Perc.Change >= 0])

avg.neg.cor <- length(avg.test$Average.Predicted.Perc.Change
                      [avg.test$Average.Predicted.Perc.Change < 0 &
                        avg.test$Actual.Perc.Change < 0])

avg.obs <- nrow(avg.test)

perc.direction.call <- (avg.pos.cor + avg.neg.cor) / avg.obs

avg.cor.test <- perc.direction.call

### 7. Naive Approach ===========================================================================================

# Model the naive approach on the training set
naive_approach <-
  as.data.frame(seq(training_start_date, training_end_date, 7))
colnames(naive_approach) <- "Date"
naive_approach$Naive.Predicted.Perc.Change <-
  lag(training$Price.Perc.Change)
naive_approach$Actual.Perc.Change <- training$Price.Perc.Change
naive_approach$Actual.Price <- training$Price
naive_approach$Naive.Predicted.Price <-
  lag(naive_approach$Actual.Price) *
  (1 + naive_approach$Naive.Predicted.Perc.Change / 100)
naive_approach$Perc.Residuals <-
  naive_approach$Naive.Predicted.Perc.Change - naive_approach$Actual.Perc.Change
naive_approach$Price.Residuals <-
  naive_approach$Naive.Predicted.Price - naive_approach$Actual.Price

sse.naive.train <-
  sum(naive_approach$Perc.Residuals ^ 2, na.rm = TRUE)
rmse.naive.train <-
  sqrt(mean(naive_approach$Perc.Residuals ^ 2, na.rm = TRUE))

sse.naive.train
rmse.naive.train

# Model the naive approach on the test set

naive_approach_test <-
  as.data.frame(seq(test_start_date, test_end_date, 7))
colnames(naive_approach_test) <- "Date"
naive_approach_test$Naive.Predicted.Perc.Change <-
  lag(test$Price.Perc.Change)
naive_approach_test$Actual.Perc.Change <- test$Price.Perc.Change
naive_approach_test$Actual.Price <- test$Price
naive_approach_test$Naive.Predicted.Price <-
  lag(naive_approach_test$Actual.Price) *
  (1 + naive_approach_test$Naive.Predicted.Perc.Change / 100)
naive_approach_test$Perc.Residuals <-
  naive_approach_test$Naive.Predicted.Perc.Change - naive_approach_test$Actual.Perc.Change
naive_approach_test$Price.Residuals <-
  naive_approach_test$Naive.Predicted.Price - naive_approach_test$Actual.Price

sse.naive.test <-
  sum(naive_approach_test$Perc.Residuals ^ 2, na.rm = TRUE)
rmse.naive.test <-
  sqrt(mean(naive_approach_test$Perc.Residuals ^ 2, na.rm = TRUE))

sse.naive.test
rmse.naive.test

# Merge datasets
naive_results <-
  merge(naive_approach, naive_approach_test, all = TRUE)

# Plot predictions one step ahead vs. actuals

naive_chart <- ggplot(data = naive_results,
                      aes(x = Date)) +
  geom_line(aes(y = Actual.Price, color = "darkslateblue"),
            alpha = 0.8,
            size = 0.5) +
  geom_line(
    aes(y = Naive.Predicted.Price, color = "red"),
    alpha = 0.8,
    size = 0.5,
    linetype = 2
  ) +
  labs(x = "", y = "Price per Share, $") +
  ggtitle("Naive Approach - Actual vs. Predicted Prices") +
  scale_colour_manual(
    name = " ",
    values = c('darkslateblue' = 'darkslateblue',
               'red' = 'red'),
    labels = c('Actual Prices', 'Predicted Prices')
  ) +
  theme(legend.position = "bottom") +
  annotate(
    "rect",
    xmin = test_start_date,
    xmax = test_end_date,
    ymin = 50,
    ymax = 220,
    alpha = 0.2,
    fill = "chartreuse4"
  ) +
  annotate(
    "text",
    x = test_start_date + 250,
    y = 210,
    label = "Test Period",
    color = "darkgreen"
  ) +
  geom_hline(yintercept = 50,
             size = 1.2,
             colour = "cornsilk4") +
  xlim(training_start_date, test_end_date)

naive_chart

# Subset data into training and test set
naive.train <- subset(naive_results, Date < test_start_date)
naive.test <- subset(naive_results, Date >= test_start_date)

# Compute percent correct directions
naive.pos.cor <- length(naive.train$Naive.Predicted.Perc.Change
                        [naive.train$Naive.Predicted.Perc.Change >= 0 &
                          naive.train$Actual.Perc.Change >= 0])

naive.neg.cor <- length(naive.train$Naive.Predicted.Perc.Change
                        [naive.train$Naive.Predicted.Perc.Change < 0 &
                          naive.train$Actual.Perc.Change < 0])

naive.obs <- nrow(naive.train)

perc.direction.call <- (naive.pos.cor + naive.neg.cor) / naive.obs

naive.cor.train <- perc.direction.call

# Repeat for test data
naive.pos.cor <- length(naive.test$Naive.Predicted.Perc.Change
                        [naive.test$Naive.Predicted.Perc.Change >= 0 &
                          naive.test$Actual.Perc.Change >= 0])

naive.neg.cor <- length(naive.test$Naive.Predicted.Perc.Change
                        [naive.test$Naive.Predicted.Perc.Change < 0 &
                          naive.test$Actual.Perc.Change < 0])

naive.obs <- nrow(naive.test)

perc.direction.call <- (naive.pos.cor + naive.neg.cor) / naive.obs

naive.cor.test <- perc.direction.call


### 8. Linear regression ===========================================================================================

# Model linear regression model
linear_model_raw <- lm(
  Price.Perc.Change ~
    Previous.Price.Perc.Delta +
    Speculators.Net +
    Commercials.Net +
    Others.Net +
    Speculators.Delta +
    Commercials.Delta +
    Others.Delta +
    Net.Put.Vol +
    Put.Call.Ratio +
    Net.Put.Delta +
    Put.Call.Delta +
    Volume +
    Volume.Delta,
  data = training
)

sse.lm.train_raw <- sum(linear_model_raw$residuals ^ 2)
rmse.lm.train_raw <- sqrt(mean(linear_model_raw$residuals ^ 2))

summary(linear_model_raw)

# Simplify linear model to only use significant variables
linear_model <- lm(Price.Perc.Change ~
                     Previous.Price.Perc.Delta +
                     Net.Put.Delta +
                     Put.Call.Delta,
                   data = training)

sse.lm.train <- sum(linear_model$residuals ^ 2)
rmse.lm.train <- sqrt(mean(linear_model$residuals ^ 2))

summary(linear_model)

# Test linear model out of sample

test.df <- data.frame(test, Date = index(test))
predict_linear <- predict(linear_model, newdata = test.df)

sse.lm.test <- sum((test.df$Price.Perc.Change - predict_linear) ^ 2)
rmse.lm.test <-
  sqrt(mean((
    test.df$Price.Perc.Change - predict_linear
  ) ^ 2))

# Define training and testing start and end dates
training_start_date <- as.Date("2006/11/24")
training_end_date <- as.Date("2012/11/09")
test_start_date <- as.Date("2012/11/16")
test_end_date <- as.Date("2016/03/25")

# Create training plotting data
training_results <- as.data.frame(fitted(linear_model))
colnames(training_results) <- "Linear.Predicted.Perc.Change"
training_results$Date <-
  seq(training_start_date, training_end_date, 7)
training_results$Actual.Perc.Change <- training$Price.Perc.Change
training_results$Actual.Price <- training$Price
training_results$Linear.Predicted.Price <-
  lag(training_results$Actual.Price) *
  (1 + training_results$Linear.Predicted.Perc.Change / 100)
training_results <- training_results %>% select(
  Date,
  Actual.Price,
  Actual.Perc.Change,
  Linear.Predicted.Perc.Change,
  Linear.Predicted.Price
)

# Create test plotting data
test_results <- as.data.frame(predict_linear)
colnames(test_results) <- "Linear.Predicted.Perc.Change"
test_results$Date <- seq(test_start_date, test_end_date, 7)
test_results$Actual.Perc.Change <- test$Price.Perc.Change
test_results$Actual.Price <- test$Price
test_results$Linear.Predicted.Price <-
  lag(test_results$Actual.Price) *
  (1 + test_results$Linear.Predicted.Perc.Change / 100)
test_results <-
  test_results %>%  select(
    Date,
    Actual.Price,
    Actual.Perc.Change,
    Linear.Predicted.Perc.Change,
    Linear.Predicted.Price
  )

# Merge datasets
results <- merge(training_results, test_results, all = TRUE)
results$Perc.Residuals <-
  results$Linear.Predicted.Perc.Change - results$Actual.Perc.Change

# Plot predictions one step ahead vs. actuals
lm_chart <- ggplot(data = results,
                   aes(x = Date)) +
  geom_line(aes(y = Actual.Price, color = "darkslateblue"),
            alpha = 0.8,
            size = 0.5) +
  geom_line(
    aes(y = Linear.Predicted.Price, color = "red"),
    alpha = 0.8,
    size = 0.5,
    linetype = 2
  ) +
  labs(x = "", y = "Price per Share, $") +
  ggtitle("Multiple Linear Regression - Actual vs. Predicted Prices") +
  scale_colour_manual(
    name = " ",
    values = c('darkslateblue' = 'darkslateblue',
               'red' = 'red'),
    labels = c('Actual Prices', 'Predicted Prices')
  ) +
  theme(legend.position = "bottom") +
  annotate(
    "rect",
    xmin = test_start_date,
    xmax = test_end_date,
    ymin = 50,
    ymax = 220,
    alpha = 0.2,
    fill = "chartreuse4"
  ) +
  annotate(
    "text",
    x = test_start_date + 250,
    y = 210,
    label = "Test Period",
    color = "darkgreen"
  ) +
  geom_hline(yintercept = 50,
             size = 1.2,
             colour = "cornsilk4") +
  xlim(training_start_date, test_end_date)

lm_chart

# Test percentage of correct directional calls
# Subset data into training and test set
lm.train <- subset(results, Date < test_start_date)
lm.test <- subset(results, Date >= test_start_date)

# Compute percent correct directions
lm.pos.cor <- length(lm.train$Linear.Predicted.Perc.Change
                     [lm.train$Linear.Predicted.Perc.Change >= 0 &
                       lm.train$Actual.Perc.Change >= 0])

lm.neg.cor <- length(lm.train$Linear.Predicted.Perc.Change
                     [lm.train$Linear.Predicted.Perc.Change < 0 &
                       lm.train$Actual.Perc.Change < 0])

lm.obs <- nrow(lm.train)

perc.direction.call <- (lm.pos.cor + lm.neg.cor) / lm.obs

lm.cor.train <- perc.direction.call

# Repeat for test data
lm.pos.cor <- length(lm.test$Linear.Predicted.Perc.Change
                     [lm.test$Linear.Predicted.Perc.Change >= 0 &
                       lm.test$Actual.Perc.Change >= 0])

lm.neg.cor <- length(lm.test$Linear.Predicted.Perc.Change
                     [lm.test$Linear.Predicted.Perc.Change < 0 &
                       lm.test$Actual.Perc.Change < 0])

lm.obs <- nrow(lm.test)

perc.direction.call <- (lm.pos.cor + lm.neg.cor) / lm.obs

lm.cor.test <- perc.direction.call


### 9. Regression Tree ===========================================================================================

# Run regression tree model

library(rpart)
library(rpart.plot)

training.df <- data.frame(training, Date = index(training))

tree.fit <-
  rpart(
    Price.Perc.Change ~ Speculators.Net + Commercials.Net +
      Others.Net + Speculators.Delta +
      Commercials.Delta + Others.Delta +
      Net.Put.Vol + Put.Call.Ratio +
      Net.Put.Delta + Put.Call.Delta +
      Previous.Price.Perc.Delta +
      Volume +
      Volume.Delta,
    data = training,
    method = "anova"
  )

prp(tree.fit)
printcp(tree.fit)

#Pruning the tree
pruned.tree <- prune(tree.fit, cp = 0.013639) #from cptable
prp(pruned.tree)

# Calculate RMSE and SSE
tree.pred <- predict(pruned.tree, newdata = test)
sse.tree.test <- sum((tree.pred - test$Price.Perc.Change) ^ 2)
rmse.tree.test <- sqrt(mean((tree.pred - test$Price.Perc.Change) ^ 2))
pruned.tree.fitted <- predict(pruned.tree, newdata = training)
sse.tree.train <-
  sum((pruned.tree.fitted - training$Price.Perc.Change) ^ 2)
rmse.tree.train <-
  sqrt(mean((
    pruned.tree.fitted - training$Price.Perc.Change
  ) ^ 2))

# Create training plotting data for tree
training_results_tree <- as.data.frame(pruned.tree.fitted)
colnames(training_results_tree) <- "Tree.Predicted.Perc.Change"
training_results_tree$Date <-
  seq(training_start_date, training_end_date, 7)
training_results_tree$Actual.Perc.Change <-
  training$Price.Perc.Change
training_results_tree$Actual.Price <- training$Price
training_results_tree$Tree.Predicted.Price <-
  lag(training_results_tree$Actual.Price) *
  (1 + training_results_tree$Tree.Predicted.Perc.Change / 100)
training_results_tree <-
  training_results_tree %>% select(
    Date,
    Actual.Price,
    Actual.Perc.Change,
    Tree.Predicted.Perc.Change,
    Tree.Predicted.Price
  )

# Create test plotting data
test_results_tree <- as.data.frame(tree.pred)
colnames(test_results_tree) <- "Tree.Predicted.Perc.Change"
test_results_tree$Date <- seq(test_start_date, test_end_date, 7)
test_results_tree$Actual.Perc.Change <- test$Price.Perc.Change
test_results_tree$Actual.Price <- test$Price
test_results_tree$Tree.Predicted.Price <-
  lag(test_results_tree$Actual.Price) *
  (1 + test_results_tree$Tree.Predicted.Perc.Change / 100)
test_results_tree <-
  test_results_tree %>%  select(
    Date,
    Actual.Price,
    Actual.Perc.Change,
    Tree.Predicted.Perc.Change,
    Tree.Predicted.Price
  )

# Merge datasets
results_tree <-
  merge(training_results_tree, test_results_tree, all = TRUE)
results_tree$Perc.Residuals <-
  results_tree$Tree.Predicted.Perc.Change - results_tree$Actual.Perc.Change

# Plot predictions one step ahead vs. actuals
tree_chart <- ggplot(data = results_tree,
                     aes(x = Date)) +
  geom_line(aes(y = Actual.Price, color = "darkslateblue"),
            alpha = 0.8,
            size = 0.5) +
  geom_line(
    aes(y = Tree.Predicted.Price, color = "red"),
    alpha = 0.8,
    size = 0.5,
    linetype = 2
  ) +
  labs(x = "", y = "Price per Share, $") +
  ggtitle("Regression Tree - Actual vs. Predicted Prices") +
  scale_colour_manual(
    name = " ",
    values = c('darkslateblue' = 'darkslateblue',
               'red' = 'red'),
    labels = c('Actual Prices', 'Predicted Prices')
  ) +
  theme(legend.position = "bottom") +
  annotate(
    "rect",
    xmin = test_start_date,
    xmax = test_end_date,
    ymin = 50,
    ymax = 220,
    alpha = 0.2,
    fill = "chartreuse4"
  ) +
  annotate(
    "text",
    x = test_start_date + 250,
    y = 210,
    label = "Test Period",
    color = "darkgreen"
  ) +
  geom_hline(yintercept = 50,
             size = 1.2,
             colour = "cornsilk4") +
  xlim(training_start_date, test_end_date)

tree_chart

# Subset data into training and test set
tree.train <- subset(results_tree, Date < test_start_date)
tree.test <- subset(results_tree, Date >= test_start_date)

# Compute percent correct directions
tree.pos.cor <- length(tree.train$Tree.Predicted.Perc.Change
                       [tree.train$Tree.Predicted.Perc.Change >= 0 &
                         tree.train$Actual.Perc.Change >= 0])

tree.neg.cor <- length(tree.train$Tree.Predicted.Perc.Change
                       [tree.train$Tree.Predicted.Perc.Change < 0 &
                         tree.train$Actual.Perc.Change < 0])

tree.obs <- nrow(tree.train)

perc.direction.call <- (tree.pos.cor + tree.neg.cor) / tree.obs

tree.cor.train <- perc.direction.call

# Repeat for test data
tree.pos.cor <- length(tree.test$Tree.Predicted.Perc.Change
                       [tree.test$Tree.Predicted.Perc.Change >= 0 &
                         tree.test$Actual.Perc.Change >= 0])

tree.neg.cor <- length(tree.test$Tree.Predicted.Perc.Change
                       [tree.test$Tree.Predicted.Perc.Change < 0 &
                         tree.test$Actual.Perc.Change < 0])

tree.obs <- nrow(tree.test)

perc.direction.call <- (tree.pos.cor + tree.neg.cor) / tree.obs

tree.cor.test <- perc.direction.call

### 10. Random Forest ===========================================================================================


library(randomForest)

set.seed(1983)

randomForest.Fit <- randomForest(
  Price.Perc.Change ~
    Speculators.Net + Commercials.Net +
    Others.Net + Speculators.Delta +
    Commercials.Delta + Others.Delta +
    Net.Put.Vol + Put.Call.Ratio +
    Net.Put.Delta + Put.Call.Delta +
    Volume +
    Volume.Delta +
    Previous.Price.Perc.Delta,
  data = training,
  na.action = na.omit,
  importance = TRUE,
  ntree = 5000
)


rforest.imp <-
  as.data.frame(cbind(rownames(importance(randomForest.Fit)),
                      importance(randomForest.Fit)))
colnames(rforest.imp) <- c("Variable", "Perc.Inc.MSE", "IncNodePurity")
rforest.imp$Perc.Inc.MSE <- as.character(rforest.imp$Perc.Inc.MSE)
rforest.imp$Perc.Inc.MSE <- as.numeric(rforest.imp$Perc.Inc.MSE)
rforest.imp <- rforest.imp %>%
  arrange(desc(Perc.Inc.MSE))

# Change the display order of variables for the chart
rforest.imp$varorder <- reorder(rforest.imp$Variable,
                                rforest.imp$Perc.Inc.MSE)

rforest.chart <- ggplot(data = rforest.imp) +
  geom_point(
    aes(x = Perc.Inc.MSE, y = varorder),
    color = "darkcyan",
    pch = 19,
    size = 2
  ) +
  labs(title = "Random Forest Feature Importance",
       x = "% Increase in Mean Squared Error by feature permutations", y = " ") +
  scale_x_continuous(breaks = seq(-12, 16, 2))

rforest.chart

randomForest.pred <- predict(randomForest.Fit, newdata = test)

sse.rforest.test <-
  sum((randomForest.pred - test$Price.Perc.Change) ^ 2)
rmse.rforest.test <-
  sqrt(mean((
    randomForest.pred - test$Price.Perc.Change
  ) ^ 2))

# Plotting forest results
randomForest.fitted <- predict(randomForest.Fit, newdata = training)
sse.rforest.train <-
  sum((randomForest.fitted - training$Price.Perc.Change) ^ 2)
rmse.rforest.train <-
  sqrt(mean((
    randomForest.fitted - training$Price.Perc.Change
  ) ^ 2))


# Create training plotting data
training_results_forest <- as.data.frame(randomForest.fitted)
colnames(training_results_forest) <- "Forest.Predicted.Perc.Change"
training_results_forest$Date <-
  seq(training_start_date, training_end_date, 7)
training_results_forest$Actual.Perc.Change <-
  training$Price.Perc.Change
training_results_forest$Actual.Price <- training$Price
training_results_forest$Forest.Predicted.Price <-
  lag(training_results_forest$Actual.Price) *
  (1 + training_results_forest$Forest.Predicted.Perc.Change / 100)
training_results_forest <-
  training_results_forest %>% select(
    Date,
    Actual.Price,
    Actual.Perc.Change,
    Forest.Predicted.Perc.Change,
    Forest.Predicted.Price
  )

# Create test plotting data
test_results_forest <- as.data.frame(randomForest.pred)
colnames(test_results_forest) <- "Forest.Predicted.Perc.Change"
test_results_forest$Date <- seq(test_start_date, test_end_date, 7)
test_results_forest$Actual.Perc.Change <- test$Price.Perc.Change
test_results_forest$Actual.Price <- test$Price
test_results_forest$Forest.Predicted.Price <-
  lag(test_results_forest$Actual.Price) *
  (1 + test_results_forest$Forest.Predicted.Perc.Change / 100)
test_results_forest <-
  test_results_forest %>%  select(
    Date,
    Actual.Price,
    Actual.Perc.Change,
    Forest.Predicted.Perc.Change,
    Forest.Predicted.Price
  )

# Merge datasets
results_forest <-
  merge(training_results_forest, test_results_forest, all = TRUE)
results_forest$Perc.Residuals <-
  results_forest$Forest.Predicted.Perc.Change - results_forest$Actual.Perc.Change

# Plot predictions one step ahead vs. actuals
rforest_ts <- ggplot(data = results_forest,
                     aes(x = Date)) +
  geom_line(aes(y = Actual.Price, color = "darkslateblue"),
            alpha = 0.8,
            size = 0.5) +
  geom_line(
    aes(y = Forest.Predicted.Price, color = "red"),
    alpha = 0.8,
    size = 0.5,
    linetype = 2
  ) +
  labs(x = "", y = "Price per Share, $") +
  ggtitle("Random Forest - Actual vs. Predicted Prices") +
  scale_colour_manual(
    name = " ",
    values = c('darkslateblue' = 'darkslateblue',
               'red' = 'red'),
    labels = c('Actual Prices', 'Predicted Prices')
  ) +
  theme(legend.position = "bottom") +
  annotate(
    "rect",
    xmin = test_start_date,
    xmax = test_end_date,
    ymin = 50,
    ymax = 220,
    alpha = 0.2,
    fill = "chartreuse4"
  ) +
  annotate(
    "text",
    x = test_start_date + 250,
    y = 210,
    label = "Test Period",
    color = "darkgreen"
  ) +
  geom_hline(yintercept = 50,
             size = 1.2,
             colour = "cornsilk4") +
  xlim(training_start_date, test_end_date)

rforest_ts

# Subset data into training and test set
forest.train <- subset(results_forest, Date < test_start_date)
forest.test <- subset(results_forest, Date >= test_start_date)

# Compute percent correct directions
forest.pos.cor <- length(forest.train$Forest.Predicted.Perc.Change
                         [forest.train$Forest.Predicted.Perc.Change >= 0 &
                           forest.train$Actual.Perc.Change >= 0])

forest.neg.cor <- length(forest.train$Forest.Predicted.Perc.Change
                         [forest.train$Forest.Predicted.Perc.Change < 0 &
                           forest.train$Actual.Perc.Change < 0])

forest.obs <- nrow(forest.train)

perc.direction.call <-
  (forest.pos.cor + forest.neg.cor) / forest.obs

forest.cor.train <- perc.direction.call

# Repeat for test data
forest.pos.cor <- length(forest.test$Forest.Predicted.Perc.Change
                         [forest.test$Forest.Predicted.Perc.Change >= 0 &
                           forest.test$Actual.Perc.Change >= 0])

forest.neg.cor <- length(forest.test$Forest.Predicted.Perc.Change
                         [forest.test$Forest.Predicted.Perc.Change < 0 &
                           forest.test$Actual.Perc.Change < 0])

forest.obs <- nrow(forest.test)

perc.direction.call <-
  (forest.pos.cor + forest.neg.cor) / forest.obs

forest.cor.test <- perc.direction.call

### 11. Residuals Plot ===========================================================================================

# Create residuals dataset for ease of comparison
residuals_table <- results %>% select(Date)
residuals_table$Average.Res <-  average_results$Perc.Residuals
residuals_table$Naive.Res <- naive_results$Perc.Residuals
residuals_table$Lm.Res <- results$Perc.Residuals
residuals_table$Tree.Res <- results_tree$Perc.Residuals
residuals_table$Forest.Res <- results_forest$Perc.Residuals

residuals_test_period <-
  residuals_table[residuals_table$Date > as.Date("2012-11-16"), ]

# Plotting residuals by model
residuals_chart <- ggplot(data = residuals_test_period,
                          aes(x = Date)) +
  geom_jitter(aes(y = Average.Res, color = "Average approach"),
              data = residuals_table,
              alpha = 0.8) +
  geom_jitter(aes(y = Naive.Res, color = "Naive approach"),
              data = residuals_table,
              alpha = 0.8) +
  geom_jitter(aes(y = Lm.Res, color = "Linear model"),
              data = residuals_table,
              alpha = 0.8) +
  geom_jitter(aes(y = Tree.Res, color = "Regression tree"),
              data = residuals_table,
              alpha = 0.8) +
  geom_jitter(aes(y = Forest.Res, color = "Random forest"),
              data = residuals_table,
              alpha = 0.8) +
  labs(x = "",
       y = "Residuals") +
  xlim(test_start_date, test_end_date) +
  ggtitle("Residuals by model in test period") +
  ylim(-7, 7) +
  theme(legend.position = "bottom")

residuals_chart