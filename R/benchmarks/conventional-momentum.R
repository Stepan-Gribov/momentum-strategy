library(tidyr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(lubridate)


#data <- read.csv("data/top_100_cap_from_2020-01-01_to_2024-02-28.csv")
data <- read.csv("data/1H_top_100_cap_from_2020-01-01_to_2024-02-28.csv")

data=data[-1]

price = data[,c(1,2,6)]
price_wide <- reshape(price, idvar = "Open.Time", timevar = "Symbol", direction = "wide")
names(price_wide) = c('moment',unique(price$Symbol))

price_wide = price_wide[,c(1:21)] #only top 10
prices_xts <- xts(price_wide[, -1], order.by = as.POSIXct(price_wide$moment))
prices_xts = prices_xts[,!(colSums(is.na(prices_xts['2023-01-01/2024-02-28'])) > 0)]
returns_xts = CalculateReturns(prices_xts, method="simple")[-1,]

plot(returns_xts$BTCUSDT)

momentum_portfolios = function(returns_xts,dt_start,dt_end,decile)
{
  dt_start = ymd(dt_start)
  dt_end = ymd(dt_end)
  
  intermidiate = returns_xts[paste0(dt_start,'/',dt_end)]
  month_return <- apply((intermidiate+1), 2, prod)
  mean_return <- apply((intermidiate), 2, mean, na.rm = TRUE)
  volatility_return = apply((intermidiate), 2, sd, na.rm = TRUE)
  month_return[is.na(month_return)] <- 0
  mean_return[is.na(mean_return)] <- 0
  volatility_return[is.na(volatility_return)] <- 0
  
  mean_upper_decile <- quantile(mean_return, probs = 1-decile, na.rm = TRUE)
  mean_lower_decile <- quantile(mean_return, probs = decile, na.rm = TRUE)
  
  best_gainers_portfolio=ifelse(mean_return > mean_upper_decile, 1, 0) #BGP
  worst_gainers_portfolio=ifelse(mean_return < mean_lower_decile, 1, 0) #WGP
  
  weights_BGP =best_gainers_portfolio/sum(best_gainers_portfolio)
  weights_WGP= worst_gainers_portfolio/sum(worst_gainers_portfolio)
  
  BGP_return = sum(weights_BGP*month_return)
  WGP_return = sum(weights_WGP*month_return)
  print(paste0('sample_return: ',BGP_return+(1-WGP_return)))
  portfolio_weights = cbind(weights_BGP,weights_WGP)
  return(portfolio_weights)
}

portfolio_testing = function(returns_xts, portfolio_weights, test_start, test_period)
{
  test_start = ymd(test_start)
  test_end = test_start + days(test_period)
  
  test_sample = returns_xts[paste0(test_start,'/',test_end)]
  month_return_test <- apply((test_sample+1), 2, prod)
  month_return_test[is.na(month_return_test)] <- 0
  
  BGP_return_test = sum(portfolio_weights[,1]*month_return_test)
  WGP_return_test = sum(portfolio_weights[,2]*(-1)*month_return_test)
  
  # print(paste0('total_test_return: ',(BGP_return_test)+(WGP_return_test)+1))
  return(c(start = as.character(test_start),end = as.character(test_end),return = as.double((BGP_return_test)+(WGP_return_test)+1)))
}


##TESTING SECTION

{
dt_start = '2022-10-01'
dt_end = '2022-12-31'
decile = 0.1
test_start = '2023-01-01'
test_period = 5
test_end = ymd(test_start) + days(test_period)
}
portfolio_weights = momentum_portfolios(returns_xts,dt_start,dt_end,decile)
portfolio_testing(returns_xts, portfolio_weights, test_start, test_period)



test_stat <- c(start_date = NA, end_date = NA, return = NA)
for (i in 1:11){
  portfolio_weights = momentum_portfolios(returns_xts,dt_start,dt_end,decile)
  stat = portfolio_testing(returns_xts, portfolio_weights, test_start, test_period)
  test_stat = rbind(test_stat,stat)
  rm(stat)
  dt_end = ymd(dt_end) + days(test_period)
  test_start = ymd(dt_end) + days(1)
  test_end = ymd(test_start) + days(test_period)
  dt_start = ymd(dt_start) + days(test_period+1)
}
test_stat = test_stat[-1,]
prod(as.double(test_stat[,3]))


rm(test_stat)
#GRAPH
{
  portfolio = portfolio_weights[,1]-portfolio_weights[,2]
  test_sample = returns_xts[paste0(test_start,'/',test_end)]
  
  result <- sweep(test_sample, 2, portfolio, "*")
  result[is.na(result)] <- 0
  result=cbind(result,'sum' = (rowSums(result)+1))
  result = cbind(result, "cumsum" = cumprod(result$sum))
  plot(result$sum.1)
}
