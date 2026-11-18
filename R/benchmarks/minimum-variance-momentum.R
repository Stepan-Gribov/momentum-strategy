library(tidyr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(lubridate)
library(NMOF)


data <- read.csv("data/top_100_cap_from_2020-01-01_to_2024-02-28.csv")
#data <- read.csv("data/1H_top_100_cap_from_2020-01-01_to_2024-02-28.csv")

data=data[-1]

price = data[,c(1,2,6)]
price_wide <- reshape(price, idvar = "Open.Time", timevar = "Symbol", direction = "wide")
names(price_wide) = c('moment',unique(price$Symbol))

# price_wide = price_wide[,c(1:21)] #only top 10
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
  
  #MAX return
  
  mean_upper_decile <- quantile(month_return[month_return != 0], probs = 1-decile, na.rm = TRUE)
  mean_lower_decile <- quantile(month_return[month_return != 0], probs = decile, na.rm = TRUE)

  best_gainers_portfolio=ifelse(month_return > mean_upper_decile, 1, 0) #BGP
  worst_gainers_portfolio=ifelse((month_return < mean_lower_decile), ifelse(month_return !=0, 1, 0), 0) #WGP

  #SR
  # SR = month_return/volatility_return
  # SR[is.na(SR)] <- 0
  # 
  # mean_upper_decile <- quantile(SR[SR != 0], probs = 1-decile, na.rm = TRUE)
  # mean_lower_decile <- quantile(SR[SR != 0], probs = decile, na.rm = TRUE)
  # 
  # best_gainers_portfolio=ifelse(SR > mean_upper_decile, 1, 0) #BGP
  # worst_gainers_portfolio=ifelse((SR < mean_lower_decile), ifelse(SR !=0, 1, 0), 0) #WGP
  
  #########
  
  weights_BGP =best_gainers_portfolio/sum(best_gainers_portfolio)
  weights_WGP= worst_gainers_portfolio/sum(worst_gainers_portfolio)
  
  BGP_return = sum(weights_BGP*month_return)
  WGP_return = sum(weights_WGP*month_return)
  print(paste0('sample_return: ',BGP_return+(1-WGP_return)))
  portfolio_weights = cbind(weights_BGP,weights_WGP)
  
  # portfolio_weights = min_variance(intermidiate,portfolio_weights)
  return(portfolio_weights)
}

min_variance = function(intermidiate,portfolio_weights){
  weights_BGP = portfolio_weights[,1]
  weights_WGP = portfolio_weights[,2]
  
  BGP_data = intermidiate[, names(weights_BGP)[which(weights_BGP != 0)]]
  WGP_data = intermidiate[, names(weights_WGP)[which(weights_WGP != 0)]]
  BGP_var = var(BGP_data)
  WGP_var = var(WGP_data)
  
  BGP_portfolio = minvar(BGP_var, wmin = 0.01, wmax = 0.5, method = "qp",groups = NULL, groups.wmin = NULL, groups.wmax = NULL)[1:(ncol(BGP_var))]
  WGP_portfolio = minvar(WGP_var, wmin = 0.01, wmax = 0.5, method = "qp",groups = NULL, groups.wmin = NULL, groups.wmax = NULL)[1:(ncol(WGP_var))]
  
  names(BGP_portfolio) = names(BGP_data)
  names(WGP_portfolio) = names(WGP_data)
  
  wgths_BGP = rep(0,length(weights_BGP))
  names(wgths_BGP) = names(weights_BGP)
  wgths_WGP = rep(0,length(weights_WGP))
  names(wgths_WGP) = names(weights_WGP)
  
  common_names <- intersect(names(wgths_BGP), names(BGP_portfolio))
  wgths_BGP[common_names] <- wgths_BGP[common_names] + BGP_portfolio[common_names]
  
  common_names <- intersect(names(wgths_WGP), names(WGP_portfolio))
  wgths_WGP[common_names] <- wgths_WGP[common_names] + WGP_portfolio[common_names]
  
  portfolio_weights = cbind(wgths_BGP,wgths_WGP)
  return(portfolio_weights)
}



portfolio_testing = function(returns_xts, portfolio_weights, test_start, test_period)
{
  test_start = ymd(test_start)
  test_end = test_start + days(test_period-1)
  
  test_sample = returns_xts[paste0(test_start,'/',test_end)]
  month_return_test <- apply((test_sample+1), 2, prod)
  month_return_test[is.na(month_return_test)] <- 0
  
  BGP_return_test = sum(portfolio_weights[,1]*(1)*month_return_test)
  WGP_return_test = sum(portfolio_weights[,2]*(-1)*month_return_test)
  
  # print(paste0('total_test_return: ',(BGP_return_test)+(WGP_return_test)+1))
  return(c(start = as.character(test_start),end = as.character(test_end),return = as.double((BGP_return_test)+(WGP_return_test)+1)))
}


##TESTING SECTION

{
rm(list = c('stat','portfolio_weights','weights','weights_log','test_stat', 'summary'))
  
dt_start = '2020-01-01'
dt_end = '2020-01-30'
decile = 0.2
test_start = ymd(dt_end)+ days(1)
test_period = 7
test_end = ymd(test_start) + days(test_period) - days(1)
}



{
rm(test_stat)
test_stat <- c(start_date = NA, end_date = NA, return = NA)
weights_log = c(0,0,0)

for (i in 1:(round(as.numeric(difftime(as.Date("2024-02-28"), as.Date(dt_start), units = "days"))/test_period)-30/test_period)){
  portfolio_weights = momentum_portfolios(returns_xts,dt_start,dt_end,decile)
  stat = portfolio_testing(returns_xts, portfolio_weights, test_start, test_period)
  weights = portfolio_weights[,1]-portfolio_weights[,2]
  test_stat = rbind(test_stat,stat)
  weights_log = rbind(weights_log,weights)
  rm(stat)
  dt_end = ymd(dt_end) + days(test_period)
  test_start = ymd(dt_end) + days(1)
  test_end = ymd(test_start) + days(test_period)- days(1)
  dt_start = ymd(dt_start) + days(test_period)
  print(c(dt_start,dt_end,test_start,test_end))

}
test_stat = test_stat[-1,]

print(c('Total value',prod(as.double(test_stat[,3]))))
test_stat = cbind(test_stat,cumprod(test_stat[,3]))
colnames(test_stat)[ncol(test_stat)] = 'Cumul_return'
plot(as.Date(test_stat[,2]),as.numeric(test_stat[,4]),type = 'l')

}

summary = data.frame(length_of_calib = as.numeric(dt_end-dt_start)+1, 
                     length_of_test = test_period, 
                     return = prod(as.numeric(test_stat[,3])),
                     APR = ((prod(as.numeric(test_stat[,3]))^(1/50*12)-1)*100),
                     volatility = sd(as.numeric(test_stat[,3])),
                     Sharp_ratio = prod(as.numeric(test_stat[,3]))/sd(as.numeric(test_stat[,3]))
                     )
summary

