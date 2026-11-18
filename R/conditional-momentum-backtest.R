library(tidyr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(lubridate)
library(NMOF)
library(progress)



data_spot <- read.csv("data/top_100_cap_from_2020-01-01_to_2024-02-28.csv")
# data_spot <- read.csv("data/1H_top_100_cap_from_2020-01-01_to_2024-02-28.csv")

data_spot=data_spot[-1]

price_spot = data_spot[,c(1,2,6)]
price_wide_spot <- reshape(price_spot, idvar = "Open.Time", timevar = "Symbol", direction = "wide")
names(price_wide_spot) = c('moment',unique(price_spot$Symbol))

# price_wide = price_wide[,c(1:21)] #only top 10
prices_xts_spot <- xts(price_wide_spot[, -1], order.by = as.POSIXct(price_wide_spot$moment))
prices_xts_spot = prices_xts_spot[,!(colSums(is.na(prices_xts_spot['2023-01-01/2024-02-28'])) > 0)]
returns_xts_spot = CalculateReturns(prices_xts_spot, method="simple")[-1,]

# plot(returns_xts_spot$BTCUSDT)
rm(list = c('data_spot','price_spot','price_wide_spot','prices_xts_spot'))

funding_rate <- read.csv("data/funding_rates_from_2020-01-01_to_2024-02-28.csv")
funding_rate=funding_rate[-1]
funding_rate <- subset(funding_rate, as.Date(funding_rate$fundingTime) <= as.Date("2024-02-28"))

funding_rate_wide <- reshape(funding_rate, idvar = "fundingTime", timevar = "Symbol", direction = "wide")
funding_rate_wide <- funding_rate_wide[!is.na(funding_rate_wide$fundingRate.BTCUSDT), ]
names(funding_rate_wide) = c('moment',unique(funding_rate$Symbol))


# price_wide = price_wide[,c(1:21)] #only top 10
funding_rate_xts <- xts(funding_rate_wide[, -1], order.by = as.POSIXct(funding_rate_wide$moment))
funding_rate_xts = apply.daily(funding_rate_xts, mean)
index(funding_rate_xts) <- as.Date(index(funding_rate_xts))
# plot(funding_rate_xts$BTCUSDT)

rm(list = c('funding_rate','funding_rate_wide'))

#FEATURES CALCULATION###############################################
##FORWADR DISCOUNTIG CALCULATION
{
AFD_xts = funding_rate_xts*3*365
# plot(as.Date(index(AFD_xts)),as.numeric(AFD_xts$BTCUSDT), type = 'l', col = 'blue')
# abline(h = 0, col = 'red')
}


##CROSS-SECTION RETURN DISPERSION
cross_sect_vol = function(returns_xts_spot,dt_start,dt_end){
  mean_return <- apply((returns_xts_spot[paste0(dt_start,'/',dt_end)]), 2, mean, na.rm = TRUE)
  cross_sect_vol = sd(mean_return, na.rm = TRUE)
return(cross_sect_vol)
}
#CS_RD indicator function
cross_sect_indicator = function(returns_xts_spot,dt_start,dt_end){
  dt_start_internal <- dt_end - days(90) #Num days for CS-RD reference calculation
  dt_start_internal <- max(dt_start_internal, ymd('2020-01-01'))
return(cross_sect_vol(returns_xts_spot, dt_start_internal, dt_end))  
}

##FX volatility
fx_volatolity = function(returns_xts_spot,dt_start,dt_end){
  mean_abs_ret_by_row = xts(apply((returns_xts_spot)^2, 1, mean, na.rm = TRUE), order.by = index(returns_xts_spot))
  names(mean_abs_ret_by_row) = c('mean_abs_ret_by_row')
  fx_volatility = mean(mean_abs_ret_by_row[paste0(dt_start,'/',dt_end)])
  return(fx_volatility)
}

fx_volatility_indicator = function(returns_xts_spot,dt_start,dt_end){
  dt_start_internal <- dt_end - days(90) #Num days for CS-RD reference calculation
  dt_start_internal <- max(dt_start_internal, ymd('2020-01-01'))
  return(fx_volatolity(returns_xts_spot, dt_start_internal, dt_end))  
}

rm(list = c('funding_rate_xts'))

#PORTFOLIOS##########################################################################
{
#portfolio formation function
momentum_portfolios = function(returns_xts_spot,dt_start,dt_end,decile,AFD_xts, AFR_act, OCRD_act, VOL_act)
{
  intermidiate = returns_xts_spot[paste0(dt_start,'/',dt_end)]
  month_return <- apply((intermidiate+1), 2, prod)
  mean_return <- apply((intermidiate), 2, mean, na.rm = TRUE)
  volatility_return = apply((intermidiate), 2, sd, na.rm = TRUE)
  month_return[is.na(month_return)] <- 0
  mean_return[is.na(mean_return)] <- 0
  volatility_return[is.na(volatility_return)] <- 0
  
  ####Individual conditioning on AFD
  AFD_mean = apply((AFD_xts[paste0(dt_start,'/',dt_end)]), 2, mean, na.rm = TRUE)
  AFD_mean[is.na(AFD_mean)] = 0
  
  ####
  mean_upper_decile <- quantile(month_return[month_return != 0], probs = 1-decile, na.rm = TRUE)
  mean_lower_decile <- quantile(month_return[month_return != 0], probs = decile, na.rm = TRUE)
  
  best_gainers_portfolio=ifelse(month_return > mean_upper_decile, 1, 0) #BGP
  worst_gainers_portfolio=ifelse((month_return < mean_lower_decile), ifelse(month_return !=0, 1, 0), 0) #WGP
  
  weights_BGP =best_gainers_portfolio/sum(best_gainers_portfolio)
  weights_WGP= worst_gainers_portfolio/sum(worst_gainers_portfolio)
  
  BGP_return = sum(weights_BGP*month_return)
  WGP_return = sum(weights_WGP*month_return)
  # print(paste0('sample_return: ',BGP_return+(1-WGP_return)))
  portfolio_weights = cbind(weights_BGP,weights_WGP)
  
  #Here last multipliers determines actions (0,1,1) - default
  #AFD
  portfolio_weights = portfolio_weights*ifelse(mean(AFD_mean) > 0,1,AFR_act)
  
  #C-SRD
  portfolio_weights = portfolio_weights*ifelse(cross_sect_vol(returns_xts_spot,dt_start,dt_end)
                                               >= cross_sect_indicator(returns_xts_spot,dt_start,dt_end),1,OCRD_act)
  
  #FX_volatility
  portfolio_weights = portfolio_weights*ifelse(fx_volatolity(returns_xts_spot,dt_start,dt_end)
                                               >= fx_volatility_indicator(returns_xts_spot,dt_start,dt_end),1,VOL_act)
  
  return(portfolio_weights)
}


#(-1,0,0) Yilds smoth interesting result


#Test function
portfolio_testing = function(returns_xts_spot, portfolio_weights, test_start, test_period)
{
  test_end = min(test_start + days(test_period-1), ymd('2024-02-28'))
  
  test_sample = returns_xts_spot[paste0(test_start,'/',test_end)]
  month_return_test <- apply((test_sample+1), 2, prod)
  month_return_test[is.na(month_return_test)] <- 0
  
  mult = 1
  
  BGP_return_test = sum(portfolio_weights[,1]*(mult)*month_return_test)
  WGP_return_test = sum(portfolio_weights[,2]*(-mult)*month_return_test)
  
  # print(paste0('total_test_return: ',(BGP_return_test)+(WGP_return_test)+1))
  return(c(start = as.character(test_start),end = as.character(test_end),return = as.double((BGP_return_test)+(WGP_return_test)+1)))
}
}

##TESTING SECTION###############################################################

strategy_test = function(returns_xts_spot,dt_start,dt_end,decile,AFD_xts,d_formation, d_testing){
AFR_act = 1
OCRD_act = 1
VOL_act = 1
{
  rm(list = c('stat','portfolio_weights','weights','weights_log','test_stat', 'summary','fund_rate_switch'))
  
  dt_start = '2020-01-01'
  # dt_end = '2020-01-30'
  decile = 0.2
  
  dt_start = ymd(dt_start)
  dt_end = ymd(dt_start) + days(d_formation-1)
  test_start = (dt_end)+ days(1)
  test_period = d_testing
  test_end = ymd(test_start) + days(test_period) - days(1)
}

{
  test_stat <- c(start_date = NA, end_date = NA, return = NA)
  weights_log = c(0,0,0)
  row_names = c(test_start)
  fund_rate_switch = c(0)
  cross_sect_vol_log = c(0)
  cross_sect_vol_switch = c(0)
  fx_vol_log = c(0)
  fx_vol_switch = c(0)
  
  pb <- progress_bar$new(
    format = "Testing [:bar] :percent :eta",
    total = (round(as.numeric(difftime(as.Date("2024-02-28"), as.Date(dt_start), units = "days"))/test_period)-30/test_period+1), clear = FALSE, width = 60
  )
  
  for (i in 1:(round(as.numeric(difftime(as.Date("2024-02-28"), as.Date(dt_start), units = "days"))/test_period)-30/test_period+1)){
    portfolio_weights = momentum_portfolios(returns_xts_spot,dt_start,dt_end,decile,AFD_xts, AFR_act, OCRD_act, VOL_act)
    stat = portfolio_testing(returns_xts_spot, portfolio_weights, test_start, test_period)
    weights = portfolio_weights[,1]-portfolio_weights[,2]
    test_stat = rbind(test_stat,stat)
    weights_log = rbind(weights_log,weights)
    row_names = c(row_names, as.Date(test_end))
    rm(stat)
    #time shifts
    dt_end = ymd(dt_end) + days(test_period)
    test_start = (dt_end) + days(1)
    test_end = (test_start) + days(test_period)- days(1)
    dt_start = ymd(dt_start) + days(test_period)
    # 
    # print(c(dt_start,dt_end,test_start,test_end))
    #AFD
    fund_rate_switch = rbind(fund_rate_switch,ifelse(mean(AFD_xts[paste0(dt_start,'/',dt_end)], na.rm = TRUE) > 0, yes = 1,no = NA))
    #C-SRD
    cross_sect_vol_log = rbind(cross_sect_vol_log,cross_sect_vol(returns_xts_spot,dt_start,dt_end))
    cross_sect_vol_switch = rbind(cross_sect_vol_switch,ifelse(cross_sect_vol(returns_xts_spot,dt_start,dt_end)
                                                               >= cross_sect_indicator(returns_xts_spot,dt_start,dt_end),2,NA))
    #fx_vol
    fx_vol_log = rbind(fx_vol_log, fx_volatolity(returns_xts_spot,dt_start,dt_end))
    fx_vol_switch = rbind(fx_vol_switch,ifelse(fx_volatolity(returns_xts_spot,dt_start,dt_end)
                                                       >= fx_volatility_indicator(returns_xts_spot,dt_start,dt_end),3,NA))
    
    
    pb$tick()
  }
  test_stat = test_stat[-1,]
  weights_log = weights_log[-1,]
  fund_rate_switch = fund_rate_switch[-1]
  row_names = row_names[-1]
  weights_log = xts(weights_log, order.by = (row_names))
  fx_vol_log = xts(fx_vol_log[-1], order.by = (row_names))
  # weights_log[1,] = weights_log[2,]
  #portfolio weights for every day
  full_dates <- seq(ymd('2020-01-31'), ymd('2024-02-28'), by = "day")
  filled_weights_log <- na.locf(merge(weights_log, xts(, order.by = full_dates), all = TRUE), fromLast = TRUE)
  filled_fx_log = na.locf(merge(fx_vol_log, xts(, order.by = full_dates), all = TRUE), fromLast = TRUE)
  
  #daily portfolio returns for SR and tests
  
  #for hourly data
  # daily_returns_xts = apply.daily((returns_xts_spot+1), function(x) apply(x, 2, prod))['2020-01-31/2024-02-29']
  # index(daily_returns_xts) <- as.Date(index(daily_returns_xts))
  daily_returns_xts = returns_xts_spot
  
  dayly_portfolio_return = as.data.frame(filled_weights_log['2020-01-31/2024-02-28']) *
    as.data.frame(daily_returns_xts[index(filled_weights_log['2020-01-31/2024-02-28'])]+1)
  # dayly_portfolio_return = dayly_portfolio_return[-2,]
  dayly_portfolio_return = xts(dayly_portfolio_return, order.by = index(filled_weights_log['2020-01-31/2024-02-28']))
  dayly_portfolio_return = cbind(dayly_portfolio_return, rowSums(dayly_portfolio_return, na.rm = TRUE))
  names(dayly_portfolio_return) = c(names(dayly_portfolio_return)[-ncol(dayly_portfolio_return)],'daily_return')
  
  dayly_portfolio_return = cbind(dayly_portfolio_return,cumprod(dayly_portfolio_return$daily_return+1))
  colnames(dayly_portfolio_return) = c(colnames(dayly_portfolio_return)[-ncol(dayly_portfolio_return)], 'Cumul_return')
  
  
  dayly_portfolio_return = cbind(dayly_portfolio_return,cumprod(na.fill(rollapply(
    dayly_portfolio_return[,61]+1, width = test_period, by = test_period, FUN = prod, align = "right", fill = NA), fill = 1)))
  colnames(dayly_portfolio_return) = c(colnames(dayly_portfolio_return)[-ncol(dayly_portfolio_return)], 'Cumul_portfolio_value')
  
  #
  print(c('Total value',prod(as.double(test_stat[,3]))))
  test_stat = cbind(test_stat,cumprod(test_stat[,3]))
  colnames(test_stat)[ncol(test_stat)] = 'Cumul_return'
  

summary = data.frame(length_of_calib = as.numeric(dt_end-dt_start)+1, 
                     length_of_test = test_period, 
                     return = prod(as.numeric(test_stat[,3])),
                     return_adj = prod(dayly_portfolio_return$daily_return+1),
                     APR = ((prod(as.numeric(test_stat[,3]))^(1/50*12)-1)*100),
                     APR_adj = ((prod(dayly_portfolio_return$daily_return+1)^(1/50*12)-1)*100),
                     volatility = sd(as.numeric(test_stat[,3])),
                     volatility_adj = sd((dayly_portfolio_return$daily_return+1)),
                     Sharp_ratio = prod(as.numeric(test_stat[,3]))^(1/nrow(filled_weights_log)*test_period)/sd(as.numeric(test_stat[,3])),
                     Sharp_ratio_adj = mean(dayly_portfolio_return$daily_return)/sd(dayly_portfolio_return$daily_return)
)
print(summary)

rm(list = c('full_dates','daily_returns_xts'))
rm(list = c('weights','row_names','i'))

##USED FUNDING RATE AS SIGNAL, NOT CONTRANGO


#Main graph
{
plot(as.Date(test_stat[,2]), as.numeric(test_stat[,4]), type = "l", col = "black", ylab = "Return", xlab = 'year', ylim = c(0,max(as.numeric(test_stat[,4]))))
lines(index(weights_log), fund_rate_switch, col = 'red')
lines(as.Date(test_stat[,2]), cross_sect_vol_log[-1,]*100, col = "green")
lines(as.Date(test_stat[,2]), cross_sect_vol_switch[-1,], col = "blue")
lines(as.Date(test_stat[,2]), fx_vol_switch[-1], col = 'gray')
lines(as.Date(test_stat[,2]), fx_vol_switch[-1]* cross_sect_vol_switch[-1,]*fund_rate_switch, col = 'coral')

lines(as.Date(index(dayly_portfolio_return)), dayly_portfolio_return$Cumul_portfolio_value, col = 'violet', lwd = 2)
# lines(as.Date(index(dayly_portfolio_return)), filled_fx_log*50, col = 'brown')

}

}
  return(summary)
}



















testing_data_ext = c(0)

for (d_formation in c(7,14,30,90)){
  for (d_testing in c(1,3,5,7,14,30)){
    if (d_formation>d_testing){
output = strategy_test(returns_xts_spot,dt_start,dt_end,decile,AFD_xts,d_formation, d_testing)
output = cbind(d_formation, d_testing, output)
testing_data_ext = rbind(testing_data_ext, output)
    }
    else NA
  }
}


























load('testing_data_weekly.Rdata')

strategy_test(returns_xts_spot,dt_start,dt_end,decile,AFD_xts,d_formation, d_testing)

d_formation = 30
d_testing = 7
# install.packages("moments")
library(moments)
testing_data[,3:14] = testing_data[,3:14]-1


#cumul_returns
cumret = apply(testing_data[,3:14]+1, 2, prod, na.rm = TRUE)
#avg_returns
avgret = apply(testing_data[,3:14], 2, mean, na.rm = TRUE)
#n-trade days
ntradedays = 213-sapply(testing_data[,3:14], function(col) sum(col == 0))
#volatility
volatil = apply(testing_data[,3:14], 2, sd, na.rm = TRUE)
#skew
skew = sapply(testing_data[,3:14], skewness)
#Kurt
kurt = sapply(testing_data[,3:14], kurtosis)
#Sharp 
sharp = apply(testing_data[,3:14], 2, mean, na.rm = TRUE)/apply(testing_data[,3:14], 2, sd, na.rm = TRUE)

stats_to_export = cbind(cumret, avgret, ntradedays, volatil, skew, kurt, sharp)
stats_to_export[,2] = stats_to_export[,2]*100
stats_to_export[,4] = stats_to_export[,4]*100
stats_to_export = round(stats_to_export, 2)


#Sortino
apply(testing_data[,3:14], 2, function(x) {SortinoRatio(x, MAR = 0)})
SortinoRatio(testing_data[,3:14][,1],0)

plot(cumprod(testing_data[,8]+1), tupe = 'l')
