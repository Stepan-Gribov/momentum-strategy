library(tseries)


load ('testing_data_weekly.Rdata')
index = testing_data[,1:2]

load ('testing_data_weekly_adj.Rdata')
testing_data = testing_data11
rm(testing_data11)
df = testing_data


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



line_colors <- c("red", "blue", "green", "purple","red", "blue", "green", "purple","red", "blue", "green", "purple")
i=9
k = i
plot(index(testing_data), cumprod(testing_data[,i]+1), type = "l", col = line_colors[1], ylim = range(cumprod(testing_data[,i]+1)),
     xlab = "Дата", ylab = "Значение")
for(i in i:(i+3)) {
  lines(index(testing_data), cumprod(testing_data[,i]+1), col = line_colors[i])
}

legend("topleft", legend = colnames(testing_data[, k:(k+3)]), col = line_colors, lty = 1, cex = 0.8)









##Good single plot
{
i =c(1,5,7,9,11)
cex = 0.8
png(filename = "plots/plot_best.png", width = 1440, height = 960, res = 200)
{
par(mfrow = c(1, 1), family = "serif", mar = c(4, 4, 1, 1),cex.axis = cex, cex.lab = cex+0.1, cex.main = cex)

plot(as.Date(index$Test_end), cumprod(1 + testing_data[, i[1]]), type = "l", 
     col = "blue", lwd = 2, 
     ylim = c(0,55),
     xlab = "", ylab = "",
     las = 1.5 
     # log = "y"
     )

# Добавление остальных линий
lines(as.Date(index$Test_end), cumprod(1 + testing_data[, i[2]]), col = "red", lwd = 2)
lines(as.Date(index$Test_end), cumprod(1 + testing_data[, i[3]]), col = "green", lwd = 2)
lines(as.Date(index$Test_end), cumprod(1 + testing_data[, i[4]]), col = "purple", lwd = 2)
lines(as.Date(index$Test_end), cumprod(1 + testing_data[, i[5]]), col = "brown", lwd = 2)

mtext("Year", side = 1, line = 2)  # side = 1 указывает на нижнюю ось
mtext("Cumulative return, times", side = 2, line = 2)  # side = 2 указывает на левую ось

# Добавление сетки и вертикальных линий каждые 3 месяца
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = seq(min(as.Date(index$Test_end)), max(as.Date(index$Test_end)), by = "3 months"), col = "gray", lty = "dotted")

# Добавление легенды для идентификации линий
legend("topleft", legend = colnames(testing_data)[i], col = c("blue", "red", "green", "purple", 'brown'), lty = 1, lwd = 2, cex = cex+0.1)
}
dev.off()
}




{
i =c(1,2,3)
cex = 0.8
png(filename = "plots/plot_num_curr.png", width = 1440, height = 960, res = 200)
{
par(mfrow = c(1, 1), family = "serif", mar = c(4, 4, 1, 1),cex.axis = cex, cex.lab = cex+0.1, cex.main = cex)
plot(index(returns_xts_spot),(apply(returns_xts_spot, 1, function(row) sum(!is.na(row)))), type = "l", 
     col = "blue",
     lwd = 2,
     ylim = c(25,60),
     # main = "Number of Non-Empty Elements per Row",
     xlab = "Year", ylab = "Number of cryptocurrencies in dataframe",
     las = 1.5, # Поворот меток по оси y для лучшей читаемости
     cex.axis = 1, # Размер шрифта меток осей
     cex.lab = 1, # Размер шрифта подписей осей
     cex.main = 1) # Размер шрифта заголовка
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted") # Сетка по вертикали
abline(v = seq(min(index(returns_xts_spot)), max(index(returns_xts_spot)), by = "3 month"), col = "gray", lty = "dotted") # Вертикальные линии сетки
}
dev.off()
}



cumprod(1 + testing_data)




# Функция для вычисления накопленных доходностей
calculate_cumulative_returns <- function(returns) {
  cumprod(1 + returns)
}

# Функция для вычисления максимальной просадки
calculate_max_drawdown <- function(cumulative_returns) {
  peak <- cumulative_returns[1]
  max_drawdown <- 0
  for (i in 2:length(cumulative_returns)) {
    if (cumulative_returns[i] > peak) {
      peak <- cumulative_returns[i]
    }
    drawdown <- (peak - cumulative_returns[i]) / peak
    if (drawdown > max_drawdown) {
      max_drawdown <- drawdown
    }
  }
  return(max_drawdown)
}

# Функция для вычисления продолжительности максимальной просадки
calculate_max_drawdown_duration <- function(cumulative_returns) {
  peak <- cumulative_returns[1]
  max_duration <- 0
  current_duration <- 0
  for (i in 2:length(cumulative_returns)) {
    if (cumulative_returns[i] > peak) {
      peak <- cumulative_returns[i]
      current_duration <- 0
    } else {
      current_duration <- current_duration + 1
      if (current_duration > max_duration) {
        max_duration <- current_duration
      }
    }
  }
  return(max_duration)
}

# Применение функций к каждому столбцу датафрейма
cumulative_returns_list <- lapply(testing_data, calculate_cumulative_returns)
max_drawdowns <- sapply(cumulative_returns_list, calculate_max_drawdown)
max_drawdown_durations <- sapply(cumulative_returns_list, calculate_max_drawdown_duration)

# Вывод результатов
print(max_drawdowns)
print(max_drawdown_durations)

maxdrawdown(cumprod(1 + testing_data)[,1])$from


i= 2
1-cumprod(1 + testing_data)[maxdrawdown(cumprod(1 + testing_data)[,i])$to,1]/cumprod(1 + testing_data)[maxdrawdown(cumprod(1 + testing_data)[,i])$from,1]
-maxdrawdown(cumprod(1 + testing_data)[,i])$from + which(cumprod(1 + testing_data)[,i]>cumprod(1 + testing_data)[maxdrawdown(cumprod(1 + testing_data)[,i])$from,i])[1]



# Установка и загрузка пакета PerformanceAnalytics
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Функция для вычисления результатов для одного столбца
calculate_metrics <- function(returns, i) {
  cum_returns <- cumprod(1 + returns[, i])
  max_dd_info <- maxdrawdown(cum_returns)
  
  max_dd <- 1 - cum_returns[max_dd_info$to] / cum_returns[max_dd_info$from]
  duration <- -max_dd_info$from + which(cum_returns > cum_returns[max_dd_info$from])[1]
  
  return(c(max_dd, duration))
}

# Применение функции к каждому столбцу датафрейма
results <- t(sapply(1:ncol(testing_data), function(i) calculate_metrics(testing_data, i)))

# Создание нового датафрейма с результатами
results_df <- data.frame(Strategy = colnames(testing_data), Max_Drawdown = results[, 1], Duration = results[, 2])
results_df$Max_Drawdown = round(results_df$Max_Drawdown*100,2)
# Вывод результирующего датафрейма
print(results_df)









#####BOXPLOTS################################
{
cex = 0.8
png(filename = "plots/plot_BOX.png", width = 1440, height = 960, res = 200)
{
par(mfrow = c(1, 1), family = "serif", mar = c(4, 4, 1, 1),cex.axis = cex, cex.lab = cex+0.1, cex.main = cex)

  boxplot(testing_data[,c(1,5,7,11)]*100, 
        # main = "Boxplot of Returns for Different Strategies", 
        xlab = "Strategies", 
        ylab = "Weekly returns, %", 
        col = rainbow(ncol(df)),
        outline = FALSE)
abline(h = 0, col = "black", lty = 2)
}
dev.off()
}




