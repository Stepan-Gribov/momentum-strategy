library(tidyr)
library(car)
library(moments)
library(MASS)
library(PerformanceAnalytics)
library(stats)



load ('testing_data_weekly_adj.Rdata')
testing_data = testing_data11
rm(testing_data11)
df = testing_data

num_bootstrap_samples <- 10000
bootstrap_matrix <- matrix(nrow = num_bootstrap_samples, ncol = ncol(df))
for (i in 1:ncol(df)) {
  bootstrap_matrix[, i] <- sample(df[[i]], num_bootstrap_samples, replace = TRUE)
}
bootstrap_df <- as.data.frame(bootstrap_matrix)
colnames(bootstrap_df) <- colnames(df)



#NORMALITY TESTS

perform_shapiro_test <- function(column) {
  shapiro_result <- shapiro.test(column)
  return(shapiro_result$p.value)
}

# Применение функции к каждому столбцу
shapiro_results <- sapply(df, perform_shapiro_test)

# Вывод p-значений для каждого столбца
print(shapiro_results)


par(mfrow = c(3, 4))

# Функция для создания Q-Q графика для каждого столбца
create_qq_plot <- function(column, column_name) {
  qqnorm(column, main = paste("Q-Q Plot of", column_name))
  qqline(column, col = "red")
}

# Применение функции к каждому столбцу
for (i in 1:ncol(bootstrap_df)) {
  create_qq_plot(bootstrap_df[[i]], colnames(bootstrap_df)[i])
}
par(mfrow = c(1, 1))
hist(bootstrap_df$Conventional, breaks = 20)

sd(bootstrap_df$Conventional)*100
# install.packages("nortest")
library(nortest)

# Функция для выполнения теста Андерсона-Дарлинга для каждого столбца
perform_ad_test <- function(column) {
  ad_result <- ad.test(column)
  return(ad_result$p.value)
}

# Применение функции к каждому столбцу
ad_results <- sapply(testing_data, perform_ad_test)

# Вывод p-значений для каждого столбца
print(ad_results)



#ANOVA################################################
df_long <- gather(bootstrap_df, key = "Group", value = "Value")

# Вывод структуры длинного датафрейма для проверки
str(df_long)
anova_result <- aov(Value ~ Group, data = df_long)

# Вывод результатов ANOVA теста
summary(anova_result)


#welch_test###########################################
# Функция для выполнения одностороннего теста Уэлча и сбора статистики
perform_welch_test <- function(col1, col2) {
  test_result <- t.test(col1, col2, alternative = "less", var.equal = FALSE)
  return(list(mean1 = mean(col1), mean2 = mean(col2), 
              t_statistic = test_result$statistic, 
              degrees_of_freedom = test_result$parameter,
              p_value = test_result$p.value,
              critical_value = qt(0.95, df = test_result$parameter)))
}

# Применение функции к каждой паре колонок (первая колонка и колонки 2-12)
results <- lapply(1:12, function(i) perform_welch_test(bootstrap_df[[1]], bootstrap_df[[i]]))

# Создание датафрейма с результатами
results_df <- data.frame(
  Column = colnames(bootstrap_df)[1:12],
  Mean1 = sapply(results, function(x) x$mean1),
  Mean2 = sapply(results, function(x) x$mean2),
  T_Statistic = sapply(results, function(x) x$t_statistic),
  Degrees_of_Freedom = sapply(results, function(x) x$degrees_of_freedom),
  Critical_Value = sapply(results, function(x) x$critical_value),
  P_Value = sapply(results, function(x) x$p_value)
)
results_df[,c(2,3,4,5,6,7)]=round(results_df[,c(2,3,4,5,6,7)],3)
results_df[,5]=round(results_df[,5],0)
# Вывод результирующего датафрейма
print(results_df)



#LEVEE_TEST#####################################################
# Преобразование данных в длинный формат
df_long <- gather(bootstrap_df, key = "Group", value = "Value")

# Вывод структуры длинного датафрейма для проверки
str(df_long)

# Выполнение теста Левена
levene_test_result <- leveneTest(Value ~ Group, data = df_long)

# Вывод результатов теста Левена
print(levene_test_result)



#F-test####################################
# Функция для выполнения одностороннего F-теста
perform_f_test <- function(col1, col2) {
  f_test_result <- var.test(col1, col2, alternative = "less")
  df1 <- length(col1) - 1
  df2 <- length(col2) - 1
  critical_value <- qf(0.05, df1, df2, lower.tail = TRUE)
  return(list(
    variance1 = var(col1), 
    variance2 = var(col2), 
    f_statistic = f_test_result$statistic, 
    p_value = f_test_result$p.value,
    critical_value = critical_value
  ))
}

# Применение функции к каждой паре колонок (первая колонка и колонки 2-12)
results <- lapply(1:12, function(i) perform_f_test(bootstrap_df[[1]], bootstrap_df[[i]]))

# Создание датафрейма с результатами
results_df <- data.frame(
  Group = colnames(bootstrap_df)[1:12],
  Variance1 = sapply(results, function(x) x$variance1*100),
  Variance2 = sapply(results, function(x) x$variance2*100),
  F_Statistic = sapply(results, function(x) x$f_statistic),
  Critical_Value = sapply(results, function(x) x$critical_value),
  P_Value = 1-sapply(results, function(x) x$p_value)
  )
print(results_df)
results_df[,c(2,3,4,5,6)]=round(results_df[,c(2,3,4,5,6)],3)



######################################################################
##########FINANCIAL PERFORMANCE PART##################################
#cumul_returns
cumret = apply(testing_data+1, 2, prod, na.rm = TRUE)
#avg_returns
avgret = apply(testing_data, 2, mean, na.rm = TRUE)
#n-trade days
ntradedays = 213-sapply(testing_data, function(col) sum(col == 0))
#volatility
volatil = apply(testing_data, 2, sd, na.rm = TRUE)
#skew
skew = sapply(testing_data, skewness)
#Kurt
kurt = sapply(testing_data, kurtosis)
#Sharp 
sharp = apply(testing_data, 2, mean, na.rm = TRUE)/apply(testing_data, 2, sd, na.rm = TRUE)
#Sortino
# sortino_ratio <- function(returns, Rf = 0) {
#   mean_return <- mean(returns)
#   downside_risk <- sd(returns[returns < Rf])
#   if (downside_risk == 0) {
#     return(NA)
#   }
#   return((mean_return - Rf) / downside_risk)
# }
# sortino_ratios <- sapply(testing_data, sortino_ratio, Rf = 0)
sortino_ratios <- sapply(testing_data, function(x) {SortinoRatio(x, MAR = 0)})
#OMEGA
Omega = sapply(testing_data, theoretical_distr_fit)

avgret-1/2*volatil*volatil


stats_to_export = c(0)
stats_to_export = cbind(cumret, avgret, volatil, sharp,sortino_ratios, Omega,MPPM)
stats_to_export[,2] = stats_to_export[,2]*100
stats_to_export[,3] = stats_to_export[,3]*100
stats_to_export = round(stats_to_export, 2)

print(stats_to_export)

###################OMEGA RATIO#######################
theoretical_distr_fit = function(col){
data = (col+1)
fit <- fitdistr(data, "normal")
print(fit)
fitted_vector <- rnorm(213,mean = fit$estimate[1],  sd = fit$estimate[2])

plot(density(data), main = "Density of Real Data", xlab = "Value", ylab = "Frequency", ylim = c(0,10))
lines(density(fitted_vector), col = "red", lwd = 2, lty = 2)
probability <- pnorm(1, mean = fit$estimate[1], sd = fit$estimate[2])
Omega = (1-probability)/probability
return(Omega)
}

##MPPM###############################################
MPPM = c(NA)
gamma = 2
for (i in 1:12){
mppm <- (1/gamma) * log(mean((testing_data[,i]+1)/(testing_data[,1]+1)))^(1-gamma)
MPPM = cbind(MPPM,mppm)
}
MPPM = MPPM[2:13]
names(MPPM) = names(sharp)
print(MPPM/100)





Rf <- mean(testing_data$Conventional)

# Функция для вычисления MPPM
calculate_mppm <- function(returns, Rf = 0) {
  mu <- mean(returns)
  sigma <- sd(returns)
  T <- length(returns)
  
  mppm <- (mu - Rf) / sqrt(sigma^2 + 1/T)
  return(mppm)
}

# Применение функции к каждому столбцу датафрейма
mppm_values <- sapply(testing_data, calculate_mppm, Rf = Rf)

# Вывод результатов
print(mppm_values*100)














theta <- 2

# Функция для вычисления MPPM
calculate_mppm <- function(returns, unconditional_returns, theta) {
  T <- length(returns)
  ratio <- ((1+returns) / (unconditional_returns+1))^(theta)
  mppm <- (1 / theta) * log(mean(ratio))
  return(mppm)
}

# Применение функции к каждому столбцу датафрейма
mppm_values <- sapply(testing_data, calculate_mppm, unconditional_returns = testing_data$Conventional, theta = theta)
MPPM = mppm_values*100
# Вывод результатов
print(mppm_values*100)




