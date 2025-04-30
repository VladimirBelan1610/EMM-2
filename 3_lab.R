# Проверка и установка пакета tseries
if (!require(tseries)) install.packages("tseries")
library(tseries)

# Моделирование процесса AR(2)ARCH(3)
simulate_ar2arch3 <- function(n, ar_params, arch_params) {
  # Инициализация векторов
  series <- numeric(n)
  vol_squared <- numeric(n)
  noise <- rnorm(n, 0, 1)
  
  # Начальные значения
  vol_squared[1:3] <- var(noise)  # Начальная дисперсия
  series[1:3] <- rnorm(3)  # Случайные начальные значения
  
  # Симуляция процесса
  for (t in 4:n) {
    vol_squared[t] <- arch_params[1] + arch_params[2] * series[t-1]^2 + 
      arch_params[3] * series[t-2]^2 + arch_params[4] * series[t-3]^2
    series[t] <- ar_params[1] * series[t-1] + ar_params[2] * series[t-2] + 
      sqrt(vol_squared[t]) * noise[t]
  }
  
  list(series = series, variance = vol_squared)
}

# Прогнозирование на один шаг вперед
forecast_one_step <- function(series, train_size, test_size, ar_coefs, arch_coefs) {
  pred_series <- numeric(test_size)
  pred_vol <- numeric(test_size)
  
  # Вычисление прогнозов
  for (i in 1:test_size) {
    idx <- train_size + i
    prev_vals <- c(series[idx-1], series[idx-2])
    pred_series[i] <- sum(ar_coefs * prev_vals)
    pred_vol[i] <- sqrt(arch_coefs[1] + arch_coefs[2] * series[idx-1]^2 + 
                          arch_coefs[3] * series[idx-2]^2 + arch_coefs[4] * series[idx-3]^2)
  }
  
  list(pred_series = pred_series, pred_vol = pred_vol)
}

# Параметры модели
n_obs <- 2100
ar_coefs <- c(-0.3, 0.4)  # Коэффициенты AR(2)
arch_coefs <- c(1, 0.2, 0.1, 0.2)  # Коэффициенты ARCH(3)

# Симуляция и визуализация процесса
ar_arch_data <- simulate_ar2arch3(n_obs, ar_coefs, arch_coefs)
plot(ar_arch_data$series, type = "l", col = "darkred", main = "Процесс AR(2)ARCH(3)", 
     xlab = "Время", ylab = "Значения")

# Разделение на обучающую и тестовую выборки
train_ratio <- 20/21
train_size <- floor(train_ratio * n_obs)
test_size <- n_obs - train_size

train_data <- ar_arch_data$series[1:train_size]
test_data <- ar_arch_data$series[(train_size + 1):n_obs]

# Визуализация выборок
par(mfrow = c(2, 1))
plot(train_data, type = "l", col = "darkblue", main = "Обучающая выборка", 
     xlab = "Время", ylab = "Значения")
plot(test_data, type = "l", col = "black", main = "Тестовая выборка", 
     xlab = "Время", ylab = "Значения")
par(mfrow = c(1, 1))

# Оценка параметров модели
# AR(2) модель
ar_model <- arima(train_data, order = c(2, 0, 0))
est_ar_coefs <- coef(ar_model)[1:2]
cat("Оцененные параметры AR(2):", est_ar_coefs, "\n")

# Остатки AR(2) и их визуализация
residuals_ar <- residuals(ar_model)
plot(residuals_ar, type = "l", col = "black", main = "Остатки модели AR(2)", 
     xlab = "Время", ylab = "Остатки")

# ARCH(3) модель для остатков
arch_model <- garch(residuals_ar, order = c(0, 3))
est_arch_coefs <- coef(arch_model)
cat("Оцененные параметры ARCH(3):", est_arch_coefs, "\n")

# Визуализация оцененной дисперсии
est_variance <- fitted(arch_model)[,1]^2
plot(est_variance, type = "l", col = "red", main = "Оцененная дисперсия ARCH(3)", 
     xlab = "Время", ylab = "σ²")

# Прогнозирование и визуализация
forecasts <- forecast_one_step(ar_arch_data$series, train_size, test_size, est_ar_coefs, est_arch_coefs)
upper_bound <- forecasts$pred_series + forecasts$pred_vol
lower_bound <- forecasts$pred_series - forecasts$pred_vol

plot(test_data, type = "l", col = "blue", main = "Прогноз на один шаг", 
     xlab = "Наблюдения", ylab = "Значения", ylim = range(c(lower_bound, upper_bound, test_data)))
points(forecasts$pred_series, col = "black", pch = 20)
lines(upper_bound, col = "red", lty = 1)
lines(lower_bound, col = "red", lty = 1)
legend("topleft", legend = c("Реальные значения", "Прогноз", "Границы волатильности"), 
       col = c("blue", "black", "red"), lty = c(1, NA, 1), pch = c(NA, 20, NA))

# Анализ финансовых данных
fin_data <- read.csv("C:/Users/Vladimir Belan/OneDrive/Рабочий стол/ЭММ-2/GAZP.csv", sep = ";")

# Визуализация цен закрытия
plot(fin_data$X.TIME, fin_data$X.CLOSE, type = "l", col = "red", 
     main = "Динамика цен закрытия", xlab = "Время", ylab = "Цена")

# Вычисление и визуализация логарифмической доходности
log_returns <- diff(log(fin_data$X.CLOSE))
plot(log_returns, type = "l", col = "green", main = "Логарифмическая доходность", 
     xlab = "Время", ylab = "Доходность")

# Моделирование и прогнозирование для доходности
train_size_ret <- floor(train_ratio * length(log_returns))
test_size_ret <- length(log_returns) - train_size_ret

train_returns <- log_returns[1:train_size_ret]
test_returns <- log_returns[(train_size_ret + 1):length(log_returns)]

# Оценка AR(2) для доходности
ar_model_ret <- arima(train_returns, order = c(2, 0, 0))
est_ar_coefs_ret <- coef(ar_model_ret)[1:2]
cat("Оцененные параметры AR(2) для доходности:", est_ar_coefs_ret, "\n")

# ARCH(3) для остатков
residuals_ret <- residuals(ar_model_ret)
arch_model_ret <- garch(residuals_ret, order = c(0, 3))
est_arch_coefs_ret <- coef(arch_model_ret)
cat("Оцененные параметры ARCH(3) для доходности:", est_arch_coefs_ret, "\n")

# Визуализация дисперсии
est_variance_ret <- fitted(arch_model_ret)[,1]^2
plot(est_variance_ret, type = "l", col = "red", main = "Оцененная дисперсия ARCH(3) для доходности", 
     xlab = "Время", ylab = "σ²")

# Прогнозирование доходности
forecasts_ret <- forecast_one_step(log_returns, train_size_ret, test_size_ret, est_ar_coefs_ret, est_arch_coefs_ret)
upper_bound_ret <- forecasts_ret$pred_series + forecasts_ret$pred_vol
lower_bound_ret <- forecasts_ret$pred_series - forecasts_ret$pred_vol

plot(test_returns, type = "l", col = "blue", main = "Прогноз доходности на один шаг", 
     xlab = "Наблюдения", ylab = "Доходность", ylim = range(c(lower_bound_ret, upper_bound_ret, test_returns)))
points(forecasts_ret$pred_series, col = "black", pch = 20)
lines(upper_bound_ret, col = "red", lty = 1)
lines(lower_bound_ret, col = "red", lty = 1)
legend("topleft", legend = c("Реальные значения", "Прогноз", "Границы волатильности"), 
       col = c("blue", "black", "red"), lty = c(1, NA, 1), pch = c(NA, 20, NA))