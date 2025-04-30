# Установка и загрузка пакета tseries, если он не установлен
if (!require(tseries)) install.packages("tseries")
library(tseries)
set.seed(123)

# Моделирование процесса GARCH(1,0)
simulate_garch10 <- function(const, alpha1, n_obs) {
  if (const <= 0) stop("Константа должна быть положительной")
  if (alpha1 <= 0 || alpha1 >= 1) stop("Alpha1 должна быть в диапазоне (0, 1)")
  
  vol <- numeric(n_obs)
  series <- numeric(n_obs)
  noise <- rnorm(n_obs)
  
  vol[1] <- const / (1 - alpha1)  # Начальная волатильность
  for (t in 2:n_obs) {
    vol[t] <- const + alpha1 * noise[t-1]^2 * vol[t-1]
    series[t] <- sqrt(vol[t]) * noise[t]
  }
  
  par(mfrow = c(2, 1))
  plot(series, type = "l", col = "darkgreen", main = "Серия GARCH(1,0)", ylab = "Значение", xlab = "Время")
  plot(sqrt(vol), type = "l", col = "navy", main = "Волатильность GARCH(1,0)", ylab = "σ", xlab = "Время")
  
  list(series = series, volatility = sqrt(vol))
}

# Моделирование процесса GARCH(3,0)
simulate_garch30 <- function(const, alpha1, alpha2, alpha3, n_obs) {
  if (const <= 0) stop("Константа должна быть положительной")
  if (alpha1 <= 0 || alpha2 <= 0 || alpha3 <= 0 || (alpha1 + alpha2 + alpha3) >= 1) 
    stop("Сумма alpha1, alpha2, alpha3 должна быть меньше 1")
  
  vol <- numeric(n_obs)
  series <- numeric(n_obs)
  noise <- rnorm(n_obs)
  
  vol[1:3] <- const / (1 - alpha1 - alpha2 - alpha3)  # Начальная волатильность
  for (t in 4:n_obs) {
    vol[t] <- const + alpha1 * series[t-1]^2 + alpha2 * series[t-2]^2 + alpha3 * series[t-3]^2
    series[t] <- sqrt(vol[t]) * noise[t]
  }
  
  par(mfrow = c(2, 1))
  plot(series, type = "l", col = "darkgreen", main = "Серия GARCH(3,0)", ylab = "Значение", xlab = "Время")
  plot(sqrt(vol), type = "l", col = "navy", main = "Волатильность GARCH(3,0)", ylab = "σ", xlab = "Время")
  
  list(series = series, volatility = sqrt(vol))
}

# Моделирование процесса GARCH(1,1)
simulate_garch11 <- function(const, alpha1, beta1, n_obs) {
  if (const <= 0) stop("Константа должна быть положительной")
  if (alpha1 <= 0 || beta1 <= 0 || (alpha1 + beta1) >= 1) 
    stop("Alpha1 и beta1 должны быть положительными, а их сумма < 1")
  
  vol <- numeric(n_obs)
  series <- numeric(n_obs)
  noise <- rnorm(n_obs)
  
  vol[1] <- const / (1 - alpha1 - beta1)  # Начальная волатильность
  for (t in 2:n_obs) {
    vol[t] <- const + alpha1 * noise[t-1]^2 + beta1 * vol[t-1]
    series[t] <- sqrt(vol[t]) * noise[t]
  }
  
  par(mfrow = c(2, 1))
  plot(series, type = "l", col = "darkgreen", main = "Серия GARCH(1,1)", ylab = "Значение", xlab = "Время")
  plot(sqrt(vol), type = "l", col = "navy", main = "Волатильность GARCH(1,1)", ylab = "σ", xlab = "Время")
  
  list(series = series, volatility = sqrt(vol))
}

# Параметры для моделирования
n_obs <- 1100
const <- 0.1
alpha1 <- 0.4

# Запуск моделирования GARCH(1,0)
garch10_data <- simulate_garch10(const, alpha1, n_obs)

# Параметры для GARCH(3,0)
alpha1_g30 <- 0.3
alpha2_g30 <- 0.2
alpha3_g30 <- 0.1

# Запуск моделирования GARCH(3,0)
garch30_data <- simulate_garch30(const, alpha1_g30, alpha2_g30, alpha3_g30, n_obs)

# Параметры для GARCH(1,1)
beta1 <- 0.5

# Запуск моделирования GARCH(1,1)
garch11_data <- simulate_garch11(const, alpha1, beta1, n_obs)

# Прогнозирование для GARCH(3,0)
train_size <- 1000
train_series <- garch30_data$series[1:train_size]
test_series <- garch30_data$series[(train_size + 1):n_obs]

# Подгонка модели GARCH(3,0) на обучающих данных
garch30_fit <- garch(train_series, order = c(0, 3), trace = FALSE)
cat("Оцененные параметры GARCH(3,0):\n")
print(coef(garch30_fit))

# Функция последовательного прогнозирования для GARCH(3,0)
forecast_garch30 <- function(model, data, steps_ahead) {
  params <- coef(model)  # Извлечение оцененных параметров
  const <- params["a0"]
  alpha1 <- params["a1"]
  alpha2 <- params["a2"]
  alpha3 <- params["a3"]
  
  vol_forecast <- numeric(steps_ahead)
  series_forecast <- numeric(steps_ahead)
  noise <- rnorm(steps_ahead)
  
  # Инициализация последними тремя квадратами наблюдений
  last_squares <- tail(data, 3)^2
  vol_forecast[1] <- const + alpha1 * last_squares[3] + alpha2 * last_squares[2] + alpha3 * last_squares[1]
  series_forecast[1] <- sqrt(vol_forecast[1]) * noise[1]
  
  # Цикл последовательного прогнозирования
  for (t in 2:steps_ahead) {
    if (t == 2) {
      vol_forecast[t] <- const + alpha1 * series_forecast[t-1]^2 + alpha2 * last_squares[3] + alpha3 * last_squares[2]
    } else if (t == 3) {
      vol_forecast[t] <- const + alpha1 * series_forecast[t-1]^2 + alpha2 * series_forecast[t-2]^2 + alpha3 * last_squares[3]
    } else {
      vol_forecast[t] <- const + alpha1 * series_forecast[t-1]^2 + alpha2 * series_forecast[t-2]^2 + alpha3 * series_forecast[t-3]^2
    }
    series_forecast[t] <- sqrt(vol_forecast[t]) * noise[t]
  }
  
  list(vol_forecast = sqrt(vol_forecast), series_forecast = series_forecast)
}

# Выполнение прогноза на 100 шагов вперед
steps_ahead <- 100
forecast_result <- forecast_garch30(garch30_fit, train_series, steps_ahead)

# График квадратов наблюдений против прогнозируемой волатильности
par(mfrow = c(1, 1))
plot(test_series[1:steps_ahead]^2, type = "l", col = "blue", 
     main = "GARCH(3,0): Квадраты наблюдений против прогнозируемой волатильности", 
     ylab = "Квадраты значений / Волатильность", xlab = "Время")
lines(forecast_result$vol_forecast^2, col = "red", lty = 2)
legend("topright", legend = c("Квадраты наблюдений", "Прогнозируемая волатильность"), 
       col = c("blue", "red"), lty = c(1, 2))
