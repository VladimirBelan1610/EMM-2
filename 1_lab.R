#install.packages("dplyr")
#library(dplyr)
#install.packages("Deriv")
#library(Deriv)
#install.packages("stats")
#library(stats)
#install.packages("forecast")
#library(forecast)



# Параметры
n_obs <- 1000
theta_1 <- 0.3
theta_2 <- 1
theta_3 <- 3
theta_vec <- c(theta_1, theta_2, theta_3)

# Генерация AR(1) процесса
generate_ar1 <- function(param, length) {
  series <- numeric(length)
  series[1] <- rnorm(1)
  for (i in 2:length) {
    series[i] <- param * series[i-1] + rnorm(1)
  }
  series
}

# Создание данных для разных theta
data_ar1 <- generate_ar1(theta_1, n_obs)
data_ar2 <- generate_ar1(theta_2, n_obs)
data_ar3 <- generate_ar1(theta_3, n_obs)

# Визуализация данных
plot_series <- function(series) {
  plot(series, type = "l", col = "blue", 
       main = "График временного ряда", 
       xlab = "Время", ylab = "Значение")
}

plot_series(data_ar1)
plot_series(data_ar2)
plot_series(data_ar3)

# Определение функции error_sum
error_sum <- function(p, ser, st, en) {
  total <- 0
  for (i in st:en) {
    total <- total + (ser[i] - p * ser[i-1])^2
  }
  total
}

# Задание 2: МНК оценка
estimate_mnk <- function(param, series, start, end) {
  # Численная производная
  num_deriv <- function(f, x, ser, st, en) {
    delta <- 1e-5
    (f(x + delta, ser, st, en) - f(x - delta, ser, st, en)) / (2 * delta)
  }
  
  # Поиск корня
  find_zero <- function(p) num_deriv(error_sum, p, series, start, end)
  result <- uniroot(find_zero, interval = c(-10, 10))$root
  result
}

mnk_result1 <- estimate_mnk(theta_1, data_ar1, 2, n_obs)
cat("Оценка параметра МНК:", mnk_result1, "\n")

# Задание 3: Метод минимизации
minimize_mnk <- function(func, series) {
  optim_result <- optimize(error_sum, c(-10, 10), ser = series, st = 2, en = length(series))
  param_est <- optim_result$minimum
  cat("Оценка параметра:", param_est, "\n")
}

minimize_mnk(mnk_result1, data_ar1)

# Задание 4
new_theta <- 0.8
new_ar <- generate_ar1(new_theta, n_obs)

estimate_vector <- function(param, series, k_min, k_max) {
  vec <- numeric(k_max - k_min)
  for (i in seq_along(vec)) {
    vec[i] <- estimate_mnk(param, series, k_min + i, k_max)
  }
  vec
}

mnk_vec <- estimate_vector(new_theta, new_ar, 10, n_obs)
cat("Первая МНК оценка:", mnk_vec[1], "\n")
minimize_mnk(mnk_vec, new_ar)
plot_series(mnk_vec)

# Задание 5: AR(2) процесс
generate_ar2 <- function(param1, param2, length) {
  series <- numeric(length)
  series[1:2] <- rnorm(2)
  for (i in 3:length) {
    series[i] <- param1 * series[i-1] + param2 * series[i-2] + rnorm(1)
  }
  series
}

theta_new1 <- 0.4
theta_new2 <- 0.1
ar2_data <- generate_ar2(theta_new1, theta_new2, n_obs)

check_stationarity <- function(p1, p2) {
  disc <- p1^2 - 4 * p2
  if (disc < 0) {
    real_part <- p1 / 2
    imag_part <- sqrt(abs(disc)) / 2
    modulus <- sqrt(real_part^2 + imag_part^2)
    is_stationary <- modulus < 1
  } else {
    root1 <- (p1 + sqrt(disc)) / 2
    root2 <- (p1 - sqrt(disc)) / 2
    is_stationary <- abs(root1) < 1 && abs(root2) < 1
  }
  is_stationary
}

if (check_stationarity(theta_new1, theta_new2)) {
  cat("Ряд стационарен\n")
  plot_series(ar2_data)
} else {
  cat("Ряд не стационарен\n")
}

# Задание 6
param1 <- 0.6
param2 <- -0.4
series_ar2 <- generate_ar2(param1, param2, n_obs)

fitted_arima <- arima(series_ar2, order = c(2, 0, 0), include.mean = FALSE)
predictions <- forecast(fitted_arima, h = 10)
plot(predictions)