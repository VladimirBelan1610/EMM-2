# Параметры моделирования
initial_price <- 100  # Начальная цена
drift <- 0.5         # Параметр дрейфа
volatility <- 0.8    # Волатильность
time_step <- 0.01    # Шаг дискретизации
num_steps <- 1000    # Количество шагов
num_points <- num_steps + 1  # Количество временных точек

# Функция для генерации броуновского движения
simulate_brownian_motion <- function(num_points, time_step) {
  # Генерация случайных приращений
  increments <- rnorm(num_points, mean = 0, sd = sqrt(time_step))
  
  # Инициализация и заполнение вектора
  brownian <- numeric(num_points)
  for (k in 2:num_points) {
    brownian[k] <- brownian[k - 1] + increments[k]
  }
  
  brownian
}

# Функция для генерации геометрического броуновского движения
simulate_geometric_bm <- function(num_points, time_step, initial_price, drift, volatility, brownian) {
  # Инициализация вектора
  gbm <- numeric(num_points)
  gbm[1] <- initial_price
  
  # Вычисление траектории
  for (k in 1:(num_points - 1)) {
    gbm[k + 1] <- initial_price * exp((drift - volatility^2 / 2) * (k * time_step) + volatility * brownian[k + 1])
  }
  
  gbm
}

# Функция для оценки параметров из логарифмических приращений
estimate_parameters <- function(gbm, time_step) {
  # Проверка входных данных
  if (length(gbm) < 2) stop("Недостаточно данных для вычисления приращений")
  if (any(is.na(gbm)) || any(!is.finite(gbm)) || any(gbm <= 0)) {
    stop("Данные содержат NA, бесконечные или неположительные значения")
  }
  
  # Вычисление логарифмических приращений
  log_returns <- diff(log(gbm))
  
  # Проверка приращений
  if (any(is.na(log_returns)) || any(!is.finite(log_returns))) {
    stop("Логарифмические приращения содержат NA или бесконечные значения")
  }
  
  # Оценка среднего и дисперсии
  mu_est <- mean(log_returns)
  var_est <- var(log_returns)
  
  # Преобразование оценок
  vol_est_sq <- var_est / time_step
  drift_est <- mu_est / time_step + vol_est_sq / 2
  
  list(drift_est = drift_est, vol_est_sq = vol_est_sq)
}

# Задание 1: Моделирование геометрического броуновского движения
brownian_path <- simulate_brownian_motion(num_points, time_step)
gbm_path <- simulate_geometric_bm(num_points, time_step, initial_price, drift, volatility, brownian_path)

# Проверка корректности gbm_path
if (any(is.na(gbm_path)) || any(!is.finite(gbm_path)) || any(gbm_path <= 0)) {
  stop("Ошибка в траектории геометрического броуновского движения: содержит NA, бесконечные или неположительные значения")
}

# Визуализация траектории
time_grid <- seq(0, num_steps * time_step, by = time_step)
plot(time_grid, gbm_path, type = "l", col = "darkred", 
     main = "Геометрическое броуновское движение", 
     xlab = "Время", ylab = "S(t)", lwd = 1.5)
grid()

# Задание 2: Оценка параметров
params_est <- estimate_parameters(gbm_path, time_step)

# Вывод результатов
cat("Оценка дрейфа (a):", params_est$drift_est, "\n")
cat("Оценка волатильности (sigma^2):", params_est$vol_est_sq, "\n")