# Параметры моделирования
time_step <- 0.0001  # Шаг дискретизации
max_steps <- 1000    # Максимальное количество шагов
num_points <- max_steps + 1  # Количество временных точек

# Функция для генерации броуновского движения
generate_brownian_motion <- function(num_points, time_step) {
  # Генерация случайных приращений
  increments <- rnorm(num_points, mean = 0, sd = sqrt(time_step))
  
  # Инициализация вектора броуновского движения
  bm <- numeric(num_points)
  bm[1] <- 0  # Начальное значение
  
  # Вычисление траектории
  for (k in 2:num_points) {
    bm[k] <- bm[k - 1] + increments[k]
  }
  
  bm
}

# Функция для генерации геометрического броуновского движения
generate_geometric_bm <- function(num_points, time_step, initial_value, drift, volatility, bm) {
  # Инициализация вектора
  gbm <- numeric(num_points)
  gbm[1] <- initial_value
  
  # Вычисление значений
  for (k in 1:(num_points - 1)) {
    gbm[k + 1] <- initial_value * exp((drift - volatility^2 / 2) * (k * time_step) + volatility * bm[k + 1])
  }
  
  gbm
}

# Задание 2: Моделирование одной траектории броуновского движения
cat("Формула броуновского движения:\n")
cat("B_{t + Δt} = B_t + ε_{t + Δt}, где ε_{t + Δt} ~ N(0, Δt)\n\n")

brownian_path <- generate_brownian_motion(num_points, time_step)

# Визуализация траектории
time_grid <- seq(0, max_steps * time_step, by = time_step)
plot(time_grid, brownian_path, type = "l", col = "darkgreen", 
     main = "Траектория броуновского движения", 
     xlab = "Время", ylab = "B(t)", lwd = 1.5)
grid()

# Задание 3: Ансамбль траекторий броуновского движения
num_paths <- 200  # Количество траекторий
cat("Правило трех сигм:\n")
cat("-3√t ≤ B_t ≤ 3√t\n\n")

# Инициализация пустого графика
plot(time_grid, numeric(num_points), type = "n", 
     main = "Ансамбль траекторий броуновского движения", 
     xlab = "Время", ylab = "B(t)", ylim = c(-3, 3))

# Генерация и отрисовка траекторий
for (i in 1:num_paths) {
  bm_path <- generate_brownian_motion(num_points, time_step)
  lines(time_grid, bm_path, col = adjustcolor("purple", alpha.f = 0.1))
}

# Добавление границ трех сигм
sigma_bounds <- 3 * sqrt(time_grid)
lines(time_grid, sigma_bounds, col = "red", lty = 2, lwd = 2)
lines(time_grid, -sigma_bounds, col = "red", lty = 2, lwd = 2)

# Задание 5: Моделирование геометрического броуновского движения
initial_price <- 1  # Начальная цена
drift <- 0.5       # Параметр дрейфа
volatility <- 0.9  # Волатильность

cat("Формула геометрического броуновского движения:\n")
cat("S_t = S_0 * exp((a - σ²/2) * t + σ * B_t)\n\n")

gbm_path <- generate_geometric_bm(num_points, time_step, initial_price, drift, volatility, brownian_path)

# Визуализация
plot(time_grid, gbm_path, type = "l", col = "darkblue", 
     main = "Геометрическое броуновское движение", 
     xlab = "Время", ylab = "S(t)", ylim = c(0, 2), lwd = 1.5)
grid()

# Задание 6: Ансамбль геометрического броуновского движения
cat("Формула для ансамбля геометрического броуновского движения:\n")
cat("S_t = S_0 * exp((a - σ²/2) * t + σ * B_t)\n\n")

# Инициализация пустого графика
plot(time_grid, numeric(num_points), type = "n", 
     main = "Ансамбль геометрического броуновского движения", 
     xlab = "Время", ylab = "S(t)", ylim = c(0, 2))

# Генерация и отрисовка траекторий
for (i in 1:num_paths) {
  bm_path <- generate_brownian_motion(num_points, time_step)
  gbm_path <- generate_geometric_bm(num_points, time_step, initial_price, drift, volatility, bm_path)
  lines(time_grid, gbm_path, col = adjustcolor("orange", alpha.f = 0.1))
}