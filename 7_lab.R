# Установка начального значения генератора случайных чисел
set.seed(123)

# Функция для моделирования процесса капитала страховой компании
simulate_capital_process <- function(initial_capital, premium_rate, claim_rate, claim_mean, max_time) {
  # Инициализация переменных
  capital <- initial_capital
  times <- c(0)
  capital_path <- c(initial_capital)
  
  # Моделирование процесса
  while (TRUE) {
    # Генерация времени до следующего страхового случая
    interclaim_time <- rexp(1, rate = claim_rate)
    current_time <- sum(times) + interclaim_time
    
    # Проверка выхода за максимальное время
    if (current_time > max_time) {
      capital <- capital + premium_rate * (max_time - sum(times))
      times <- c(times, max_time)
      capital_path <- c(capital_path, capital)
      break
    }
    
    # Генерация размера выплаты
    claim_size <- rexp(1, rate = 1 / claim_mean)
    capital <- capital + premium_rate * interclaim_time - claim_size
    times <- c(times, current_time)
    capital_path <- c(capital_path, capital)
    
    # Проверка разорения
    if (capital < 0) break
  }
  
  list(times = times, capital = capital_path)
}

# Функция для оценки вероятности разорения
estimate_ruin_probability <- function(initial_capital, premium_rate, claim_rate, claim_mean, max_time, num_sim) {
  # Проверка разорения в одной симуляции
  check_ruin_once <- function() {
    result <- simulate_capital_process(initial_capital, premium_rate, claim_rate, claim_mean, max_time)
    any(result$capital < 0)
  }
  
  # Проведение симуляций
  ruin_outcomes <- replicate(num_sim, check_ruin_once())
  mean(ruin_outcomes)
}

# Задание 1: Моделирование количества страховых случаев
claim_freq <- 2  # Средняя частота страховых случаев
time_period <- 50  # Период времени
num_simulations <- 1000

# Генерация количества страховых случаев
claim_counts <- rpois(num_simulations, claim_freq * time_period)

# Построение гистограммы и теоретического распределения
hist(claim_counts, breaks = 20, probability = TRUE, 
     main = "Распределение числа страховых случаев", 
     xlab = "Число случаев", ylab = "Плотность", 
     col = "lightblue", border = "darkblue")
curve(dpois(x, claim_freq * time_period), add = TRUE, col = "darkred", lwd = 2)
legend("topright", legend = c("Эмпирическое", "Теоретическое"), 
       col = c("darkblue", "darkred"), lty = c(1, 1))

# Задание 2: Моделирование процесса капитала
initial_capital <- 50  # Начальный капитал
premium_rate <- 1      # Доход от премий за единицу времени
claim_rate <- 0.3     # Интенсивность страховых случаев
claim_mean <- 3       # Средний размер выплаты
max_time <- 100       # Максимальное время

# Симуляция с разорением
result_ruin <- simulate_capital_process(initial_capital, premium_rate, claim_rate, claim_mean, max_time)
plot(result_ruin$times, result_ruin$capital, type = "l", col = "purple", 
     main = "Капитал компании (с разорением)", 
     xlab = "Время", ylab = "Капитал", lwd = 1.5)
grid()

# Симуляция без разорения
result_no_ruin <- simulate_capital_process(initial_capital, premium_rate, 0.1, claim_mean, max_time)
plot(result_no_ruin$times, result_no_ruin$capital, type = "l", col = "darkgreen", 
     main = "Капитал компании (без разорения)", 
     xlab = "Время", ylab = "Капитал", lwd = 1.5)
grid()

# Задание 3: Оценка вероятности разорения
initial_capital <- 100  # Начальный капитал
num_simulations <- 1000
max_time <- 1000

# Вычисление выборочной вероятности разорения
ruin_prob <- estimate_ruin_probability(initial_capital, premium_rate, claim_rate, claim_mean, max_time, num_simulations)
cat("Оценочная вероятность разорения:", ruin_prob, "\n")

# Вычисление теоретической вероятности разорения (условие Лундберга)
safety_loading <- premium_rate / (claim_rate * claim_mean) - 1
theoretical_ruin_prob <- exp(-initial_capital * (safety_loading / (1 + safety_loading)) / claim_mean)
cat("Теоретическая вероятность разорения:", theoretical_ruin_prob, "\n")