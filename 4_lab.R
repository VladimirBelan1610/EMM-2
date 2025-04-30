# Установка и загрузка пакета stats
if (!require(stats)) install.packages("stats")
library(stats)

# Функция для моделирования банковского счета
simulate_bank_account <- function(steps, rate) {
  # Инициализация вектора для банковского счета
  balance <- numeric(steps + 1)
  balance[1] <- 1  # Начальный баланс
  
  # Вычисление значений счета
  for (n in 1:steps) {
    balance[n + 1] <- (1 + rate) * balance[n]
  }
  
  balance
}

# Функция для моделирования цены акции
simulate_stock_price <- function(steps, initial_price, down_ret, up_ret, prob_up) {
  # Инициализация вектора для цен акций
  stock_price <- numeric(steps + 1)
  stock_price[1] <- initial_price
  
  # Генерация случайных доходностей
  set.seed(123)  # Для воспроизводимости
  returns <- rbinom(steps, 1, prob_up)
  returns <- ifelse(returns == 1, up_ret, down_ret)
  
  # Вычисление цен акций
  for (n in 1:steps) {
    stock_price[n + 1] <- (1 + returns[n]) * stock_price[n]
  }
  
  stock_price
}

# Функция для расчета справедливой цены call-опциона
calculate_call_option_price <- function(initial_price, steps, down_ret, up_ret, rate, strike) {
  # Вычисление вероятностей
  p_bar <- (rate - down_ret) / (up_ret - down_ret)
  p_star <- (1 + up_ret) / (1 + rate) * p_bar
  
  # Вычисление минимального числа успехов K0
  log_term1 <- log(strike / (initial_price * (1 + down_ret)^steps))
  log_term2 <- log((1 + up_ret) / (1 + down_ret))
  K0 <- 1 + floor(log_term1 / log_term2)
  
  # Расчет цены опциона
  if (K0 > steps) {
    option_price <- 0
  } else {
    # Хвосты биномиального распределения
    binom_tail_p_star <- 1 - pbinom(K0 - 1, steps, p_star)
    binom_tail_p_bar <- 1 - pbinom(K0 - 1, steps, p_bar)
    
    # Справедливая цена call-опциона
    option_price <- initial_price * binom_tail_p_star - strike * (1 + rate)^(-steps) * binom_tail_p_bar
  }
  
  option_price
}

# Задание 1: Моделирование банковского счета
steps <- 200
interest_rate <- 0.01

bank_balance <- simulate_bank_account(steps, interest_rate)

# Визуализация банковского счета
plot(0:steps, bank_balance, type = "l", col = "darkgreen", 
     xlab = "Шаги", ylab = "Баланс", 
     main = "Динамика банковского счета (r = 0.01)")
cat("Начальный баланс B_0:", bank_balance[1], "\nКонечный баланс B_200:", bank_balance[steps + 1], "\n")

# Задание 2: Моделирование цены акции
initial_stock_price <- 1
down_return <- -0.3
up_return <- 0.8
prob_up <- 0.4

stock_prices <- simulate_stock_price(steps, initial_stock_price, down_return, up_return, prob_up)

# Визуализация траектории цены акции
plot(0:steps, stock_prices, type = "l", col = "purple", 
     xlab = "Шаги", ylab = "Цена акции", 
     main = "Траектория цены акции")
cat("Начальная цена S_0:", stock_prices[1], "\nКонечная цена S_200:", stock_prices[steps + 1], "\n")

# Задание 3: Расчет справедливой цены call-опциона
initial_stock_price <- 100
steps <- 10
down_return <- -0.3
up_return <- 0.8
interest_rate <- 0.2
strike_price <- 100

call_price <- calculate_call_option_price(initial_stock_price, steps, down_return, 
                                          up_return, interest_rate, strike_price)

# Вывод результата
cat("Справедливая цена call-опциона:", call_price, "\n")