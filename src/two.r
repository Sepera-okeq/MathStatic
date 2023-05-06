# Task 2
# By sepera_okeq (Sergey Leshkevich)
# Source code license: GPLv3

# Список заданий:
# 1. оценить параметры линейной среднеквадратической регрессии;
# 2. нарисовать линию среднеквадратической регрессии поверх графика рассеяния данных;
# 3. вычислить значение регрессии в заданной точке.

file_data <- read.csv(file = '~/MathStatic/materials/6/r4z2.csv') # Считывание из csv файла данных X and Y

x <- file_data$X;
y <- file_data$Y;

plot(x, y)

# Equivalent
M <- cbind(x, y)
plot(M, main = "Диаграмма распределения и регресии",)

min.x <- min(x) # Находим минимальный X
max.x <- max(x) # Находим максимальный X
n <- length(x)  # Размер массива
i <- 1:n        # Массив от 1 до n

min.y <- min(y) # Находим минимальный Y
max.y <- max(y) # Находим максимальный Y

x_middle <- sum(x) / n                          # Выборочное среднее, см стр. 29.
sigma.x.sq <- sum(x ^ 2) / n - x_middle ^ 2     # Выборочная дисперсия (смещённая), первый вариант, см. формулу на 29 стр.
sigma.x <- sqrt(sigma.x.sq)                     # Стандартное отклонение сигмы, см. 30 стр.

y_middle <- sum(y) / n                          # Выборочное среднее, см стр. 29.
sigma.y.sq <- sum(y ^ 2) / n - y_middle ^ 2     # Выборочная дисперсия (смещённая), первый вариант, см. формулу на 29 стр.
sigma.y <- sqrt(sigma.y.sq)                     # Стандартное отклонение сигмы, см. 30 стр.

r = (1/n * sum(x * y)-1/n*sum(x) * 1/n * sum(y))/(sigma.x * sigma.y);

k = r*sigma.y/sigma.x;
b = y_middle - r*sigma.y*x_middle/sigma.x;

k_1 = r*sigma.y/sigma.x;
b_1 = y_middle - r*sigma.y*x_middle/sigma.x;
k_2 = r*sigma.x/sigma.y;
b_2 = x_middle - r*sigma.x*y_middle/sigma.y;

y_line = k * x+b;

f <- function(x) {
    k * x + b;
}

plot(f,
     min.x,
     max.x,
     add = TRUE,
     sub = "",
     col = '#E84545')

y.regression <- function(y){
  x_middle + r*sqrt(sigma.x.sq)*(y-y_middle)/sqrt(sigma.y.sq)
}
cat("\nСредние значения:\n",
  "\tx:", x_middle,
  "\ty:", y_middle,
 "\nДисперсии:\n",
  "\tx:", sigma.x.sq,
  "\ty:", sigma.y.sq,
 "\nСтандартные отклонения:\n",
  "\tx:", sigma.x,
  "\ty:", sigma.y,
 "\nКоэффициент корреляции:", r,
 "\ny =", k_1, "x", ifelse(b_1 >= 0, "+", "-"), abs(b_1),
 "\nx =", k_2, "y", ifelse(b_2 >= 0, "+", "-"), abs(b_2),
 "\nПрогноз при Y = 82:", y.regression(82))
