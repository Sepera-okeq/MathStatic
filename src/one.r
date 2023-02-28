# Task 1
# By sepera_okeq (Sergey Leshkevich)
# Source code license: GPLv3

# English:

# List of tasks:
# Primary statistical analysis
# 1. Sample characteristics
# 2. Sample histogramgram
# 3. Empirical distribution function

# Russian:

# Список заданий:
# Первичный статистический анализ
# 1. Выборочные характеристики
# 2. Гистограмма выборки
# 3. Эмпирическая функция распределения

cat("Лешкевич Сергей 09-132\n Задание 1\n")
x <- read.csv(file = '~/MathStatic/materials/6/r1z1.csv')$X                        # Считывание из csv файла данных X
x.varied <- sort(x)                                                                # сортируем в порядке возростания

# Предварительные данные из массива:
min.x <- min(x)                                                                    # Находим минимальный X
max.x <- max(x)                                                                    # Находим максимальный X
n <- length(x)                                                                     # Обьём массива
i <- 1:n                                                                           # Массив от 1 до n

x_middle <- sum(x) / n                                                             # Выборочное среднее, см стр. 29.
sigma.sq <- sum(x ^ 2) / n - x_middle ^ 2                                          # Выборочная дисперсия (смещённая), см. формулу на 29 странице "Теоретические_основы_выполнения_курсовой_работы_по_математической.pdf"
#sigma.sq <- sum((x[i]-x_middle)^2)/n                                              # Есть еще вариант выборочной дисперсии, но вопрос открыт, что из них будет боле точным.
sigma.sq.nooffset <- sigma.sq * (n / (n - 1))                                      # Поправленную на несмещённость оценку дисперсии, см на стр. 52
sigma <- sqrt(sigma.sq)                                                            # Стандартное отклонение сигмы
#sigma <- sqrt(sigma.sq.nooffset)                                                  # Возможно ли этот вариант отклонения? Альтернативое стандартное отклонение сигмы.
asymmetries <- sum((x - x_middle) ^ 3) / n / (sigma ^ 3)                           # Выборочный коэффициент асимметрии
excess <- sum((x - x_middle) ^ 4) / n / (sigma ^ 4) - 3                            # Выборочный коэффициент эксцесса

Q <- function(q) {                                                                 # Квантѝль, см. 15 стр
  if ((1 + q * (n - 1)) %% 1 == 0) {                                               # Если при условии будет целое число, то выполняем все спокойно, иначе else!
    x.varied[1 + q * (n - 1)]
  } else {
    (x.varied[1 + q * (n - 1)] + x.varied[(2 + q * (n - 1))]) / 2
  }
}

mode.of <- function(x) {                                                           # Мода дискретной случайной величины называется ее наиболее вероятное значение. Для непрерывной случайной величины мода – такое значение случайной величины, при которой плотность распределения имеет максимум.
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

windows()                                                                          # Создадим новое окно
par(bg = "#f7f7f7")

plot(x,                                                                             # Предварительнаая генерация графика
     type = "p",
     xlim = c(min.x, max.x),
     ylim = c(0, 1),
     tck = -0.02,
     main = "График ЭФР и гистрограмма",
     #sub = "x",
     xlab = "Диапазон всех чисел X",
     ylab = "Диапазон чисел 0.0 ... 1.0",
     fg = "#ffd700",
     font.main = 1, #font style
     font.sub  = 2,
     font.axis = 3,
     font.lab  = 4,
     
)

edf <- function(X) {                                                                # Эмпирическая функция распределения (ЭФР), см на стр. 58
  acc <- 0
  for (i in 1:n) {
    acc <- acc + (x[i] < X)
  }
  acc / n
}


edf.points <- append(append(unique(x.varied), Inf), -Inf, 0)                        # Точки из выборки без повторений с бесконечностями по краям. Диапозон от -Inf до Inf.

for (i in 1:(length(edf.points) - 1)) {                                             # Рендеринг на пространстве эмпирической функции распределения
  left <- edf.points[i]
  h <- edf(left)
  right <- edf.points[i + 1]
  segments(
    left, h,
    right, h,
    col = "#30E3CA"
  )
}

# Гистограмма (аля, ступенчатая кривая, см. стр. 55)
# Диапозон от -Inf до Inf.

k <- floor(n / 10)
delta <- (max.x - min.x) / (k - 1)
histogram.points <- append(append(seq( 
 min.x + delta / 2,
 max.x - delta / 2,
 delta
), Inf), -Inf, 0)

histogram <- function(column.number) {
  left <- histogram.points[column.number - 1]
  right <- histogram.points[column.number]
  acc <- 0
  for (i in 1:n) {
    acc <- acc + ((x[i] >= left) && (x[i] < right))
  }
  acc / n / delta
}

for (i in 2:(length(histogram.points))) {
  height <- histogram(i)
  rect(histogram.points[i - 1], 0, histogram.points[i], height, col = "#F9F9F9")
}

f <- function(x) {
  denominator <- sigma * sqrt(2 * pi)
  power <- (-1 / 2) * (((x - x_middle) / sigma) ^ 2)
  exp(power) / denominator
}

plot(f, min.x, max.x, add = TRUE, sub = "", col = '#E84545')                        # Рендерим красную линию (Эмпирическая функция распределения)

cat(                                                                                # Финальные данные выводим в консоль
 "Характеристики выборки:",
 "\n Обьём:", n,
 "\n Минимум:", min.x,
 "\n Максимум:", max.x,
 "\n Размах:", max.x - min.x,
 "\n Выборочное среднее:", x_middle,
 "\n Выборочная дисперсия (смещённая):", sigma.sq,
 "\n Выборочная дисперсия (несмещённая):", sigma.sq.nooffset,
 "\n Выборочное стандартное отклонение:", sigma,
 "\n Выборочный коэффициент ассиметрии:", asymmetries,
 "\n Выборочный коэффициент эксцесса:", excess,
 "\n Выборочная медиана:", Q(1 / 2),
 "\n Интерквартильная широта:", Q(3 / 4) - Q(1 / 4),
 "\n Мода:", mode.of(x)
)