# Лешкевич С.А
# Вариант 6 -> MoMe 4
# Z11: Построить доверительные границы для среднего значения нормального распределения
Q <- 0.99
alpha <- 1 - Q
# Вид доверительной границы: Верхняя

x <- read.csv(file='~/MathStatic/materials/6/r3z2.csv')$X
x <- x[!is.na(x)]                           # Вычищаем от NaN и прочего барахла
n <- length(x)                              # Обьем выборки
sd <- sd(x)                                 # Cтандартное отклонение
se <- sd / sqrt(n)                          # Стандартная ошибка среднего
mean <- mean(x)                             # Выборочное среднее
degrees_of_freedom = n - 1                  # Степень свободы

# t-балл, который связан с уровнем достоверности.
t_score = qt(p=alpha, df=degrees_of_freedom,lower.tail=F)

# tα / 2,N – 1 Sx̄
me <- t_score * se                          # Предел погрешности

# x̄ ± tα / 2,N – 1 Sx̄
# x̄ ± tα / 2 : величина, необходимую для формирования области α / 2 (каждый хвост t-распределения)
# Доверительный интервал равен среднему значению + /- предел погрешности.

lower_bound <- mean - me                    # Вычисление нижней границы
#upper_bound <- mean + me                    # Вычисление верхней границы

#p <- max(pnorm(x, mean = mean, sd = sd))    # Определения квантиля для нормального распределения.
#upper_bound_alt <- mean + z * se;           # Верхняя граница доверительного интервала (legacy metod)

cat(
    "Задача 6: ", "\n",
    "Объем выборки:", n, "\n",
    "Стандартная ошибка среднего:", se, "\n",
    "Выборочное среднее:", mean, "\n",
    "Вычисленный доверительный интервал:", c("-inf", upper_bound)
)