# Лешкевич С.А
# Вариант 6 (Z10: Проверить гипотезу равенства дисперсий (по Стьюденту)) 

alpha <- 0.1        # По усл. задачи

data <- read.csv("~/MathStatic/materials/6/r2z1.csv")
x <- data$X         # Считаем все ячейки X
y <- data$Y         # Считаем все ячейки Y
x <- x[!is.na(x)]   # Очистим от пустых ячеек, перезаписав Х
y <- y[!is.na(y)]   # Очистим от пустых ячеек, перезаписав Y

x.n <- length(x)    # Расчитаем сколько элементов в X
y.n <- length(y)    # Расчитаем сколько элементов в Y

x.mu <- sum(x)/x.n  # Расчет выборочного среднего для X
y.mu <- sum(y)/y.n  # Расчет выборочного среднего для Y

x.dispersion <- sum(x^2)/x.n - x.mu^2   # Выборочная дисперсия для X
y.dispersion <- sum(y^2)/y.n - y.mu^2   # Выборочная дисперсия для Y

x.sigma <- sqrt(x.dispersion)       # Стандартное отклонение сигмы для X
y.sigma <- sqrt(y.dispersion)       # Стандартное отклонение сигмы для X

fd <- x.n + y.n - 2     # Расчет степени свободы

T <- (x.mu - y.mu) * sqrt(
 x.n * y.n * (x.n + y.n - 2) / (x.n + y.n)
) / sqrt(
 x.n * x.dispersion + y.n * y.dispersion
)

p <- 2 * (1 - pt(abs(T), fd))      # 

critical.c <- abs(qt(alpha/2, fd)) # Расчет критической константы

cat("Задача:\nПроводили замеры: по росту человек каждый человек в классе. Эксперимент проводили на учениках 7 А и Б классов. Ставится вопрос: отличается ли рост у учеников А и Б?\n")
cat(
 "\t\t X\t\t\t Y\n",
 "μ: \t", x.mu, "\t", y.mu, "\n",
 "σ²:\t", x.dispersion, "\t", y.dispersion, "\n",
 "σ: \t", x.sigma, "\t", y.sigma, "\n\n",
 "H₀: μ_X = μ_Y\n",
 "H₁: μ_X ≠ μ_Y\n",
 "ɑ:", alpha, "\n",
 "Степень свободы:", fd, "\n"
)

cat(
 " Значение статистики:", T, "\n",
 "p-value:", p, "\n",
 "Критическая константа:", critical.c, "\n",
 "Критическая область: ( -∞,", -critical.c, "] ∪ [", critical.c, ", ∞ )\n"
)

T.off.critical <- (-critical.c < T) && (T < critical.c)
if (T.off.critical) {
 cat("Находится вне критической области. Принимаем нулевую гипотезу: выборки одинаковые.")
} else {
 cat("Находится в критической области. Принимаем альтернативую гипотезу: выборки разные.")
}



