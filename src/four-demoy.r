# Лешкевич С.А
# Вариант 2 (Вариант Z5 (Критерий согласия Колмогорова))

# Сбор данных из csv
x <- read.csv("~/MathStatic/materials/6/r2z2.csv")$X

x <- x[!is.na(x)]     # Вычищаем от NaN и прочего барахла
x_const <- x          # Копируем фактические данные.
n <- length(x)        # Общее число всех элементов X

alternative <- "greater" # Альтернативная гипотеза, что меньше
y <- "pnorm"          # Нормальное распределение
pvalue <- NULL        # p-value, иницилизация
alpha <- 0.1          # По усл. задачи
lambda <- 1.5         # По усл. задачи
mean <- 0             # По усл. задачи

# Создаем плоскость для графика
plot(0,0,
     type="p",
     xlim=c(min(x), max(x)),
     ylim=c(-1, 1),
     main="ЭФР и функция распределения (По Колмогорову)",
     ylab="",
     xlab="y"
)

# Функция получения p-value Колмогорова для 1x.
pkolmogorov1x <- function(x, n) {
  if(x <= 0) return(0)
  if(x >= 1) return(1)
  j <- seq.int(from = 0, to = floor(n * (1 - x)))
  1 - x * sum(exp(lchoose(n, j)
                  + (n - j) * log(1 - x - j / n)
                  + (j - 1) * log(x + j / n)))
}

# Получениие квантиля при малых a (аппроксимация)
kolmogorovKvRev <- function(value){
  #cat("debug:", sqrt((-1/2) * log(value/2, base = exp(1))), "\n")
  #cat((-1/2) * ln(value/2), "\n")
  sqrt((-1/2) * ln(value/2))
}

# Проверяем и задаем тип распределения (Нормальное распределение)
if(is.character(y))
  y <- get(y, mode="function")
if(is.null(exact))
  exact <- (n < 100)

x <- y(sort(x)) - (0 : (n-1)) / n

pvalue_result <- 1

while ((kolmogorovKvRev(pvalue_result)) < stat) {
  pvalue_result <- (pvalue_result - 0.0001)
}

statistic <- switch(alternative, "greater" = max(1/n - x), "less" = max(x))

# Получение значения квантиля на основе альфа значения
kolmogorov_alpha <- kolmogorovKvRev(alpha)

# Получение p-value
if(exact) {
  pvalue <- 1 - pkolmogorov1x(statistic, n)
}

# Рендерим ЭФР на плоскости
for (i in 2:length(x)) {
  if (x_sort[i-1] != x_sort[i]) {
    segments(x_sort[i-1], i/(length(x)+2), x_sort[i], i/(length(x)+2), col = "blue")
  }  
}

# Рендерим функцию распределения на плоскости
i <- 1
while (i < length(x)) {
  points(x_const[i], pnorm(x_const[i], mean = mean, sd = sqrt(lambda)), col= "red")
  i <- i + 1
}

cat("\nЗадание: Критерий согласия Колмогорова\n\nДанные:")
cat("\nКритическая область: ", "правосторонняя")
cat("\nСтатистика: ", statistic)
cat("\nКритическая константа: ", kolmogorov_alpha)
cat("\nПроверка нулевой гипотезы: ")
if (statistic > kolmogorov_alpha)
cat("отклоняем нулевую гипотезу") else
cat("принимаем нулевую гипотезу")
cat("\np-значение: ", pvalue)
cat("\np-значение: ", pvalue_result)
