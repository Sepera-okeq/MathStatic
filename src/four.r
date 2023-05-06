# Лешкевич С.А
# Вариант 2 (Вариант Z5 (Критерий согласия Колмогорова))

alpha <- 0.1
lambda <- 1.5

X <- read.csv("~/MathStatic/materials/6/r2z2.csv")$X

x_sort <- sort(X)
x_exp <- sum(X)/length(X)
x_dev <- sqrt((sum((X-x_exp)^2))/length(X))

plot(0,0,
  type="p",
  xlim=c(min(X), max(X)),
  ylim=c(-1, 1),
  main="ЭФР и функция распределения",
  ylab="",
  xlab="y"
)

EDF <- function(y) {
  i <- 1
  while (y > x_sort[i] && i < length(X)) {
    i <- i + 1
  }
  (i-1) / length(X)
}

FDestenyExp <- function(y) {
  if (y < 0)
    return(0)
  else {
    return(1 - exp(-lambda * y))
  }
}

stat <- 0
for(i in X) {
  if (EDF(i) - FDestenyExp(i) > stat)
    stat <- EDF(i) - FDestenyExp(i)
}
stat <- stat * sqrt(length(X))

FKolmRev <- function(value){
  #cat("xyi:", sqrt((-1/2) * log(value/2, base = exp(1))), "\n")
  is.nan(sqrt((-1/2) * log(value/2, base = exp(1))))
}

pkolmogorov1x <- function(x, n) {
    ## Probability function for the one-sided one-sample Kolmogorov
    ## statistics, based on the formula of Birnbaum & Tingey (1951).
    if(x <= 0) return(0)
    if(x >= 1) return(1)
    j <- seq.int(from = 0, to = floor(n * (1 - x)))
    1 - x * sum(exp(lchoose(n, j)
                    + (n - j) * log(1 - x - j / n)
                    + (j - 1) * log(x + j / n)))
}

kolmrev_alpha <- FKolmRev(alpha)

pvalue <- 1

cat(stat, "\n")

while ((FKolmRev(pvalue)) < stat) {
  pvalue <- (pvalue - 0.0001)
  #cat(pvalue, "\n")
}

# Вывод ЭФР
for (i in 2:length(X)) {
  if (x_sort[i-1] != x_sort[i]) {
    segments(x_sort[i-1], i/(length(X)+2), x_sort[i], i/(length(X)+2), col = "blue")
  }  
}

# Вывод Функции Распределения (Эксп)
i <- 1
while (i < length(X)) {
  points(X[i], FDestenyExp(X[i]), col= "red")
  i <- i + 1
}

cat("\nКритическая область: ", "правосторонняя")
cat("\nСтатистика: ", stat)
cat("\nКритическая константа: ", kolmrev_alpha)
cat("\nПроверка нулевой гипотезы: ")
if (stat > kolmrev_alpha)
  cat("отклоняем нулевую гипотезу") else
  cat("принимаем нулевую гипотезу")
cat("\np-значение: ", pvalue)
