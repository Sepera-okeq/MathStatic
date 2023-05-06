data <- read.csv("~/MathStatic/materials/6/r2z1.csv")
x <- data$X         # Считаем все ячейки X
y <- data$Y         # Считаем все ячейки Y
x <- x[!is.na(x)]   # Очистим от пустых ячеек, перезаписав Х
y <- y[!is.na(y)]   # Очистим от пустых ячеек, перезаписав Y

var.test(x, y,  alternative ="two.sided")