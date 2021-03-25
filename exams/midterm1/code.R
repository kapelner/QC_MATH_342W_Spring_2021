pacman::p_load(ggplot2)


#Problems 4-7
n = 1000
set.seed(1984)
x_1 = runif(n, 0, 4)
x_2 = runif(n, 0, 1)
x_3 = x_2 > (x_1 - 2)^2
x_4 = x_2 < (x_1 - 2)^2 + 0.3

y = as.numeric(x_3 & !x_4)
num_diff = 20
y[sample(1 : n, num_diff)] = rbinom(num_diff, 1, 0.5)
y[x_3 & x_4] = NA
table(y)

df = na.omit(data.frame(x_1 = x_1, x_2 = x_2, y = as.factor(y)))
ggplot(df) + 
  geom_point(aes(x = x_1, y = x_2, col = y, shape = y), lwd = 3) #+ ylim(0, 5)

pacman::p_load(class)
knn(df[, 1:2], c(2, 0.13), df$y, k = 1)
knn(df[, 1:2], c(2, 0.13), df$y, k = 100)

#Problem 8
n = 2000
x_1 = runif(n, 0, 2)
y1 = 2 * sin(x_1 * 2 * pi) #+ 
y2 = 1 * sin(10 * 2 * pi * x_1) #+ rnorm(n, 0, 0.2)
base = ggplot(data.frame(x = x_1, y1 = y1, y2 = y2, y3 = y1+y2)) +
  aes(x = x, y = y3) +
  geom_point() + ylab("y")
base
base + geom_smooth(method = "lm")


#Problem 12
mod1 = lm(medv ~ log(tax) + poly(rm, 2) + poly(zn, 2) + poly(nox, 2), MASS::Boston)
mod2 = lm(medv ~ ., MASS::Boston)
mod3 = lm(medv ~ . * rm, MASS::Boston)
mod4 = lm(medv ~ . * rm + poly(rm, 2) + poly(zn, 2) + poly(nox, 2), MASS::Boston)
coef(mod3)



