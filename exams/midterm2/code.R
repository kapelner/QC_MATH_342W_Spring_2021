
options(java.parameters = "-Xmx4000m")
pacman::p_load(ggplot2, dplyr, YARF)

#problem 3 
set.seed(1)
iris %>% sample_n(5)
iris %>% filter(Species == "setosa") %>% select(-Species) %>% sample_n(5)

#problem 4
tree_mod = YARFCART(X = iris[, 1:4], y = iris[, 5])
get_tree_num_nodes_leaves_max_depths(tree_mod)
illustrate_trees(tree_mod, max_depth = 4, open_file = TRUE)

#problem 9
ncol(model.matrix(~ .*.*., diamonds))

#problem 10
pacman::p_load_gh("coatless/ucidata")
data(adult)
adult = na.omit(adult) #kill any observations with missingness
dim(adult)
set.seed(1)
train_idx = sample(1 : nrow(adult), 10000)
adult_train = adult[train_idx, ]
lmod = glm(income ~ age + hours_per_week + capital_gain + education_num, adult_train, family = "binomial")
round(coef(lmod),4)
1 / (1 + exp(-(-8.1582 + 0.0454 * 20)))
1 / (1 + exp((-8.1582 + 0.0454 * 20)))

#problem 11
test_idx = setdiff(1 : nrow(adult), train_idx)
adult_test = adult[test_idx, ]
y_test = as.numeric(adult_test$income) - 1
yhat = as.numeric(predict(lmod, adult_test, type = "response") > 0.9)
table(y_test, yhat)

oos_conf = table(y_test, yhat)
n = sum(oos_conf)
TP = oos_conf[2, 2]
FP = oos_conf[1, 2]
FN = oos_conf[2, 1]
TN = oos_conf[1, 1]
N = sum(oos_conf[1, ])
P = sum(oos_conf[2, ])
PN = sum(oos_conf[, 1])
PP = sum(oos_conf[, 2])
(FP + FN) / n
FPR = FP / N
recall = TP / P
FPR
recall
FDR = FP / PP
FOR = FN / PN
FDR
FOR
FP / n * 100


