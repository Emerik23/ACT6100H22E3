if("rmarkdown" %in% rownames(installed.packages()) == FALSE){
  install.packages("rmarkdown")
}
if("glmnet" %in% rownames(installed.packages()) == FALSE){
  install.packages("glmnet")
}
if("pls" %in% rownames(installed.packages()) == FALSE){
  install.packages("pls")
}

################          Les données         ###############

df <- read.csv("https://raw.githubusercontent.com/Emerik23/ACT6100H22E3/rapport_remis/donnees.csv")
rownames(df) <- NULL

data <- df[ (df$Year %in% c(2015,2016,2017)),]
data_skimmed <- data[,-c(1,2,3)]

summary(data_skimmed)

data_skimmed2 <- data_skimmed[data_skimmed$Salary != 0, ]

any(is.na(data_skimmed2))
data_final <- na.omit(data_skimmed2)
dim(data_final)

################          Statistique descriptive         ###############

colnames(data_final) <- c("Salary","Position","Age","Games played","Games started",
                            "Player efficiency rating","True shooting percentage",
                            "Percentage of field goal attemps from 3-point range",
                            "Number of free throw attempts per field goal attempt",
                            "Offensive rebound percentage", "Defensive rebound percentage",
                            "Total rebound percentage", "Assists percentage", "Steal percentage",
                            "Block percentage", "Turnover percentage", "Usage percentage",
                            "Offensive win shares", "Defensive win shares", "Win shares",
                            "Win shares per minute", "Offensive box plus minus",
                            "Defensive box plus minus", "Box plus minus", "Value over replacement player",
                            "Field goal percentage", "3-point percentage", "2-point percentage",
                            "Effective field goal percentage", "Free throw percentage", 
                            "Minutes played per game", "Field goals per game",
                            "Field goal attempts per game", "3 points per game", "3-point attempts per game",
                            "2 points per game", "2-point attempts per game", "Free throws per game",
                            "Free throw attempts per game", "Offensive rebounds per game",
                            "Defensive rebounds per game", "Total rebounds per game",
                            "Assists per game", "Steals per game", "Blocks per game", "Turnovers per game",
                            "Personal fouls per game", "Points per game")
colnames(data_final)
rownames(data_final) <- c(1:length(data_final$Salary))
rownames(data_final)

summary(data_final)
table(data_final$Position)

plot(c(1:length(data_final$Salary)),data_final$Salary, main = "Distribution des salaires annuels",
     xlab = "Numéro d'identification du joueur selon la base de données", ylab = "Salaire annuel")

set.seed(6100)
x <- model.matrix(data_final$Salary ~ ., data_final)[, -1]
y <- data_final$Salary
train <- sample(1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

###############         Modèles         ###############

###############         Régression Lasso         ###############

set.seed(6100)
x <- model.matrix(data_final$Salary ~ ., data_final)[, -1]
y <- data_final$Salary
train <- sample(1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

library(glmnet)

grid <- 10^seq(10, -2, length = 100)
modele_lasso <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

lambda <- cv.out$lambda.min
lambda

prediction_lasso <- predict(modele_lasso, s=lambda, newx=x[test,])
mean((prediction_lasso - y.test)^2)

prediction_lasso <- predict(modele_lasso, s=0, newx=x[test,])
head(prediction_lasso)

out <- glmnet(x, y, alpha=1, lambda = grid)
coefficients_lasso <- predict(out, type = "coefficients", s=lambda)[1:51,]
coefficients_lasso

EQM_lasso <- mean((prediction_lasso - y.test)^2)
EQM_lasso

###############         Régression par l'ACP         ###############

library(pls)

modele_ACP <- pcr(data_final$Salary ~ ., data = data_final, subse = train, scale = TRUE, validation = "CV")
validationplot(modele_ACP, val.type = "MSEP")

set.seed(6100)
x <- model.matrix(data_final$Salary ~ ., data_final)[, -1]
y <- data_final$Salary
train <- sample(1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

modele_ACP_fit <- pcr(y ~ x, scale = TRUE, ncomp = 20)
summary(modele_ACP_fit)

prediction_ACP <- predict(modele_ACP, x[test,], ncomp = 20)
head(prediction_ACP)

EQM_ACP <- mean((prediction_ACP - y.test)^2)
EQM_ACP

