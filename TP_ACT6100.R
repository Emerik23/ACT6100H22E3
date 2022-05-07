install.packages("Rmarkdown")

df <- read.csv("C:\\Users\\Max-PC\\Documents\\École\\ACT 6100\\nba_data.csv")
rownames(df) <- NULL

data <- df[ (df$Year %in% c(2015,2016,2017)),]
data_skimmed <- data[,-c(1,2,3)]


summary(data_skimmed)

data_skimmed2 <- data_skimmed[data_skimmed$Salary != 0, ]

any(is.na(data_skimmed2))
data_final <- na.omit(data_skimmed2)

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
