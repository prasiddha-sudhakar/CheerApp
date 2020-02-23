############################################################################
# Class demo on Linear Regression
############################################################################

############################################################################
library('readxl')
##############################
## Loading data
##############################
## load excel file
dtibble <- read_excel("MoodProject.xlsx", col_types = "numeric")
df = data.frame(dtibble)
head(df)



### matrix method
y = df$Mood
X = df[ , c(2,3,4,5,6)]
n = nrow(df)
intercept = rep(1,nrow(df))
X = cbind(intercept,X)

X = as.matrix(X)
y = as.matrix(y)

betaOLS = solve( t(X) %*% X ) %*% t(X) %*% y

SSR = t(y- X %*%betaOLS )%*% (y- X %*%betaOLS )
sigmasqaureHat = SSR/ (n-5-1)

sigmasqaureHat = as.numeric(sigmasqaureHat)

VarCovMatrixBetaOLS = sigmasqaureHat * solve(t(X) %*% X) 

VarBetaOLS = diag(VarCovMatrixBetaOLS)
sdBetaOLS = sqrt(VarBetaOLS)

# multiple regression
linearMod2 <- lm(Mood ~ Time+Weather+Sleep+Meals+Age, data=df)
summary(linearMod2) 
summary(linearMod2)$coef

---

### graphic: mood & time 
Time <- c(1, 1, 1, 1, 3, 5, 6, 8, 9, 9, 10, 11, 11, 11, 12, 13, 14, 14, 15, 15, 16, 17, 17, 20, 20, 21, 21, 22)
Mood <- c(6, 5, 3, 1, 7, 5, 3, 7, 7, 7, 5, 1, 3, 3, 7, 0, 2, 10, 3, 7, 10, 7, 9, 9, 4, 4, 8, 6)
plot(Mood, Time)
plot(Mood, Time, pch = 16, cex = 1.3, col = "blue", main = "TIME PLOTTED AGAINST MOOD", xlab = "MOOD (0-10)", ylab = "TIME (Z)")


### graphic: mood & weather  
Weather <- c()
Mood <- c()
plot(Mood, Weather)
plot(Mood, Weather, pch = 16, cex = 1.3, col = "blue", main = "WEATHER PLOTTED AGAINST MOOD", xlab = "MOOD (0-10)", ylab = "WEATHER (Degrees Fahrenheit)")


### graphic: mood & sleep 
Sleep <- c()
Mood <- c()
plot(Mood, Sleep)
plot(Mood, Sleep, pch = 16, cex = 1.3, col = "blue", main = "SLEEP PLOTTED AGAINST MOOD", xlab = "MOOD (0-10)", ylab = "SLEEP (Hours Per Night)")


### graphic: mood & meals 
Meals <- c()
Mood <- c()
plot(Mood, Meals)
plot(Mood, Meals, pch = 16, cex = 1.3, col = "blue", main = "MEALS PLOTTED AGAINST MOOD", xlab = "MOOD (0-10)", ylab = "MEALS (Meals Per Day)")


### graphic: mood & age  
Age <- c()
Mood <- c()
plot(Mood, Age)
plot(Mood, Age, pch = 16, cex = 1.3, col = "blue", main = "AGE PLOTTED AGAINST MOOD", xlab = "MOOD (0-10)", ylab = "AGE (Years)")

  





