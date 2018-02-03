#Chapter 1
#Q1
xvallm <- function (data, ycol, predvars, p, meanabs = TRUE) {
  
  
  smp_size <- floor(p * nrow(data))
  
  ## set the seed to make the partition reproductible
  #set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  valid <- data[-train_ind, ]  
  
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid[,ycol]
  if (meanabs) return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
  
}

library(regtools)
library(freqparcoord)
data(mlb)

for (ii in 1:10){
  print(xvallm(mlb , 5 , c ( 4 , 6 ) , 2/3))
}

#Q2

data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]

pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem

model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = pe)
summary(model)

age <- 32
age2 <- 32^2
wkswrkd <- 52
ms <- 1
phd <- 0
fem <- 1
agefem <- age*fem
age2fem <- age2*fem
input <- data.frame(age,age2,wkswrkd,ms,phd,fem,agefem,age2fem)
predict(model, input, interval = "prediction", level = 0.95)

#Q3

data(bodyfat)
head(bodyfat)
lm(density ~ age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist, data = bodyfat)

#indirect methods are feasible since some variables are insignificant


#Q4.a Average population height is the average height of male and female, multiplied by the proportion of 
#the population that is male and female respectively


#Q4.b Same calculation, replacing height with proportion > 70.


#Chapter 2
#Q1

data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
# interaction terms
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
pe$msfem <- pe$ms * pe$fem
# model
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem + msfem, data = pe)
model_summ <- summary(model)
# confident intervals
beta6 <- model_summ$coefficients['fem',]
beta7 <- model_summ$coefficients['msfem',]
t_value <- qt(0.975, nrow(prgeng)-1)
beta6_h <- beta6[1] + t_value*beta6[2]
beta6_l <- beta6[1] - t_value*beta6[2]
beta7_h <- beta7[1] + t_value*beta7[2]
beta7_l <- beta7[1] - t_value*beta7[2]
paste0("Question5: 95% confidence interval for beta6: (",beta6_l,', ', beta6_h,')')
paste0("Question5: 95% confidence interval for beta6 + beta7: (",beta6_l+beta7_l,', ', beta6_h+beta7_h,')')

#Q2

day <- read.csv('day.csv')
day$temp2 <- day$temp^2
day$clearday <- as.integer(day$weathersit == 1)
bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
bike_summ <- summary(bike)
t_value <- qt(0.975, nrow(day)-1)
yr <- bike_summ$coefficients['yr',]
yr_l <- yr[1] - t_value * yr[2]
yr_h <- yr[1] + t_value * yr[2]
paste0("Question6: 95% confidence interval: (",yr_l,', ', yr_h,')')

