# Import libraries
library(readxl)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(psych)
library(ltm)
library(caret)

# Read Data
data <- read_excel("./data/data.xls",
                   sheet="5484")

# Rename columns
data2 <- data %>% rename(occupation=b1, workplace_scale=b2, sex=q102, age=q103, 
                        marital=q104, education=q105, children=q106, salary=q107, 
                        numMigrant=q108a1, numResidency=q108a2, numCities=q108b, 
                        numWorkingHours=q108c1, numWorkingDays=q108c2, 
                        residency=q109, hasHeadaches=q201, hasNervousness=q202,
                        hasUnwanted=q203, hasFaintness=q204, hasLossInterest=q205, 
                        hasCritical=q206, hasParanoid=q207, hasBlameOthers=q208, 
                        hasForget=q209, hasCarelessness=q210, hasAnnoyed=q211, 
                        hasChestpain=q212,hasUneasiness=q213, hasLethargic=q214, 
                        hasThoughtsEnd=q215, hasHallucinate=q216, hasTrembling=q217, 
                        hasTrustIssue=q218, hasPoorAppetite=q219, hasCry=q220, 
                        hasShy=q221, hasTrapped=q222, hasScared=q223, 
                        hasTemper=q224, hasAfraidHouse=q225, hasBlameSelf=q226, 
                        hasLowerPain=q227, hasBlocked=q228, hasLonely=q229, 
                        hasBlue=q230,hasWorry=q231, hasZeroInterest=q232, 
                        hasFearful=q233, hasHurt=q234, hasPrivate=q235, 
                        hasUnsympathetic=q236, hasUnfriendly=q237, hasSlow=q238, 
                        hasHeartRacing=q239, hasNausea=q240, hasInferior=q241, 
                        hasSoreness=q242, hasWatched=q243, hasTroubleSleep=q244, 
                        hasCheck=q245, hasDecision=q246, hasAfraidTransport=q247, 
                        hasBreath=q248, hasSpell=q249, hasFright=q250, 
                        hasBlank=q251, hasNumbness=q252, hasLump=q253, 
                        hasHopeless=q254, hasConcentrate=q255, hasWeak=q256, 
                        hasTense=q257, hasHeavy=q258, hasThoughtsDie=q259, 
                        hasOvereating=q260, hasUneasyWatch=q261, 
                        hasOtherThoughts=q262, hasBeatUrge=q263, hasAwakening=q264,
                        hasRepeat=q265, hasDisturbedSleep=q266, hasViolentUrge=q267,
                        hasBeliefs=q268, hasSelfConscious=q269, hasCrowd=q270, 
                        hasEffort=q271, hasTerror=q272, hasEatPublic=q273, hasArgue=q274, 
                        hasNervousAlone=q275, hasLackCredit=q276, hasLonelyPeople=q277, 
                        hasRestless=q278, hasWorthless=q279, hasFamiliarStrange=q280, 
                        hasShout=q281, hasAfraidFaint=q282, hasAdvantage=q283, 
                        hasSexBothered=q284, hasPunishSin=q285, hasPushed=q286, 
                        hasWrongBody=q287, hasIsolate=q288, hasGuilt=q289, 
                        hasWrongMind=q290, smoke100=q410, smokePast30=q411, 
                        smokeNum=q412, drinks=q431, numDrinks=q432, 
                        freq6Drinks=q433, numSleep=q444, sleep=q446, 
                        breakfast=q450, fruitveg=q451, meal=q452) %>%
  
  mutate(smoke=if_else((smokePast30==2), 
                3, 
                if_else(((smokePast30==1) & (smoke100==2)), 
                        2, 
                        ifelse((smoke100==1), 
                                1, 
                                NA)
                        )
                )) %>%
  mutate(smokeNum2=if_else(smoke==1, 0, ifelse(is.na(smokeNum), NA, smokeNum))) %>%
  dplyr::select(-c(smoke100, smokePast30, smokeNum)) %>%
  rowwise() %>%
  mutate(score=sum(across(c(20:109)))) %>% 
  ungroup()

# (smokePast30==2 & smoke100==2) => 3
# (smokePast30==1 & smoke100==2) => 2
# (is.na(smokePast30) & smoke100==1) => 1
# (is.na(smokePast30) & smoke100==1) => NA


# score: 
# smoke: 1-Non, 2-Previous, 3-Current

# Smoking:
# If smokePast30 == 2 -> Current
# Else -> Previous
# Else -> Non

# Drop NA values
data3 <-na.omit(data2)

# Get response column
data3 <- data3 %>% 
  mutate(healthStatus=ifelse(score > 160, 1, 0)) %>%
  dplyr::select(-c(score))

# Flip order of categorical values into Low-> High
data3 <- data3 %>%
  mutate(workplace_scale = if_else(workplace_scale==1, 2, if_else(workplace_scale==2, 1, 0)),
         occupation = occupation-1,
         sex = sex-1,
         marital= marital-1,
         education = education-1,
         children = children-1,
         salary = salary-1,
         residency=residency-1,
         drinks = drinks-1,
         numDrinks = numDrinks-1,
         freq6Drinks = freq6Drinks-1,
         sleep = if_else(sleep==4,0,if_else(sleep==3, 1, if_else(sleep==2,2,3))),
         breakfast = if_else(breakfast==4,0,if_else(breakfast==3, 1, if_else(breakfast==2,2,3))),
         fruitveg = if_else(fruitveg==4,0,if_else(fruitveg==3, 1, if_else(fruitveg==2,2,3))),
         meal = if_else(meal==4,0,if_else(meal==3, 1, if_else(meal==2,2,3)))
         ) %>%
  rename(numSmoke=smokeNum2, smokeType=smoke)
data3[20:109] <- data3[20:109]-1


# Convert Data Types
data3 <- data3 %>%
  mutate_at(c(2,3,8:13,19,20:109,110:112,114:118,120), as.factor) %>%
  dplyr::select(-id)

str(data3, list.len=ncol(data3))








### Correlation

# Check for correlation between continuous variables using Pearson Correlation
cols <- c("height", "weight", "bphigh", "bplow", "numMigrant", 
          "numResidency", "numCities", "numWorkingHours", 
          "numWorkingDays", "numSleep", "numSmoke")
data_con <- data3[cols]

corrplot(cor(data_con), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Correlation detected:
# height and weight => 0.6474707
# bphigh and bplow => 0.6522560

# Steps:
# BMI
# Healthy Blood pressure

data4 <- data3 %>%
  mutate(highBP = as.factor(if_else((bphigh < 140 & bplow < 90), 0, 1)),
         bmi = weight/(height/100)^2) %>%
  dplyr::select(-c(height, weight, bplow, bphigh))

# bphigh < 140 & bplow < 90 => normal
# Else => high
# https://www.healthhub.sg/a-z/diseases-and-conditions/735/Understanding-Blood-Pressure-Readings

str(data4, list.len=ncol(data4))

# Check correlation Attempt #2
data_con <- data4[c("bmi", "numMigrant", "numResidency", "numCities", "numWorkingHours", 
                    "numWorkingDays", "numSleep", "numSmoke")]

corrplot(cor(data_con), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Check for correlation between categorical variables (non-binary)
all_cols <- colnames(data4)
cols1 <- setdiff(all_cols, c("bmi", "numMigrant", "numResidency", "numCities", 
                            "numWorkingHours", "numWorkingDays", "numSleep", "numSmoke", 
                            "sex", "smokeType", "highBP"))

data_cat <- data4[cols1]
pairs <- t(combn(cols1,2))
num_rows <- length(pairs)/2
cramer_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  cramer_vals[i] <- cramerV(data_cat[[row[1]]], data_cat[[row[2]]])  
}

cramer_vals[cramer_vals >= 0.5]
for (i in cramer_vals[cramer_vals >= 0.5]){
  print(pairs[which(cramer_vals == i),])
}

## Most of the Cramer's V values are < 0.5 and are close to 0. 
## Exception: 
# 1. hasLonely and hasBlue (Exact)


# Meaning are very close
# Feeling lonely
# Feeling Blue
# Remove: Feeling Blue

data5 <- dplyr::select(data4, -c(hasBlue, hasUnwanted ,hasAnnoyed ,hasLethargic ,hasTrustIssue ,hasTrapped ,hasScared ,hasTemper ,hasBlameSelf ,hasBlocked ,hasLonely ,hasBlue ,hasWorry ,hasZeroInterest ,hasFearful ,hasHurt ,hasPrivate ,hasUnsympathetic ,hasUnfriendly ,hasHeartRacing ,hasInferior ,hasWatched ,hasDecision ,hasFright ,hasBlank ,hasNumbness ,hasHopeless ,hasConcentrate ,hasWeak ,hasTense ,hasHeavy ,hasThoughtsDie ,hasUneasyWatch ,hasOtherThoughts ,hasBeatUrge ,hasViolentUrge ,hasBeliefs ,hasSelfConscious ,hasCrowd ,hasEffort ,hasTerror ,hasArgue ,hasLackCredit ,hasLonelyPeople ,hasRestless ,hasWorthless ,hasFamiliarStrange ,hasShout ,hasAdvantage ,hasSexBothered ,hasPunishSin ,hasWrongBody ,hasIsolate ,hasGuilt))

str(data5, list.len=ncol(data5))

# Check for correlation between categorical variables (binary)
cols2 <- c("sex", "smokeType", "healthStatus", "highBP")

data_cat2 <- data5[cols2]
pairs <- t(combn(cols2,2))
num_rows <- length(pairs)/2
phi_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  temptable <- table(data_cat2[[row[1]]], data_cat2[[row[2]]])
  phi_vals[i] <- phi(temptable)  
}
phi_vals[phi_vals >= 0.5]
phi_vals[phi_vals <= -0.5]
pairs[which(phi_vals==0.54),]
## Most of the Phi Coefficients values are < 0.5 or > -0.5 and are close to 0. 
## Exception:
# 1. sex and smokeType (0.54)
# Both are 2 different variables and there is only moderate correlation. Decided to keep both variables

# Check for correlation between continuous and binary categorical variables
con <- c("numMigrant", "numResidency", "numCities", "numWorkingHours", 
         "numWorkingDays", "numSleep", "numSmoke", "bmi")

pairs <- t(combn(c(con, cols2),2))
num_rows <- length(pairs)/2
pb_vals <- rep(0, num_rows)
for (i in 1:num_rows){
  row <- pairs[i,]
  if (row[1] %in% cols2){ # 1st var categorical
    if (row[2] %in% cols2){ # Both categorical
      pb_vals[i] <- NA
    }
    else { # 1st categorical, 2nd continuous
      pb_vals[i] <- biserial.cor(as.numeric(data5[[row[2]]]), as.numeric(data5[[row[1]]]))
    }
  }
  else { # 1st var continuous
    if (row[2] %in% cols2){ # 1st continuous, 2nd categorical
      pb_vals[i] <- biserial.cor(as.numeric(data5[[row[1]]]), as.numeric(data5[[row[2]]]))
    }
    else{ # Both continuous
      pb_vals[i] <- NA
    }
  }
}

pb_vals[pb_vals >= 0.5]
pb_vals[pb_vals <= -0.5]

pairs[54,]

data6 <- dplyr::select(data5, -c("numSmoke"))
str(data6, list.len=ncol(data6))


#data6 <- dplyr::select(data5, -colnames(data5)[15:103])

# EDA
data6 %>%
  ggplot(aes(occupation, fill = healthStatus)) +
  geom_bar(position = "fill") +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))

data6 %>%
  ggplot(aes(sex, fill = healthStatus)) +
  geom_bar(position = "fill") +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))

data6 %>%
  ggplot(aes(marital, fill = healthStatus)) +
  geom_bar(position = "fill") +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))

data6 %>%
  ggplot(aes(age, fill = healthStatus)) +
  geom_bar(position = "fill") +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))

data6 %>%
  filter(as.numeric(education) <= 4) %>%
  mutate(education=as.factor(education)) %>%
  ggplot(aes(education, fill = healthStatus)) +
  geom_bar(position = "fill") +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))


data6 %>%
  ggplot(aes(numDrinks, fill = healthStatus)) +
  geom_bar(position = "fill", axis.title.x=element_text(size=20)) +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))


data6 %>%
  ggplot(aes(sleep, fill = healthStatus)) +
  geom_bar(position = "fill", axis.title.x=element_text(size=20)) +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))


data6 %>%
  ggplot(aes(fruitveg, fill = healthStatus)) +
  geom_bar(position = "fill", axis.title.x=element_text(size=20)) +
  labs(y="Proportion of mental health") + 
  theme(legend.position='bottom', axis.title.x=element_text(size=20)) +
  scale_fill_discrete(labels=c("Good", "Poor"))



# Modelling
attach(data6)
set.seed(4248)  
# Train Test Split 70-30
idx <- createDataPartition(y = healthStatus, p=0.7, list=FALSE)
train <- data6[idx,]
test <- data6[as.numeric(rownames(data6[-idx,])),]

library(smotefamily)
set.seed(4248)
train <- SMOTE(as_tibble(sapply(subset(train, select = -c(healthStatus)), as.numeric)), 
               target = train$healthStatus)$data 

library(glmnet)
library(ModelMetrics)
set.seed(4248)
cv.out<-cv.glmnet(data.matrix(subset(train, select = -c(class))),
                  train$class,
                  alpha=1, 
                  family="binomial")
plot(cv.out)


bestlam<-cv.out$lambda.min

# set.seed(4248)
# trial<-glmnet(data.matrix(subset(train, select = -c(class))),
#              train$class,
#              alpha=1, 
#              lambda=bestlam,
#              family="binomial")
# trial.pred <- predict(trial,type="response",newx=data.matrix(test[,-which(names(test) %in% c("healthStatus"))]))
# table(predict = ifelse(trial.pred > 0.5, 1, 0), truth = test$healthStatus)

predict(cv.out,type="coefficients",s=bestlam)

reglog_model.pred<-predict(cv.out, 
                           s=bestlam, 
                           type="response",
                           newx=data.matrix(test[,-which(names(test) %in% c("healthStatus"))]))

reglog_model.pred <- ifelse(reglog_model.pred > 0.5, 1, 0)
table(predict = reglog_model.pred, truth = test$healthStatus)
conf <- table(predict = reglog_model.pred, truth = test$healthStatus)
(conf[1,1]+conf[2,2]) / length(reglog_model.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(test$healthStatus, reglog_model.pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1

# Model 2: XGBoost
require(xgboost)

xtrain <- subset(train, select=-c(class))
ytrain <- as.factor(train$class)
xtest <- subset(test, select=-c(healthStatus))
ytest <- test$healthStatus


dtrain <- xgb.DMatrix(data = data.matrix(xtrain), label = as.matrix(ytrain)) 
dvalid <- xgb.DMatrix(data = data.matrix(xtest), label = as.matrix(ytest)) 

# Random Search
start.time <- Sys.time()
# Create empty lists
lowest_error_list = list()
parameters_list = list()
# Create 10,000 rows with random hyperparameters
set.seed(20)
for (iter in 1:10000){
  param <- list(booster = "gbtree",
                objective = "binary:logistic",
                max_depth = sample(5:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1),
                scale_pos_weight = sample(c(1,5,10,25,50,75,100,200),1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}
# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)
# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)) {
  set.seed(4248)
  mdcv <- xgb.train(data=dtrain,
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    scale_pos_weight = parameters_df$scale_pos_weight[row],
                    nrounds= 10,
                    eval_metric = "logloss",
                    print_every_n = 100,
                    watchlist = list(train= dtrain, val= dvalid)
  )
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_logloss))
  lowest_error_list[[row]] <- lowest_error
}
# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)
# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)
# Quickly display best validation loss
max(randomsearch[,1])
# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
write_csv(randomsearch, "hello3.csv")

# Best Param (lowest logloss)
# 0.93751	gbtree	binary:logistic	9	0.293135068	0.727277689	0.633306692	0	1

set.seed(4248)
gridsearch_model <- xgb.train(data = dtrain, 
                              max.depth = 9, 
                              eta = 0.293135068, 
                              nthread = 2, 
                              nrounds = 10,
                              subsample = 0.727277689,
                              colsample_bytree = 0.633306692,
                              min_child_weight = 0,
                              booster = 'gbtree',
                              objective = "binary:logistic",
                              scale_pos_weight = 1)

xgb.pred <- predict(gridsearch_model, data.matrix(xtest))
xgb.pred <- as.numeric(xgb.pred > 0.5)
table(predict = xgb.pred, truth = ytest)
conf <- table(predict = xgb.pred, truth = test$healthStatus)
(conf[1,1]+conf[2,2]) / length(xgb.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(test$healthStatus, xgb.pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1

# 0.932864	gbtree	binary:logistic	8	0.289806869	0.86268279	0.668115448	0	1
set.seed(4248)
gridsearch_model <- xgb.train(data = dtrain, 
                              max.depth = 8, 
                              eta = 0.289806869, 
                              nthread = 2, 
                              nrounds = 10,
                              subsample = 0.86268279,
                              colsample_bytree = 0.668115448,
                              min_child_weight = 0,
                              booster = 'gbtree',
                              objective = "binary:logistic",
                              scale_pos_weight = 1)

xgb.pred <- predict(gridsearch_model, data.matrix(xtest))
xgb.pred <- as.numeric(xgb.pred > 0.5)
table(predict = xgb.pred, truth = ytest)
conf <- table(predict = xgb.pred, truth = test$healthStatus)
(conf[1,1]+conf[2,2]) / length(xgb.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(test$healthStatus, xgb.pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1

# Stepwise
library(MASS)
model.glm <- glm(class ~., family=binomial, data=train%>%mutate(class=as.factor(class)))
model.glm.step <- stepAIC(model.glm, trace=TRUE)
model.glm.step.pred<-predict(model.glm.step,
                             as_tibble(sapply(test[,-which(names(test) %in% c("healthStatus"))], as.numeric)),
                             type="response")
                             #newx=data.matrix(test[,-which(names(test) %in% c("healthStatus"))]))

model.glm.step.pred <- ifelse(model.glm.step.pred > 0.5, 1, 0)
table(predict = model.glm.step.pred, truth = test$healthStatus)


new_cols <- c(names(coef(model.glm.step))[2:48], "class")
train2 <- train[new_cols]

new_cols_2 <- c(names(coef(model.glm.step))[2:48], "healthStatus")
test2 <- test[new_cols_2]


xtrain <- subset(train2, select=-c(class))
ytrain <- as.factor(train2$class)
xtest <- subset(test2, select=-c(healthStatus))
ytest <- test$healthStatus


dtrain <- xgb.DMatrix(data = data.matrix(xtrain), label = as.matrix(ytrain)) 
dvalid <- xgb.DMatrix(data = data.matrix(xtest), label = as.matrix(ytest)) 
set.seed(4248)
gridsearch_model <- xgb.train(data = dtrain, 
                              max.depth = 8, 
                              eta = 0.289806869, 
                              nthread = 2, 
                              nrounds = 10,
                              subsample = 0.86268279,
                              colsample_bytree = 0.668115448,
                              min_child_weight = 0,
                              booster = 'gbtree',
                              objective = "binary:logistic",
                              scale_pos_weight = 1)

xgb.pred <- predict(gridsearch_model, data.matrix(xtest))
xgb.pred <- as.numeric(xgb.pred > 0.5)
table(predict = xgb.pred, truth = ytest)
conf <- table(predict = xgb.pred, truth = test$healthStatus)
(conf[1,1]+conf[2,2]) / length(xgb.pred)
recall <- conf[2,2] / (conf[2,2] + conf[1,2])
recall
precision <- conf[2,2] / (conf[2,2] + conf[2,1])
precision
auc(test$healthStatus, xgb.pred)
f1 <- 2*(precision*recall)/ (precision + recall)
f1
