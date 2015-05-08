library(caret)
library(doParallel)
set.seed(54321)

# 5/8/2015
#Revenue Prediction competition
# https://www.kaggle.com/c/restaurant-revenue-prediction/data

# Data Processing ---------------------------------------------------------

#download the files from 
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

n.train <- nrow(train)
test$revenue <- 1

##Converting into single dataframe
myData <- rbind(train, test)
myData <- myData[,-1]
rm(train, test)

#normalize
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

#Tranform Time
myData$Open.Date <- as.POSIXlt("04/30/2015", format="%m/%d/%Y") - as.POSIXlt(myData$Open.Date, format="%m/%d/%Y")
myData$Open.Date<- normalize(as.numeric(myData$Open.Date))

# summary(myData$Open.Date)


##Variable wise transformation
city<- data.frame(table(myData$City[1:137]))
# View(city[!city$Freq==0,])

#Consolidating Cities
myData$City                                      <- as.character(myData$City)
myData$City[myData$City.Group == "Other"]        <- "Other"
myData$City[myData$City == unique(myData$City)[4]] <- unique(myData$City)[2]
myData$City                                      <- as.factor(myData$City)

#Consolidate Types
myData$Type <- as.character(myData$Type)
myData$Type[myData$Type=="DT"] <- "IL"
myData$Type[myData$Type=="MB"] <- "FC"
myData$Type <- as.factor(myData$Type)
value<- read.csv("values.csv")
new1<-cbind(myData,clustering=value) 
myData$clustering<- value[,1]

#########################################################################3
#Checking which columns are factor one at a time

checkfactor<- function(y){
  x<- myData
  x[,y]<- as.factor(x[,y])
  # str(x)
  #        
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             ## repeated ten times
                             repeats = 5) #2304291
  set.seed(54321)
  model1 <- train(revenue~., 
                  data=x[1:n.train,],trControl = fitControl,method = "rf",
                  importance=TRUE)
  
  return (min(model1$results$RMSE))
}

a<- c(5,9,10,11,12,13:16,18:29,34:41)
b<- c(6:8,17,30:33)
fac_rmse1<- sapply(b,checkfactor)

summary(myData$P12)

vec<- c(5,9,10,11,12,13:16,18:29,34:41,6:8,17,30:33)
fac<- c(fac_rmse,fac_rmse1)

outp<- data.frame(cbind(fac,vec))
outp$flag<- output$fac<2304291
outp$score<- 2304291

factors<- outp$vec[output$flag] #8  9 12 16 17 18 21 23 27 29 33 40
factors<- sort(factors)

numeral<- c(1,outp$vec[!outp$flag])
numeral<- sort(c(numeral))

numeral<-C(1,5,6,7,10,11,13,14,15,19,20,22,24,25,26,28,30,31,32,34,35,36,37,38,39,41)
str(myData[,numeral])


######factors#################
factors<- c(8,9, 12 ,16, 17, 18, 21, 23, 27, 29, 33, 40,43)
myData[,c(2,3,4,factors)]<- lapply(myData[,c(2,3,4,factors)],factor)

str(myData)

#############################################
#Preprocess
numeral<-c(1,5,6,7,10,11,13,14,15,19,20,22,24,25,26,28,30,31,32,34,35,36,37,38,39,41)


#Normalizing data by transformation
preProcValues <- preProcess(myData[1:n.train,numeral], method = "BoxCox")
myData <- predict(preProcValues, myData)
# myData$revenue <- log(myData$revenue)

summary(preProcValues)

# View(myData[1:n.train,])

######Cluster Analysis###########33

d <- dist(myData[,c(1,5:7,10:11,13:15,19,20,22,24,25,28,30:32,34:39,41)], method = "euclidean")
hc <- hclust(d)
plot(hc,labels=myData[1:n.train,4])
rect.hclust(hc,k=3)
myData$deg<-as.factor(cutree(hc, k=3))

table(deg)

str(myData)



# Model prepration --------------------------------------------------------
# 
# #run model in parallel
####################Random Forest##############################3
cl <- makeCluster(detectCores())
registerDoParallel(cl)

set.seed(54321)
# Control Parameters
fitControl <- trainControl(method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 7)
set.seed(54321)
model_rf <- train(revenue~., 
               data=myData[1:n.train,],trControl = fitControl,method = "rf",
               importance=TRUE)
model_rf


x<-model1$finalModel
x
importance <- varImp(model1, scale=FALSE)
importance

#############################other model#######################
# SVM with grid search

set.seed(54321)
# Control Parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 7)
#c("P5","P10","P13","P17","P20","P21","P28","P29","P36","P2","P23","revenue")
# #8  9 12 16 17 18 21 23 27 29 33 40
#8, ##16, 17,18,9,13
set.seed(54321)
model_radial<- train(revenue~.,data=myData[1:n.train,c(numeral,42,43,16,18)],trControl = fitControl,method = "svmRadial",
              tuneGrid = expand.grid(.sigma=c(.05),.C=c(seq(.88))))
     
model_radial$results$RMSE

model_radial
rm(model_radial)

#Polynomial kernel svm
# set.seed(54321)
# model_poly<- train(revenue~.,data=myData[1:n.train,c(numeral,42)],trControl = fitControl,method = "svmPoly",
#                   tuneGrid = expand.grid(.degree=c(2),.scale= (seq(0.01,.1,.01),.C=c(seq(.1,1,.05))))
# )
# model_poly

# degree = 2, scale = 0.01 and C = 0.25.
rm(model)




###########Feature Selection###############
##Genetic Algorithm for kernel

ptm <- proc.time()
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       number = 6,
                       repeats = 5,
                       allowParallel = T,
                       genParallel = T)

rf_ga <- gafs(x = myData[1:n.train,1:41], y = myData[1:n.train,42],
              iters = 150,
              popSize = 100,
              gafsControl = ga_ctrl)
rf_ga
proc.time() - ptm

plot(rf_ga) + theme_bw()
summary()


#####################rfe#########################################
##Recursive feature selection

control <- rfeControl(functions = rfFuncs, method = "repeatedcv", verbose = FALSE,
                      returnResamp = "final", number = 10, repeats = 10, allowParallel = TRUE)

subsets <- c(5:25)
ref1<- rfe(x = myData[1:n.train,c(1,3:41)], y = myData[1:n.train,42],
               rfeControl = control, sizes = subsets)

ref1

#############################AFter GA and RFE ################################
# Results of GA and RFE
cols<- c("P5","P10","P13","P17","P20","P21","P28","P29","P36","P2","P23","revenue")

gaData<-myData[,cols] 

gaData[,fac] <- lapply(gaData[,fac], factor)
str(gaData)



# Prediction --------------------------------------------------------------

df_yhat_test <- predict(model_rf,myData[138:nrow(myData),])
df_yhat_test1 <- predict(model_radial,myData[138:nrow(myData),])

df_yhat_ensemble<- (.7*df_yhat_test + .3*df_yhat_test1)

output<- cbind("Id"= c(0:99999),"Prediction"= (df_yhat_ensemble))

write.csv(output,"df_yhat_ensemble.csv",row.names=FALSE,quote=FALSE)
