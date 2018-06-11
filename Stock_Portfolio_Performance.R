# Project - Portfolio Management -------------------------------------------------------------------------------------------------
library(ggplot2)
library(ModelMetrics)

#using 5 datasets from the UCI machine learning repository 
PeriodAll.df <- read.csv("PeriodAll.csv",header = TRUE,skip = 1)
Period1.df <- read.csv("Period1.csv",header = TRUE,skip = 1 )
Period2.df <- read.csv("Period2.csv",header = TRUE,skip = 1)
Period3.df <- read.csv("Period3.csv",header = TRUE,skip = 1)
Period4.df <- read.csv("Period4.csv",header = TRUE,skip = 1)

#resetting the column names to make it easier to code 
colnames(Period1.df) <- c('ID','x1','x2','x3','x4','x5','x6','y1','y2','y3','y4','y5','y6','y11','y22','y33','y44','y55','y66')
colnames(Period2.df) <- c('ID','x1','x2','x3','x4','x5','x6','y1','y2','y3','y4','y5','y6','y11','y22','y33','y44','y55','y66')
colnames(Period3.df) <- c('ID','x1','x2','x3','x4','x5','x6','y1','y2','y3','y4','y5','y6','y11','y22','y33','y44','y55','y66')
colnames(Period4.df) <- c('ID','x1','x2','x3','x4','x5','x6','y1','y2','y3','y4','y5','y6','y11','y22','y33','y44','y55','y66')
colnames(PeriodAll.df) <-c('ID','x1','x2','x3','x4','x5','x6','y1','y2','y3','y4','y5','y6','y11','y22','y33','y44','y55','y66')

#cleaning data---------------------------------------------------------------------------------------------------------------------
cols.num <- c(8,9,11,12,13)
Period1.df[cols.num] <- (sapply(Period1.df[cols.num],as.numeric))/100
Period2.df[cols.num] <- (sapply(Period2.df[cols.num],as.numeric))/100
Period3.df[cols.num] <- (sapply(Period3.df[cols.num],as.numeric))/100
Period4.df[cols.num] <- (sapply(Period4.df[cols.num],as.numeric))/100
PeriodAll.df[cols.num] <- (sapply(PeriodAll.df[cols.num],as.numeric))/100

summary(Period1.df[,8:13])
summary(Period2.df[,8:13])
summary(Period3.df[,8:13])
summary(Period4.df[,8:13])
summary(PeriodAll.df[,8:13])
#putting all the dataframes in a list 
df.list <- list()
df.list[[1]] <- Period1.df
df.list[[2]] <- Period2.df
df.list[[3]] <- Period3.df
df.list[[4]] <- Period4.df
df.list[[5]] <- PeriodAll.df

#sapply(c(8:10),function(x){linear.model1(Period1.df,col=x)})
#dataset is complete and has no NA values 

#preprocessing - summary

colnames(Period1.df) <- c('ID','x1','x2','x3','x4','x5','x6','Annual_Ret','Excess_Ret','Sys_Risk','Total_Risk','Abs_Risk','Rel_Win_Rate','Annual_Ret_norm','Excess Ret','Sys Risk','Total Risk','Abs_Risk_norm','Rel Win Rate')
colnames(Period2.df) <- c('ID','x1','x2','x3','x4','x5','x6','Annual_Ret','Excess_Ret','Sys_Risk','Total_Risk','Abs_Risk','Rel_Win_Rate','Annual_Ret_norm','Excess Ret','Sys Risk','Total Risk','Abs_Risk_norm','Rel Win Rate')
colnames(Period3.df) <- c('ID','x1','x2','x3','x4','x5','x6','Annual_Ret','Excess_Ret','Sys_Risk','Total_Risk','Abs_Risk','Rel_Win_Rate','Annual_Ret_norm','Excess Ret','Sys Risk','Total Risk','Abs_Risk_norm','Rel Win Rate')
colnames(Period4.df) <- c('ID','x1','x2','x3','x4','x5','x6','Annual_Ret','Excess_Ret','Sys_Risk','Total_Risk','Abs_Risk','Rel_Win_Rate','Annual_Ret_norm','Excess Ret','Sys Risk','Total Risk','Abs_Risk_norm','Rel Win Rate')
colnames(PeriodAll.df) <- c('ID','x1','x2','x3','x4','x5','x6','Annual_Ret','Excess_Ret','Sys_Risk','Total_Risk','Abs_Risk','Rel_Win_Rate','Annual_Ret_norm','Excess Ret','Sys Risk','Total Risk','Abs_Risk_norm','Rel Win Rate')
library(corrplot)

c1 = cor(Period1.df[,8:13])
c2 = cor(Period2.df[,8:13])
c3 = cor(Period3.df[,8:13])
c4 = cor(Period4.df[,8:13])
c5 = cor(PeriodAll.df[,8:13])
par(mfrow=c(2,2))

corrplot(c1)
corrplot(c2)
corrplot(c3)
corrplot(c4)

c1 = cor(Period1.df[,-1])
c2 = cor(Period2.df[,-1])
c3 = cor(Period3.df[,-1])
c4 = cor(Period4.df[,-1])
c5 = cor(PeriodAll.df[,-1])

par(mfrow=c(1,1))
corrplot(c1)
corrplot(c2)
corrplot(c3)
corrplot(c4)

c11 = cor(Period1.df[,14:19])
c22 = cor(Period2.df[,14:19])
c33 = cor(Period3.df[,14:19])
c44 = cor(Period4.df[,14:19])
c55 = cor(PeriodAll.df[,14:19])

par(mfrow=c(2,2))
corrplot(c11)
corrplot(c22)
corrplot(c33)
corrplot(c44)
corrplot(c55)

pairs(Period1.df[,8:13])
pairs(Period1.df[,2:13]) #no relations in this plot what so ever.
pairs(Period2.df[,8:13])

#
#defining multi plot function to get multiple ggplots on one page----------------------------------------------------------
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#density plots of normalized data 
for(i in 1:5){
  dp1 <- ggplot(df.list[[i]], aes(x=Annual_Ret)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)
  dp2 <-  ggplot(df.list[[i]], aes(x=Excess_Ret)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)
  dp3 <-  ggplot(df.list[[i]], aes(x=Sys_Risk)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)
  dp4 <-  ggplot(df.list[[i]], aes(x=Total_Risk)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)
  dp5 <-  ggplot(df.list[[i]], aes(x=Abs_Risk)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)
  dp6 <-  ggplot(df.list[[i]], aes(x=Rel_Win_Rate)) + geom_density(aes(group='blue', colour='blue', fill='blue'), alpha=0.3)

  file_name <- paste("density_plot",i,"jpeg",sep='.')
  jpeg(file_name)
  multiplot(dp1,dp2,dp3,dp4,dp5,dp6,cols=3)
  dev.off()
}


# PREDICTIVE MODELS-----------------------------------------------------------------------------------------------------------

# Random forest---------------------------------------------------------------------------------------------------------------
randomforest.model <- function(input_dataframe,col=14){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  rmse.randomforest.OS <- c(1:63)
  rmse.randomforest.IS <- c(1:63)
  for(i in 1:63){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    randomforest.model <- randomForest(pred~x1+x2+x3+x4+x5+x6,data=trainData)
    
    randomforest.pred.OS <- predict(randomforest.model, testData)
    randomforest.pred.IS <- predict(randomforest.model, trainData)
    
    rmse.randomforest.OS[i] <- rmse(actual=testData$pred,predicted = randomforest.pred.OS)
    rmse.randomforest.IS[i] <- rmse(actual=trainData$pred,predicted = randomforest.pred.IS)
  }
  
  return(cbind(mean(rmse.randomforest.IS),mean(rmse.randomforest.OS)))
}

rmse.randomforest.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.randomforest.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.randomforest.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")

rmse.randomforest.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.randomforest.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.randomforest.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- randomforest.model(df.list[[i]],col=j)
    rmse.randomforest.normalised.IS[i,j-13] <- x[1,1]
    rmse.randomforest.normalised.OS[i,j-13] <- x[1,2]
  }
}

#MVT Boosting algorithm CART-----------------------------------------------------------------------------------------------
library(mvtboost)

mvtboost.model <- function(input_dataframe){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  mvt.r2 <- data.frame()
  mvt.rmse <- data.frame()
  rmse <- c(1:63)
  r2 <- c(1:63) 
  
  for(i in 1:63){
    #Segement your data by fold using the which() function 
    input_dataframe[,c(8:14)] <- scale(input_dataframe[,c(8:14)] )
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]  
    
    #y.train <- trainData[,c(8:13)]
    y.train.normal <- trainData[c(14:19)]
    x.train <- trainData[,c(2:7)]
    #y.test <- testData[,c(8:13)]
    y.test.normal <- testData[c(14:19)]
    x.test <- testData[,c(2:7)]
    
    #mvtb.model1 <- mvtb(Y=y.train,X=x.train,          
    #           n.trees=1000,          # number of trees
    #            shrinkage=.01,         # shrinkage or learning rate
    #            interaction.depth=3)   # tree or interaction depth
    
    mvtb.model2 <- mvtb(Y=y.train.normal,X=x.train,          
                        n.trees=10000,          # number of trees
                        shrinkage=.01,         # shrinkage or learning rate
                        interaction.depth=5)   # tree or interaction depth
    
    
    
    #y.predicted <- predict(mvtb.model1,newdata=x.test)
    y.predicted.normal <- predict(mvtb.model2,newdata =x.test)
    
    #
    #predictions with normalised data clearly outpeforms
    #we therefore resort to the caluclating rmse and r2 values of rmse and r2 values only 
    #why does it work better with normalised data ?? 
    #
    rmse <- abs((y.test.normal) - as.matrix(t(y.predicted.normal)))
    mvt.rmse <- rbind(mvt.rmse,rmse) 
    
  }
  return(colMeans(mvt.rmse))
  
}
rmse.mvtboost.table <- data.frame()
for(i in 1:5){
  print('Period')
  print(i)
  rmse.mvtboost <- (mvtboost.model(df.list[[i]]))
  rmse.mvtboost.table <- rbind(rmse.mvtboost.table,rmse.mvtboost)
}

#MVTboost provides good predictions for y2, y3, y4 and y5 
#we use parital dependence plots to predict y2, y3, y4 and y5 
mvtb.model2$best.trees


par(mfrow=c(2,3))              # model implied effects for predictor 2 for cty and hwy
plot(mvtb.model2, response.no=2,predictor.no=1)
plot(mvtb.model2, response.no=2,predictor.no=2)
plot(mvtb.model2, response.no=2,predictor.no=3)
plot(mvtb.model2, response.no=2,predictor.no=4)
plot(mvtb.model2, response.no=2,predictor.no=5)
plot(mvtb.model2, response.no=2,predictor.no=6)

par(mfrow=c(2,3)) 
for( i in 1:6){
  plot(mvtb.model2, response.no=3,predictor.no=i)
}

par(mfrow=c(2,3)) 
for( i in 1:6){
  plot(mvtb.model2, response.no=4,predictor.no=i)
}

par(mfrow=c(2,3)) 
for( i in 1:6){
  plot(mvtb.model2, response.no=5,predictor.no=i)
}

#GAMs ---------------------------------------------------------------------------------------------------------------------
gam.model <- function(input_dataframe, col=14){
  set.seed(22)
  input_dataframe <- input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  
  rmse.gam.OS <- c(1:63)
  rmse.gam.IS <- c(1:63)
  
  for(i in 1:63){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    #now use step.gam 
    gam.obj <- gam(pred~x1+x2+x3+x4+x5+x6, data = trainData)
    gam.model <- step.Gam(gam.obj, scope = list("x1"=~ 1+ x1 + s(x1, df=2)+ s(x1, df=3)+ s(x1, df=4)+lo(x1),
                                                    "x2"=~ 1+ x2 + s(x2, df=2)+ s(x2, df=3)+ s(x2, df=4)+lo(x2),
                                                    "x3"=~ 1+ x3 + s(x3, df=2)+ s(x3, df=3)+ s(x3, df=4)+lo(x3),
                                                    "x4"=~ 1+ x4 + s(x4, df=2)+ s(x4, df=3)+ s(x4, df=4)+lo(x4),
                                                    "x5"=~ 1+ x5 + s(x5, df=2)+ s(x5, df=3)+ s(x5, df=4)+lo(x5),
                                                    "x6"=~ 1+ x6 + s(x6, df=2)+ s(x6, df=3)+ s(x6, df=4)+lo(x6)
                                                    ),direction="both",trace=FALSE)
    
    gam.pred.OS <- predict(gam.model, newdata=testData)
    gam.pred.IS <- predict(gam.model, newdata=trainData)
    
    rmse.gam.OS[i] <- rmse(actual=testData$pred,predicted = gam.pred.OS)
    rmse.gam.IS[i] <- rmse(actual=trainData$pred,predicted = gam.pred.IS)
  }
  
  return(cbind(mean(rmse.gam.IS),mean(rmse.gam.OS)))
}

rmse.gam.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.gam.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.gam.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")


rmse.gam.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.gam.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.gam.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- gam.model(df.list[[i]],col=j)
    rmse.gam.normalised.IS[i,j-13] <- x[1,1]
    rmse.gam.normalised.OS[i,j-13] <- x[1,2]
    
  }
}

#MARS_Pruned---------------------------------------------------------------------------------------------------------------------- 
library(earth)

mars.pruned.model <- function(input_dataframe,col=14){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  
  rmse.mars.pruned.OS <- c(1:63)
  rmse.mars.pruned.IS <- c(1:63)
 
   for(i in 1:63){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    mars.pruned.model <- earth(pred~x1+x2+x3+x4+x5+x6,data=trainData)
    
    mars.pruned.pred.OS <- predict(mars.pruned.model, testData)
    mars.pruned.pred.IS <- predict(mars.pruned.model, trainData)
    
    rmse.mars.pruned.OS[i] <- rmse(actual=testData$pred,predicted = mars.pruned.pred.OS)
    rmse.mars.pruned.IS[i] <- rmse(actual=trainData$pred,predicted = mars.pruned.pred.IS)
   }
  return(cbind(mean(rmse.mars.pruned.IS),mean(rmse.mars.pruned.OS)))
}
rmse.mars.pruned.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.mars.pruned.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.mars.pruned.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")


rmse.mars.pruned.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.mars.pruned.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.mars.pruned.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- mars.pruned.model(df.list[[i]],col=j)
    rmse.mars.pruned.normalised.IS[i,j-13] <- x[1,1]
    rmse.mars.pruned.normalised.OS[i,j-13] <- x[1,2]
    
  }
}

#MARS_Unpruned---------------------------------------------------------------------------------------------------------------------
library(earth)

mars.unpruned.model <- function(input_dataframe,col=14){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  
  rmse.mars.unpruned.OS <- c(1:63)
  rmse.mars.unpruned.IS <- c(1:63)
  
  for(i in 1:63){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    mars.unpruned.model <- earth(pred~x1+x2+x3+x4+x5+x6,data=trainData, pmethod=c("none"))
    
    mars.unpruned.pred.OS <- predict(mars.unpruned.model, testData)
    mars.unpruned.pred.IS <- predict(mars.unpruned.model, trainData)
    
    rmse.mars.unpruned.OS[i] <- rmse(actual=testData$pred,predicted = mars.unpruned.pred.OS)
    rmse.mars.unpruned.IS[i] <- rmse(actual=trainData$pred,predicted = mars.unpruned.pred.IS)
  }
  return(cbind(mean(rmse.mars.unpruned.IS),mean(rmse.mars.unpruned.OS)))
}
rmse.mars.unpruned.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.mars.unpruned.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.mars.unpruned.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")


rmse.mars.unpruned.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.mars.unpruned.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.mars.unpruned.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- mars.unpruned.model(df.list[[i]],col=j)
    rmse.mars.unpruned.normalised.IS[i,j-13] <- x[1,1]
    rmse.mars.unpruned.normalised.OS[i,j-13] <- x[1,2]
    
  }
}


#9)BART----------------------------------------------------------------------------------------------------------------------
library(bartMachine)


bart.model <- function(input_dataframe,col = 8){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  
  rmse.bart.OS <- c(1:63)
  rmse.bart.IS <- c(1:63)
  

  for(i in 1:63){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    df.train.covariates <- trainData[,c(2:7)]
    df.train.response <- trainData[,col]
    
    bart.model <- bartMachine(X=df.train.covariates,y=df.train.response)
    rmse.bart.OS[i] <- rmse(actual = testData[,col], predicted = predict(bart.model,testData[,c(2:7)]))
    rmse.bart.IS[i] <- rmse(actual = trainData[,col], predicted = predict(bart.model, trainData[,c(2:7)]))
    
  }
  
  return(cbind(mean(rmse.bart.IS),mean(rmse.bart.OS)))
}

rmse.bart.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.bart.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.bart.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")


rmse.bart.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.bart.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.bart.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- bart.model(df.list[[i]],col=j)
    rmse.bart.normalised.IS[i,j-13] <- x[1,1]
    rmse.bart.normalised.OS[i,j-13] <- x[1,2]
    #r2.gam[i,j-7] <- x[[2]][1]
    
  }
}


#10)SVM----------------------------------------------------------------------------------------------------------------------
library(e1071)
svm.model <- function(input_dataframe, col = 14){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  rmse.svm.OS <- c(1:63)
  rmse.svm.IS <- c(1:63)
  for(i in 1:63){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    svm.model <- svm(pred ~ x1 + x2 + x3 + x4 + x5 + x6 , trainData)
    svm.pred.OS <- predict(svm.model, testData)
    svm.pred.IS <- predict(svm.model, trainData)
    
    rmse.svm.OS[i] <- rmse(actual=testData$pred,predicted = svm.pred.OS)
    rmse.svm.IS[i] <- rmse(actual=trainData$pred,predicted = svm.pred.IS)
  }
  return(cbind(mean(rmse.svm.IS),mean(rmse.svm.OS)))
}

rmse.svm.normalised.IS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.svm.normalised.IS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.svm.normalised.IS) <- c("y11","y22","y33","y44","y55","y66")


rmse.svm.normalised.OS <- as.data.frame(matrix(nrow = 5, ncol = 6))
rownames(rmse.svm.normalised.OS) <- c("Period 1","Period 2","Period 3","Period 4","Period All")
colnames(rmse.svm.normalised.OS) <- c("y11","y22","y33","y44","y55","y66")

#tabulation of insample and outsample errors into tables 
for(i in 1:5){
  for(j in 14:19){
    x <- svm.model(df.list[[i]],col=j)
    rmse.svm.normalised.IS[i,j-13] <- x[1,1]
    rmse.svm.normalised.OS[i,j-13] <- x[1,2]
    
  }
}

#Nueral Nets --------------------------------------------------------------------------------------------------------------------
library(neuralnet)

nueralnet.model <- function(input_dataframe,col=14){
  set.seed(22)
  input_dataframe<-input_dataframe[sample(nrow(input_dataframe)),] #shuffle 
  names(input_dataframe)[col] <- "pred"
  folds <- cut(seq(1,nrow(input_dataframe)),breaks=63,labels=FALSE)
  
  for(i in 1:63){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- input_dataframe[testIndexes, ]
    trainData <- input_dataframe[-testIndexes, ]
    
    nn.model <- neuralnet()
  }
}

#BART was one of the best models-------------------------------------------------------------------------------------------
#hence we plot the Partial dependence plots for the BART model
for(i in 1:6){
  pd_plot(bart_machine=bart.model, i)
}
