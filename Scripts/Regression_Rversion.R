library(caret)
library(xlsx)
library(DescTools)
library(NLP)

TrainModel <- function(trainX, trainY, testX, column, SL = 0.05){
    pValues = c()
    trainX["const"] = 1
    testX["const"] = 1 # Breifer than python
    column[length(column) + 1] = "const"
    for(i in 1:ncol(trainX)){
        regressor = lm(trainY ~ trainX[,1] + trainX[,2] + trainX[,3] + trainX[,4] + trainX[,5]) # hardcode 5 column here for convenience.
        pValues = ExtractPValue(regressor)

        if(i == 1){
            "============="
            "Start:"
            summary(regressor)
        }

        MaxP = max(pValues)
        if( MaxP > SL){
            MaxIndex = match(MaxP, pValues)
            trainX[,MaxIndex] = NULL
            testX[,MaxIndex] = NULL
            pValue = c(pValue[1:MaxIndex-1], pValue[MaxIndex+1:length(pValue)])
        }
        else break;
    }
    print("============")
    print("Final:")
    print(summary(regressor))

    return (list("regressor" = regressor, "trainX" = trainX, "testX" = testX, "column" = column))
}

ExtractPValue <- function(model){
    # https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

EvalueModel <- function(model, scalerOfy, testX, testy, column, i = ''){
    # R's prediction isn't like Python's. It needs the same size of training data and predicting data... :(
    close_pred = predict(model,testX)
    # temp solution: randomly select 256(yes, I'm lazy. I hard code.) data.
    close_predd = c()
    smp = sample(1:1039, 256)
    for( i in 1:256){
        close_predd[i] = as.numeric(pre[[1]][String(smp[i])])
    }
    
    close_predd = InvertStandardize(close_predd, scalerOfy$Mean, scalerOfy$SD)
    close_test = InvertStandardize(testy ,scalerOfy$Mean, scalerOfy$SD)
    #Plot Pred vs Test
    par(mfrow = c(1,1), mai =  c(0.8,0.8, 0.8, 0.8), cex = 0.8)
    plot(x = close_test, y = close_predd,
        main = "Test vs Prediction \n Using Open TLC, Open NDX, Gold Value(USD), Oil Value(USD)",
        xlab = "Close BTC Prediction",
        ylab = "Close BTC Test",
        pch = 16
    )

    #Plot residual graph
    #residual = close_test-close_pred
    #plot(close_test, residual, xlab = "Close BTC Test", ylab = "Resodual", main = "REsidual Plot")
}

Standardize <- function(X, mean, sd){
    value = (X-mean)/sd
    eval.parent(substitute(X <- value))
}

InvertStandardize <- function(X, mean, sd){
    value = X*sd + mean
    return(value)
}

# In[0] Read data:
## need to fix the path to relative path....
## R is quite annoying to do so. Alt least I haven't found the answer yet.
files = c("D:/BTC Regression Model/Data/BTC.xlsx",
            "D:/BTC Regression Model/Data/Gold.xlsx",
            "D:/BTC Regression Model/Data/LTC.xlsx",
            "D:/BTC Regression Model/Data/NDX100.xlsx",
            "D:/BTC Regression Model/Data/Oil.xlsx")
df_raw <- read.xlsx(file = files[1], sheetName = 1)
# http://r3dmaotech.blogspot.com/2016/09/r-data-frame-merge-join.html
for(f in files[2:length(files)]){
    df_raw = merge(df_raw, read.xlsx(f, 1), by = "Date")
}

# In[1] Data analyze:

# Plot scatter figue of each indicator versus Close BTC.
ColumnCount = ncol(df_raw)
par(mfrow = c(ColumnCount/3,3), mai =  c(0.3, 0.3, 0.3, 0.3), cex = 0.6)
## why setting fugure in R is so unintuitive?
for(i in 3:ColumnCount){
    plot(x = df_raw[,2], y = df_raw[,i], main = names(df_raw)[i])
}
# We found that Close LTC, Gold value USD and Adj Close NDX is somehow linear with Close BTC

# In[2] Get rid of outliers
RowLengthDecider = Trim(df_raw[,2], trim = 0.015)
df = data.frame(RowLengthDecider)
for(i in 2:ColumnCount){
    df[names(df_raw)[i]] = Trim(df_raw[,i], trim = 0.015)
}


# In[3] Pre-process data
CloseBTC_stat = list("Mean" = mean(df[,2]), "SD" = sd(df[,2]))
for(i in 2:ColumnCount){
    Standardize(df[,i], mean(df[,i]), sd(df[,i]))
}
Indicators = df[,c(4,5,7,9)] #"Gold Value USD", "Open LTC", "Open NDX", "Oil Value USD"
CloseBTC = df[,2]
columns = c("Gold Value USD", "Open LTC", "Open NDX", "Oil Value USD") #sequence here matters?
# trainIndex = createDataPartition(Indicators, p = 0.8, list = FALSE)
## Error: attempt to make a table with >= 2^31 elements
trainIndex = createDataPartition(CloseBTC, p = 0.8, list = FALSE) #temp...? Nah...
# Why Python version create x and y data splits seperatly? x should match with y
Indicators_train = Indicators[trainIndex, ]
Indicators_test = Indicators[-trainIndex, ]
# trainIndex = createDataPartition(CloseBTC, p = 0.8, list = FALSE)
CloseBTC_train = CloseBTC[trainIndex]
CloseBTC_test = CloseBTC[-trainIndex]

# In[4] Establish mulyople dollar regression model.
TrainResult = TrainModel(Indicators_train, CloseBTC_train, Indicators_test, columns)
EvalueModel(TrainResult$regressor, CloseBTC_stat, TrainResult$testX, CloseBTC_test, TrainResult$column)
