library(caret)
library(xlsx)
library(DescTools)
EvalueModel <- function(model, scalerOfy, testX, testy, column, i = ''){

}

TrainModel <- function(trainX, trainY, testX, column, SL = 0.05){

}

Standardize <- function(X, mean, sd){
    value = (X-mean)/sd
    eval.parent(substitute(X <- value))
}

InvertStandardize <- function(X, mean, sd){
    value = X*sd+mean
    eval.parent(substitute(X <- value))
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
Indicators = df[,c(4,5,7,9)] #'Open LTC', 'Open NDX', 'Gold Value USD', 'Oil Value USD'
CloseBTC = df[,2]
columns = c("Open LTC", "Open NDX", "Gold Value USD", "Oil Value USD")
# trainIndex = createDataPartition(Indicators, p = 0.8, list = FALSE)
## Error: attempt to make a table with >= 2^31 elements
trainIndex = createDataPartition(CloseBTC, p = 0.8, list = FALSE) #temp
X_train = Indicators[trainIndex, ]
X_test = Indicators[-trainIndex, ]
# trainIndex = createDataPartition(CloseBTC, p = 0.8, list = FALSE)
y_train = CloseBTC[trainIndex]
y_test = CloseBTC[-trainIndex]

# In[4] Establish mulyople dollar regression model.
TrainResult = TrainModel(X_train, y_train, X_test, columns)
EvalueModel(TrainResult$regressor, CloseBTC_stat, TrainResult$X_test, y_test, TrainResult$columns_i)
