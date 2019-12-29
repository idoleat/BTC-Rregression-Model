library(crate)
library(xlsx)
EvalueModel <- function(model, scalerOfy, testX, testy, column, i = ''){

}

TrainModel <- function(trainX, trainY, testX, column, SL = 0.05){

}

# In[0] Read data:
## need to fix the path to relative path....
## R is quite annoying to do so. Alt least I haven't found the answer yet.
files = c("D:/BTC Regression Model/Data/BTC.xlsx",
            "D:/BTC Regression Model/Data/Gold.xlsx",
            "D:/BTC Regression Model/Data/LTC.xlsx",
            "D:/BTC Regression Model/Data/NDX100.xlsx",
            "D:/BTC Regression Model/Data/Oil.xlsx")
df <- read.xlsx(file = files[1], sheetName = 1)
# http://r3dmaotech.blogspot.com/2016/09/r-data-frame-merge-join.html
for(f in files[2:length(files)]){
    df = merge(df, read.xlsx(f, 1), by = "Date")
}

# In[1] Data analyze:

# Plot scatter figue of each indicator versus Close BTC.
ColumnCount = ncol(df)
par(mfrow = c(ColumnCount/3,3), mai =  c(0.3, 0.3, 0.3, 0.3), cex = 0.6)
for(i in 3:ColumnCount){
    plot(x = df[,2], y = df[,i], main = names(df)[i])
}
