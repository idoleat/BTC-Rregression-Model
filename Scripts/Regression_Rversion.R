library(crate)
library(xlsx)
EvalueModel <- function(model, scalerOfy, testX, testy, column, i = ''){

}

TrainModel <- function(trainX, trainY, testX, column, SL = 0.05){

}

# In[0] 讀入資料:
files = c("C:/Users/dppss/OneDrive/E3/FinalProject/BTC.xlsx",
            "C:/Users/dppss/OneDrive/E3/FinalProject/Gold.xlsx",
            "C:/Users/dppss/OneDrive/E3/FinalProject/LTC.xlsx",
            "C:/Users/dppss/OneDrive/E3/FinalProject/NDX100.xlsx",
            "C:/Users/dppss/OneDrive/E3/FinalProject/Oil.xlsx")
df <- read.xlsx(file = files[1], sheetName = 1)
# http://r3dmaotech.blogspot.com/2016/09/r-data-frame-merge-join.html
for(f in files[2:length(files)]){
    df = merge(df, read.xlsx(f, 1), by = "Date")
}

# In[1] 資料分析:

# 繪出各Feature與Close BTC之間的散佈圖。
