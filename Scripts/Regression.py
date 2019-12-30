import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
import statsmodels.api as sm

def EvalueModel(model, scalerOfy, testX, testy, column, i = ''):

    close_pred = scalerOfy.inverse_transform(model.predict(testX))
    close_test = scalerOfy.inverse_transform(testy)

    # 畫圖比較Pred 與 Test。
    f = plt.figure()
    x = np.linspace(close_pred.min(), close_pred.max(), 2)
    plt.scatter(close_test, close_pred)
    plt.plot(x, x, 'purple')
    plt.title('Test vs Pred (using ['+', '.join(column)+'])')
    plt.ylabel('Close BTC Pred')
    plt.xlabel('Close BTC Test')
    plt.legend(['y = x', 'Test-Pred'])
    plt.show()
    plt.close(f)

    # 繪製殘差圖。
    residual = close_test-close_pred
    f = plt.figure()
    plt.scatter(close_test, residual, color = 'r')
    plt.xlabel('Close BTC Test')
    plt.ylabel('Residual')
    plt.title('Residual Plot')
    plt.show()
    plt.close(f)

def TrainModel(trainX, trainy, testX, column, SL = 0.05):

    column = column.copy()
    trainX = np.append(np.ones([len(trainX), 1]), trainX, 1)
    testX = np.append(np.ones([len(testX), 1]), testX, 1)
    column = ['const']+column
    for i in range(trainX.shape[1]):
        regressor = sm.OLS(endog = trainy, exog = trainX).fit()
        if i == 0:
            print(' ')
            print('*'*100)
            print('Start:')
            print(regressor.summary())

        p = list(regressor.pvalues)
        maxP = max(p)
        if maxP > SL:
            maxIndex = p.index(maxP)
            trainX = np.delete(trainX, maxIndex, axis = 1)
            testX = np.delete(testX, maxIndex, axis = 1)
            column.pop(maxIndex)
        else:
            break

    print('\n\n')
    print('Final:')
    print(regressor.summary())

    return regressor, trainX, testX, column

# In[0] 讀入資料:
files = ['BTC.xlsx', 'Gold.xlsx', 'LTC.xlsx', 'NDX100.xlsx', 'Oil.xlsx']
df = pd.read_excel(files[0], 0)
for f in files[1:]:
    df = pd.merge(df, pd.read_excel(f, 0),
                  on = 'Date', how = 'inner').set_index('Date')

del files, f
date = df.index
df.index = range(len(date))
df = pd.concat([df.loc[:,df.columns != 'Close BTC'], df['Close BTC']], axis = 1)

# In[1] 資料分析:

# 繪出各Feature與Close BTC之間的散佈圖。
for i in df.columns[:-1]:
    f = plt.figure()
    plt.scatter(df[i], df['Close BTC'])
    plt.title(i+' vs Close BTC')
    plt.xlabel(i)
    plt.ylabel('Close BTC')
    plt.show()
    plt.close(f)

# 最後發現Close LTC, Gold Value USD與Adj Close NDX與Close BTC線性關係較顯著。

# In[2] 去除Outliers:
uplim = np.percentile(df, 99.85, axis = 0)
downlim = np.percentile(df, 0.15, axis = 0)
logicDf = np.logical_or(df < downlim, df > uplim)

isTrim = logicDf.iloc[:, 0]
for i in range(1, logicDf.shape[1]):
    isTrim = np.logical_or(isTrim, logicDf.iloc[:, i])

isTrim = list(isTrim*isTrim.index)
while 0 in isTrim:
    isTrim.remove(0)

df = df.drop(isTrim)
del uplim, downlim, logicDf, isTrim

# In[3] 資料預處理:
scalerX = StandardScaler()
scalerY = StandardScaler()
dfs = pd.DataFrame(np.hstack([scalerX.fit_transform(df.iloc[:, :-1]),
                              scalerY.fit_transform(df.iloc[:, -1:])]),
                              columns = df.columns)
X = dfs[['Open LTC', 'Open NDX', 'Gold Value USD', 'Oil Value USD']].values
y = dfs['Close BTC'].values
columns = ['Open LTC', 'Open NDX', 'Gold Value USD', 'Oil Value USD']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2)
del X, y

# In[4] 建立多元線性迴歸模型:
regressor, X_train, X_test, columns_i = TrainModel(X_train, y_train, X_test, columns)

# In[5] 驗證多元線性迴歸模型:
EvalueModel(regressor, scalerY, X_test, y_test, columns_i)
