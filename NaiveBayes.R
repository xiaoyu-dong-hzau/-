bank <- read.csv("C:/Users/可爱的小鱼/Desktop/bank.csv",header=T,sep=";")#读入数据
n <- nrow(bank)
#对原数据按照2:1进行分割
index<-sample(1:nrow(bank),round(nrow(bank)*2/3))
length(index)
bank_training <- bank[index,] #训练集
bank_test <- bank[-index,]#测试集
library(e1071)#调用 e1071 包
m <- naiveBayes(y~., data=bank_training,laplace=1,na.action=na.pass)
#训练模型
result<-predict(m,bank_test,type='raw')#对测试数据进行预测
result1= ifelse(result[,1]<result[,2],1,0)
result2<-data.frame(result1)
result_2<-result2[,1]
m_evo<-table(result_2,bank_test$y)#产生一个混淆矩阵
#计算模型的准确率、灵敏度、特异度、精确度、F_score
tp = m_evo[1,1]
tn = m_evo[2,2]
fp = m_evo[1,2]
fn = m_evo[2,1]
accuracy = (tp + tn)/(tp + tn + fp + fn)
sensitive = tp/(tp + fn)
specificity = tn/(tn + fp)
precision = tp/(tp + fp)
F_score = (2*precision*sensitive)/(precision+sensitive)
print('准确率、灵敏度、特异度、精确度分别为：')#依次输出
print(c(accuracy,sensitive,specificity,precision))
install.packages('pROC')
library(pROC)#载入一个 pROC 包
roc1 <- roc(bank_test$y,result[,2],levels=c('no','yes'),direction="<")#画 ROC 曲线
plot(roc1,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="gray", print.thres=TRUE)
