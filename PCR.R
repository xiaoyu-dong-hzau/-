planedata<- read.csv("C:/Users/可爱的小鱼/Desktop/民航客运量.csv",header=T)#读入数据
#进行统计性分析
summary(planedata)
#画散点图
plot(planedata$y)
plot(planedata$x1,planedata$y)
plot(planedata$x2,planedata$y)
plot(planedata$x3,planedata$y)
plot(planedata$x4,planedata$y)
plot(planedata$x5,planedata$y)
#箱线图
boxplot(planedata$y)#竖直放置

#直方图
hist(planedata$y)
hist(planedata$y,breaks=10)#手动定义区间段

#密度图
plot(density(planedata$y))



#图集
plot(planedata[,-1])#方法1


#计算变量间的相关阵(除掉第一列的年份)
cor1<-cor(planedata[,-1])
#cor1
#做普通的线性回归
lm1<-lm(y~x1+x2+x3+x4+x5,data=planedata)
summary(lm1)
#多重共线性
kappa(cor1,exact = TRUE)
#主成分回归
dates<-data.frame(scale(planedata[,-1]))#将原始数据进行标准化
pr1<-princomp(~x1+x2+x3+x4+x5,data=dates,cor=T)#对5个自变量做主成分分析，cor=T表示用相关系数矩阵进行PCA
summary(pr1,loading=TRUE)
pr1$scores[,1:2]#输出前两个主成分的得分
pr2<-pr1$scores[,1:2]
dates$z1<-pr2[,1]
dates$z2<-pr2[,2]#将z1,z2加入到dates数据集中
pr2<-lm(y~z1+z2-1,data=dates)#y对两个主成分建立回归模型
summary(pr2)
