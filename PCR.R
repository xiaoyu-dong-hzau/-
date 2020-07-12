planedata<- read.csv("C:/Users/�ɰ���С��/Desktop/�񺽿�����.csv",header=T)#��������
#����ͳ���Է���
summary(planedata)
#��ɢ��ͼ
plot(planedata$y)
plot(planedata$x1,planedata$y)
plot(planedata$x2,planedata$y)
plot(planedata$x3,planedata$y)
plot(planedata$x4,planedata$y)
plot(planedata$x5,planedata$y)
#����ͼ
boxplot(planedata$y)#��ֱ����

#ֱ��ͼ
hist(planedata$y)
hist(planedata$y,breaks=10)#�ֶ����������

#�ܶ�ͼ
plot(density(planedata$y))



#ͼ��
plot(planedata[,-1])#����1


#���������������(������һ�е����)
cor1<-cor(planedata[,-1])
#cor1
#����ͨ�����Իع�
lm1<-lm(y~x1+x2+x3+x4+x5,data=planedata)
summary(lm1)
#���ع�����
kappa(cor1,exact = TRUE)
#���ɷֻع�
dates<-data.frame(scale(planedata[,-1]))#��ԭʼ���ݽ��б�׼��
pr1<-princomp(~x1+x2+x3+x4+x5,data=dates,cor=T)#��5���Ա��������ɷַ�����cor=T��ʾ�����ϵ���������PCA
summary(pr1,loading=TRUE)
pr1$scores[,1:2]#���ǰ�������ɷֵĵ÷�
pr2<-pr1$scores[,1:2]
dates$z1<-pr2[,1]
dates$z2<-pr2[,2]#��z1,z2���뵽dates���ݼ���
pr2<-lm(y~z1+z2-1,data=dates)#y���������ɷֽ����ع�ģ��
summary(pr2)