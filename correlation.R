#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


library("Hmisc")
library("corrplot")
library("pheatmap")
library("scatterplot3d")
library("ggplot2")

#ggplot(data2,aes(x=TCAP,y=GRB7,size=TEAD2))+geom_point()

#scatterplot3d(x,y,z)

data=read.delim("D:/demo/demo.txt",head=T,sep="\t")
data2=data[,2:ncol(data)]
rownames(data2)=data[,1:1]

#pheatmap(data2)
res=cor(data2)
col=colorRampPalette(c("red","white","blue"))
corrplot(res,type="full",order="hclust",diag=TRUE,number.cex=1.1,addCoef.col="black",col=col(100))
res1=cor.mtest(data2,conf.level=.95)
corrplot(res,p.mat=res1$p,insig="p-value",sig.level=-1,col=col(100)),type="full",order="hclust",diag=TRUE,number.cex=1.1,addCoef.col="black",col=col(100),)


x=data2$TCAP
y=data2$ERBB2
#z=data2$PGAP3
summary(lm(y~x,data2))





plot(x,y,xlab="TCAP",ylab="ERBB2",col="blue",pch=16,cex=1.5,xlim=c(-3,3),ylim=c(-3,3))
abline(h=0,col="green")
abline(v=0,col="red")
text(x,y,rownames(data2),cex=0.7,pos=4)



