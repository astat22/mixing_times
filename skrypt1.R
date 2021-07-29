#setwd("E:/Prace naukowe/Praca in¿ynierska")


library(ggplot2)
 
library(RColorBrewer)  
library(reshape2) 
p <- plot_ly(z = volcano, type = "heatmap")


M <- as.matrix(read.table("fullEpsMT.txt", header = FALSE))

xpom = 5:22
xpom2 = length(xpom)-1
X=40*xpom
ypom=15:42
Y=0.51-0.01*ypom  

m<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X),c(Y)))
for(i in 1:xpom2)
{
  for(j in 1:length(ypom))
  {
    m[i,j]=sum(M[,ypom[j]]>X[i] & M[,ypom[j]]<X[i+1])
  }
}



mat.melted <- melt(m,value.name = "count", varnames=c('t', 'eps')) 
ggplot(mat.melted, aes(x = t, y = eps, fill = count)) + geom_tile() 

#przesuniete o srednia
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrRd')), space='Lab') 
means = c(1:42)
M2=matrix(nrow=3000,ncol=42)
for(i in 1:42)
{
  means[i]=mean(M[,i])
  M2[,i]=M[,i]-means[i]
}

xpom = 1:30
xpom2 = length(xpom)-1
X=20*xpom-300
ypom=10:42
Y=0.51-0.01*ypom  

m2<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X),c(Y)))
for(i in 1:xpom2)
{
  for(j in 1:length(ypom))
  {
    m2[i,j]=sum(M2[,ypom[j]]>X[i] & M2[,ypom[j]]<X[i+1])
  }
}
mat.melted2 <- melt(m2,value.name = "count", varnames=c('t', 'eps')) 
ggplot(mat.melted2, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))

#ustandaryzowana wariancja
vars = c(1:42)
M3=matrix(nrow=3000,ncol=42)
for(i in 1:42)
{
  vars[i]=sd(M2[,i])
  M3[,i]=M2[,i]/vars[i]
}

xpom = -10:10
xpom2 = length(xpom)-1
X=xpom/5
ypom=18:42
Y=0.51-0.01*ypom  

m3<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X),c(Y)))
for(i in 1:xpom2)
{
  for(j in 1:length(ypom))
  {
    m3[i,j]=sum(M3[,ypom[j]]>X[i] & M3[,ypom[j]]<X[i+1])
  }
}
mat.melted3 <- melt(m3,value.name = "count", varnames=c('t', 'eps')) 
ggplot(mat.melted3, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))


#zlogarytmowane ustandaryzowane
varsL = c(1:42)
meansL = c(1:42)
M4=log(M)
M5=matrix(nrow=3000,ncol=42)
for(i in 1:42)
{
  varsL[i]=sd(M4[,i])
  meansL[i]=mean(M4[,i])
  M5[,i]=(M4[,i]-meansL[i])/varsL[i]
}

xpom = -10:10
xpom2 = length(xpom)-1
X=xpom/5
ypom=18:42
Y=0.51-0.01*ypom  
m4<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X),c(Y)))
for(i in 1:xpom2)
{
  for(j in 1:length(ypom))
  {
    m4[i,j]=sum(M5[,ypom[j]]>X[i] & M5[,ypom[j]]<X[i+1])
  }
}
mat.melted4 <- melt(m4,value.name = "count", varnames=c('t', 'eps')) 
ggplot(mat.melted4, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))



##############################################
display.brewer.pal(9,'Greens')
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'Greens')), space='Lab') 
data2 <- as.matrix(read.table("daneFullEps2", header = FALSE))
xpom = 1:75
xpom2 = length(xpom)-1
X=4*xpom+100
X2=4*xpom-160
X3=X2/40
ypom=1:150
sds=c(1:length(ypom))
means=c(1:length(ypom))
Y=0.35-0.001*ypom  
M6=matrix(nrow=dim(data2)[1],ncol=dim(data2)[2])
M7=matrix(nrow=dim(data2)[1],ncol=dim(data2)[2])
M8=matrix(nrow=dim(data2)[1],ncol=dim(data2)[2])
for(i in ypom)
{
  sds[i]=sd(data2[,i])
  means[i]=mean(data2[,i])
  M6[,i]=data2[,i]-means[i]
  M7[,i]=M6[,i]/sds[i]
  M8[,i]=(log(data2[,i])-mean(log(data2[,i])))/sd(log(data2[,i]))
 
}



m5<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X),c(Y)))
m6<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X2),c(Y)))
m7<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X3),c(Y)))
m8<-matrix(nrow=length(X),ncol=length(Y),dimnames = list(c(X3),c(Y)))
for(i in 1:xpom2)
{
  for(j in 1:length(ypom))
  {
    m5[i,j]=sum(data2[,ypom[j]]>X[i] & data2[,ypom[j]]<X[i+1])
    if(j>25)
    {
      m6[i,j]=sum(M6[,ypom[j]]>X2[i] & M6[,ypom[j]]<X2[i+1])
      m7[i,j]=sum(M7[,ypom[j]]>(X3[i]) & M7[,ypom[j]]<(X3[i+1]))
      m8[i,j]=sum(M8[,ypom[j]]>(X3[i]) & M8[,ypom[j]]<(X3[i+1]))
      
    }
  }
}
mat.melted5 <- melt(m5,value.name = "count", varnames=c('t', 'eps')) 
mat.melted6 <- melt(m6,value.name = "count", varnames=c('t', 'eps')) 
mat.melted7 <- melt(m7,value.name = "count", varnames=c('t', 'eps')) 
mat.melted8 <- melt(m8,value.name = "count", varnames=c('t', 'eps')) 

ggplot(mat.melted5, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))
ggplot(mat.melted6, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))
ggplot(mat.melted7, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))
ggplot(mat.melted8, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))

##############################################
xpom = 1:75
xpom2 = length(xpom)-1

ypom=1:150
Y=0.35-0.001*ypom 
M6=matrix(nrow=dim(data2)[1],ncol=dim(data2)[2])
for(i in ypom)
{
  M6[,i]=data2[,i]-mean(data2[,i])
}

for(i in 1:xpom2)
{
  for(j in 25:length(ypom))
  {
    m6[i,j]=sum(M6[,ypom[j]]>X[i] & M6[,ypom[j]]<X[i+1])
  }
}
mat.melted6 <- melt(m6,value.name = "count", varnames=c('t', 'eps')) 

ggplot(mat.melted6, aes(x = t, y = eps, fill = count)) + geom_tile()  +scale_fill_gradientn(colours = hm.palette(100))
##########
#Analiza statystyczna Modelu Barabasi-Alberta
library(nortest)
library(normwhn.test)
library(ddst)
library(xtable)
MB <- as.matrix(read.table("hugeMT.txt", header = FALSE))
hist(MB,breaks=15,col="blue")
logMB=log(MB)
hist(logMB,breaks=8)

cvm.test(logMB)
ad.test(logMB)
test<-rnorm(100)
ad.test(test)
cvm.test(test)
t.test(logMB,mu=)


#regresja parametru mu
allB <- as.matrix(read.table("daneBarabasiMT.txt"))
logallB= log(allB)


pvs<-matrix(0,34,5)
Bmeans = c(1:34)
BmediansExp = c(1:34)
colnames(pvs)<-c("Cramer-von Mises","Anderson-Darling","Shapiro","Pearson","Neyman")
rownames(pvs)<-c(2:35)
for(i in 1:34)
{

    pvs[i,1] = cvm.test(logallB[,i])$p.value>0.05   #Cramera-von Misesa
    pvs[i,2] =  ad.test(logallB[,i])$p.value>0.05     #Andersona-Darlinga
    #normality.test1(logallB[,i])$p.value #d'Agostino oparty o skoœnoœæ i kurtozê
    pvs[i,3] = shapiro.test(logallB[,i])$p.value>0.05 #o statystyki pozycyjne
    pvs[i,4] = pearson.test(logallB[,i])$p.value>0.05 #test pearsona
    pvs[i,5] = ddst.norm.test(logallB[,i],compute.p=TRUE)$p.value>0.05 #o statystyki pozycyjne
    Bmeans[i] = mean(logallB[,i])
    BmediansExp[i] = median(allB[,i])
    
}
pvs = t(pvs)
options(xtable.floating=FALSE)
xtable(pvs,digits=0)
plot(Bmeans,x=c(2:35)*10)
plot(exp(Bmeans),x=c(2:35)*10)
Ns <- c(2:35)*10
#c(mean(allB[,33]),mean(allB[,34]))
fit<-lm(BmediansExp ~ Ns)
coef(fit)
summary(fit)
predict(fit,newdata=list(Ns=3000),interval="confidence",level=0.95)

t.test(
  x=logMB,
  mu=log(predict(fit,newdata=list(Ns=3000),interval="confidence",level=0.95)[1]),
  alternative="greater")

citation(package="ddst")

