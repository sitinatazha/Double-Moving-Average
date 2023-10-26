install.packages("TTR")
install.packages("graphics")
install.packages("tseries")

library(TTR)
library(graphics)
library(tseries)

#Memanggil Data
data<-read.delim("clipboard")
data1<-t(data)
n=length(data1)
n

#Data Time series
data.ts<-ts(data,start=c(2008,1), end=c(2019,12), freq=12)
data.ts

#plot
plot(data.ts)

#uji Stasioner Data
adf.test(data.ts)


#single moving average
SMA<-SMA(data.ts, n=3)
SMA
k=3

#Double Moving Average
m=3
DMA=array(NA, dim=c (n))
for(i in 1:n) {
  DMA[i+(m-1)+(k-1)]=mean(SMA[(i+(k-1)):(i+(m-1))])
}
DMA

#Menghitung nilai a
a=array(NA, dim=c(n))
for(i in 1 : n){
  a[i+(m-1)+(k-1)]=2*SMA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)]  
}
a

#Menghitung nilai b
b=array(NA, dim=c(n))
for(i in 1 : n){
  b[i+(m-1)+(k-1)]=(2/(m-1))*(SMA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)])  
}
b

#Pemulusan
prediksi=array(NA,dim=c(n))
for(i in 1 : n) {
  prediksi[i+(m-1)+(k-1)]=a[i+(m-1)+(k-1)]+b[i+(m-1)+(k-1)]
}
prediksi=ts(prediksi, start= c(2008,1), end=c(2019,12), freq=12)
prediksi

#Peramalan untuk 3 periode kedepan
forecast=function(h) {
  a[n]+b[n]*h
}
Ramalan=forecast(2:4)
Hasil=ts(Ramalan, start=c(2020,1), freq=12)
Hasil

#Ukuran Kesalahan
#MSE
e=array(NA, dim=c(n))
for(i in 1:n) {
  e[i]=(data.ts[i]-prediksi[i])^2
}
e
MSE=mean(e,na.rm=TRUE)
MSE

#MAPE
PE=array(NA, dim=c(n))
for(i in 1:n) {
  PE[i]=abs((data.ts[i]-prediksi[i])/data.ts[i])
}
PE
MAPE=(mean(PE,na.rm=TRUE))*100
MAPE

#Plot Hasil
plot(data.ts, xlab="Waktu", ylab="NTP", main="Nilai Tukar Petani di Papua", lwd=2, col="black", ylim=c(50,150), xlim=c(2008.1,2021.1),type="o", pch=15)
limitDate<-end(data.ts)[1]+(end(data.ts)[2]-1)/frequency(data.ts)
abline(v=limitDate, lty=4)
lines(prediksi, lwd=2, col="darkturquoise", type="o", pch=15)
lines(Hasil, col="Red", type="o", pch=10)
legend("topleft", legend=c("Data Aktual", "Pemulusan", "Peramalan"), col=c("black", "darkturquoise", "red"), lty=1, pch(15,12,10), cex=0.8, inset=0.05)

