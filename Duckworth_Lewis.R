library(nls2)
xavg_run<-NULL
avg_run<-NULL


## Read datafrom csv file
cric<-read.csv("04_cricket_1999to2011.csv")
cric<- cric[cric[,which(colnames(cric)=="Innings")]<2,,drop=FALSE]
cric<-cbind(50-cric[,which(colnames(cric)=="Over"),drop=FALSE],cric[,which(colnames(cric)=="Runs.Remaining"),drop=FALSE],cric[,which(colnames(cric)=="Total.Out"),drop=FALSE])


## Extracting relevant information from csv file
for(i in 0:49){
  data<-cric[cric[,which(colnames(cric)=="Over")]==i,,drop=FALSE]
  for( j in 0:9){
    xdata<-data[data[,which(colnames(data)=="Total.Out")]==j,,drop=FALSE]
    xavg_run<-c(xavg_run,mean(xdata[,which(colnames(xdata)=="Runs.Remaining")]))  
    
   }
  avg_run<-rbind(avg_run,xavg_run)
  xavg_run<-NULL
}
avg_run<-round(avg_run,0)
rownames(avg_run)<-0:49
colnames(avg_run)<-0:9
avg_run[is.na(avg_run)]<-0


##  Plot the scatter plot
plot(0:49,avg_run[,1],col="white")
for(i in 2:10){
  points(0:49,avg_run[,i],col="white")
}

coeff<-NULL
coeffL<-NULL
err<-NULL


## Fit the data
for(i in 1:10){
  fit<-nls(avg_run[,i]~Z*(1-exp(-L*0:49/Z)),start=list(Z=268, L=11),control = list(maxiter = 100))
  lines(0:49,predict(fit), col="red")
  coeff<-rbind(coeff,coef(fit)[1])
  coeffL<-rbind(coeffL,coef(fit)[2])
 
}


rownames(coeff)<-0:9
print(coeff)
print("Estimate for L")
print(coeffL[5])




