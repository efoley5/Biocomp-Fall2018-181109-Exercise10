data=read.csv("data1.csv", stringsAsFactors = F)
head(data)
library(deSolve)
library(ggplot2)

###Problem 2 
ddSim<- function(t,y,p){
  N1=y[1]
  N2=y[2]
  
  r1=p[1]
  r2=p[2]
  a11=p[3]
  a12=p[4]
  a21=p[5]
  a22=p[6]
  
  dN1dt=r1*(1-(N1*a11)-(N2*a12))*N1
  dN2dt=r2*(1-(N2*a22)-(N1*a21))*N2
  return(list(c(dN1dt,dN2dt)))
}

#Case 1 
params=c(0.5,0.5,0.5,0.1,0.5,0.1)
NO=c(1,1)
times= 1:100

modelSim=ode(y=NO,times=times,func=ddSim,parms=params)
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])
ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic() 


#Case 2 
params=c(1,1,0.1,0.5,0.4,0.4,0.1)
NO=c(1,1)
times= 1:100

modelSim=ode(y=NO,times=times,func=ddSim,parms=params)
modelOutput=data.frame(time=modelSim[,1],N=modelSim[,2])
ggplot(modelOutput,aes(x=time,y=N))+geom_line()+theme_classic() 






out2=data.frame(time=sim2[,1],normal=sim2[,2],tumor=sim2[,3])
ggplot(out2,aes(x=time,y=normal))+geom_line()+geom_line(data=out2,mapping=aes(x=time,y=tumor),col='red')+theme_classic()


