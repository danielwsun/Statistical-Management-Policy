#time unit: second

machines<-10
mu<-0.02
rho<-0.6 #plain utilisation
trho<-0.85 #total util

lambda<-(rho)*machines*mu


f<-c(-1,100)
for(i in 0:99){
  if(i <= machines){
    f[i]<-lambda^i/(factorial(i)*mu^i)
  }else{
    f[i]<-lambda^i/(factorial(machines)*(machines^(i-machines))*(mu^i))
  }
}
  
g<-c(-1,100)
for(i in 0:99){
  g[i]<-sum(f[i:99])
}


l<-c(1:machines)
key<-machines*trho*mu/lambda - 1
lower<-(f[l]*l*mu/lambda)/(g[l])
upper<-f[l]/(g[l]-f[l])

kp<-c(-1)
for(i in 1:machines){
  if((lower[i]<= key) & (upper[i]>= key)){
    kp<-append(kp,i,after=length(kp))
  }
}



pp<-c(-1)
for(i in 2:length(kp)){
  k<-kp[i]
  xkh<-(key*g[k]-f[k-1])/(g[k]*f[k] - g[k+1]*f[k-1])
  rh<-xkh*lambda*f[k]
  xkl<-(f[k]-key*g[k+1])/(g[k]*f[k] - g[k+1]*f[k-1])
  rl<-xkl*lambda*f[k-1]
  p<-rh/(rh+rl)
  pp<-append(pp, p, after=length(pp))
}

