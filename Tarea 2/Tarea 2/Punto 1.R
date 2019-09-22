x = c(3, 4.3, 6, 7, 9.1, 10.3)
N = 6
y = c(0,1,2,2,4,3)

L = matrix(c(0.5,0.5,0,0,0,0,
             1/3,1/3,1/3,0,0,0,
             0,1/3,1/3,1/3,0,0,
             0,0,1/2,1/2,0,0,
             0,0,0,1/2,1/2,0,
             0,0,0,0,1/2,1/2), ncol = 6)


Lfun=function(h){
  L=matrix(rep(0,times=N*N), ncol=N)
  for(i in 1:N){
    cond = vector()
    zz=rep(x[i],times=N)
    for(j in 1:N){
      cond[j] = ifelse(abs(x[i]-x[j]) > (h), 0, 1)
  }
    bottom=sum((1-abs(zz-x)/h)*cond)
    L[i,]=(1-abs(zz-x)/h)*cond/bottom
  }
  return(L)
}

h=2.8 #You can change the bandwidth
L = Lfun(h)
f_hat=c(L%*%y)


#MSE estimation by LOOCV
ll = diag(L)
ff = L%*%y
tra=mean(ll)
MSE=(1/N)*sum(((y-ff)/(1-ll))^2)


# GCV
h = 2
  hh=h
  ll=diag(Lfun(hh))
  ff=c(Lfun(hh)%*%y)
  tra=mean(ll)
  MSEGCV=(1/N)*sum(((y-ff)/(1-tra))^2)

