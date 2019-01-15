#Estep


#l
likely1<-function(A,a,b,E_A,alpha){
  A<-exp(A)
  a<--exp(a)
  
  beta<-beta(A,f,a,deltat,b,E_A,Tmax)
  beta_k<-c(rep(beta1,30),rep(beta2,30),rep(beta3,30),rep(beta4,30))
  aa<-rkj*log(sqrt(x_r/beta_k)+sqrt(beta_k/x_r))
  bb<-(Nkj-rkj)*log(sqrt(x_r/beta_k)+sqrt(beta_k/x_r))
  #1/M*sum(())
  x1<-rbisa(1000,alpha_hat,beta1)
  x2<-rbisa(1000,alpha_hat,beta2)
  x3<-rbisa(1000,alpha_hat,beta3)
  x4<-rbisa(1000,alpha_hat,beta4)
  ##
  Q1=-N*log(alpha_hat^2)
  ##
  Q2=(log((beta_k/x)^(1/2)+(x/beta_k)^(1/2)))
  ##
  Q3=-1/(2*alpha_hat^2)*sum(beta_k/x+x/beta_k-2)
  #
  lik<-Q1+Q2+Q3

}

rbisa_re_low<-function(n,scale,shape){
B<-runif(n)
beta1<-beta(A,f1,a,deltat1,b,E_A,Tmax1)
beta2<-beta(A,f2,a,deltat2,b,E_A,Tmax2)
beta3<-beta(A,f3,a,deltat3,b,E_A,Tmax3)
beta4<-beta(A,f4,a,deltat4,b,E_A,Tmax4)
beta<-c(rep(beta1,30),rep(beta2,30),rep(beta3,30),rep(beta4,30))
C<-pbisa(inspection_time,alpha1,beta)
A <- rnorm(n)
temp1 <- A * shape
temp1 <- temp1 * sqrt(4 + temp1^2)
ans1 <- (2 + A^2 * shape^2 + temp1) * scale/2
ans2 <- (2 + A^2 * shape^2 - temp1) * scale/2
ans <- ifelse(A < 0, pmin(ans1, ans2), pmax(ans1, ans2))
ans[shape <= 0] <- NaN
ans[scale <= 0] <- NaN
ans
}
optimr(likely1)

