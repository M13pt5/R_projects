#EX_1
t=c(1.661,1.985)
f = 2/(1+exp(1))
a = 1/(1+exp(1))
b = 2*exp(1)/(1+exp(1))^2
c = 0
S = matrix(c(0.02,0.14,-0.07,0.14,5.28,-2.38,-0.07,-2.38,1.1),3,3)

V=c(a,b,c)%*%S%*%matrix(c(a,b,c),3,1)

T = f/(sqrt(V))

T>t[1] # -> FALSE, we don't reject H0'

#confidence interval:
c(f-t[2]*sqrt(V),f+t[2]*sqrt(V)) 

#it's not possible to derive an estimator for sigma^2, because we don't have the observations with true values to compare it with our estimations, (formula for S())
#########################################
#EX_3
set.seed(22101847)
#parameters
theta = c(2,5,0.5)
sigm = 1
#data
x = runif(100,0,4)#seq(0.04, 4, by=0.04)
x = sort(x, decreasing = FALSE)
fun = function(x,theta){
  theta[1]*x+theta[2]/(theta[3]+3*x^2)
}
f = fun(x,theta)
epsilon = rnorm(100, sigm)
Y = f+epsilon
Func = data.frame(x = x, y = Y)
###ex.3.a
#plot
plot(x,Y, xlab = "x", ylab = "y", pch= 20)
lines(x,f, col = "blue", lwd=2)
#estimation: nonlinear regression
n=length(Y)
form=as.formula(y~theta1*x+theta2/(theta3+3*x^2))
start=c(2,3,1)
model=nls(form, data = Func, start=list(theta1=start[1],theta2=start[2],theta3=start[3])) 
summary(model)
#plot
est_t = coef(model)
lines(x,fun(x,est_t),col="red",lwd=2)

#estimate of the variance
hat.th = est_t
RSS = deviance(model) 
hat.var =  RSS/(n-2)
hat.var
#estimated covariance matrix
cov.est=vcov(model)
cov.est

cor(Y,predict(model))

###ex.3.b
#asymptotic normality
lb={}
ub={}
#96% interval: \alpha = 0.04, 1-\alpha/2=
for(i in 1:3){
  lb[i]=est_t[i]-qt(0.98,n-length(est_t))*sqrt(cov.est[i,i])
  ub[i]=est_t[i]+qt(0.98,n-length(est_t))*sqrt(cov.est[i,i])
  }
ci=cbind(lb,ub)
rownames(ci)=names(est_t)
ci

#bootstrap

B=1000 # 1000
par.boot=matrix(NA,B,length(est_t))
rownames(par.boot)=paste("B",1:B,sep="")
colnames(par.boot)=names(est_t)
res.centered=resid(model)-mean(resid(model))

for(b in 1:B){
  res=sample(res.centered,replace=T)
  # Calculate bootstrap values for the response
  yboot=fitted(model)+res #Y*_1,...Y*_n
  # Fit model using new response and get bootstrap estimates for parameter 
  modelBoot=nls(yboot~theta1*x+theta2/(theta3+3*x^2),
                data=data.frame(yboot,x),start=list(theta1=start[1],theta2=start[2],theta3=start[3]))
  # Store estimated (by bootstrap) parameters  
  par.boot[b,]=coef(modelBoot) #\theta*_1,..., \theta*_B
}
# Bootstrap estimated theta's
coef(modelBoot)
# Bootstrap estimated variances
c(var(par.boot[,1]),var(par.boot[,2]),var(par.boot[,2]))
# Compute the bootstrap 96% confidence intervals for the thetas
lb.boot=2*est_t-apply(par.boot,2,quantile,prob=0.98)
ub.boot=2*est_t-apply(par.boot,2,quantile,prob=0.02)
cbind(lb.boot,ub.boot)

###ex.3.c
## estimate of the mean response f(4,theta), by assymptotic normality:
f3=as.numeric(fun(3,est_t))
f3

## 98%-confidence interval for the mean response f(4,theta)
grad<-function(x,theta){rbind(x, 1/(theta[3]+3*x^2), -theta[2]/(theta[3]+3*x^2)^2)}
gradvec=grad(4,est_t)
se=sqrt(t(gradvec)%*%vcov(model)%*%gradvec)
l_b=f3-qt(0.99,n-length(est_t))*sqrt(hat.var) 
u_b=f3+qt(0.99,n-length(est_t))*sqrt(hat.var) 
c(l_b,u_b) # approximate confidence interval for f(4,theta)

###ex.3.d
fe=fun(x,est_t)
mygrad=grad(x,est_t)
se<-sqrt(apply(mygrad,2,function(xx) t(xx)%*%vcov(model)%*%xx))
l_b<-fe-qt(0.99,n-length(est_t))*se 
u_b<-fe+qt(0.99,n-length(est_t))*se
# plot of confidence intervals for all x in [0,4]
plot(x,Y,xlab="x",ylab="y",pch= 20) 
polygon(c(x,rev(x)),c(l_b,rev(u_b)),col="lightgrey",border=NA)
lines(x,fe,lwd=1, col="red")

###ex.3.e

H=(est_t[1]-theta[2])/sqrt(cov.est[1,1])
if(abs(H)>1.661){
  print("Rreject H_0")
}
