A = read.table('C:\\Users\\ew\\OneDrive\\Documents\\Studies\\Statistical_Models\\HM3\\psi.txt')

A_split <- split.data.frame(A, A$psi)

Anorm = A_split[["0"]]
Apsi = A_split[["1"]]

plot(A$gpa,A$passed)

plot(Anorm$gpa,Anorm$passed)
plot(Apsi$gpa,Apsi$passed)

sum(A[1])
mean(t(A[3]))

sum(Anorm[1])
sum(Apsi[1])

mean(t(Anorm[1]))
mean(t(Apsi[1]))

par(mfrow=c(2,2))

hist(t(Anorm[3]), breaks = 10, main = ("Regular method"), xlab = "GPA")
hist(t(Apsi[3]), breaks = 10, main = ("Psi method"), xlab = "GPA")
hist(t(A[3]), breaks = 10,  main = ("The whole class"), xlab = "GPA")
#plot(A$gpa)
plot(sort(A$gpa),main = ("Sorted gpa"), ylab = "GPA",  xlab = " ")

plot(A$passed)
plot(sort(A$gpa) ,sort(A$passed))

############
#a


plot(sort(A$gpa) ,(A$passed))
molel=glm(passed~psi+gpa,data = A, family = binomial)
lines(A$gpa,fitted(molel),t="p",pch=16, col = "light green")
summary(molel)

plot(sort(A$gpa) ,(A$passed))
modelt = glm(passed~gpa*psi,data = A, family = binomial)
lines(A$gpa,fitted(modelt),t="p",pch=16, col = "pink")
summary(modelt)


2.338- 2.042*1.041 
3.063+ 2.045* 1.223

##########
#b)

B = 0
Ba = 0
C = 0
Ca = 0
for (i in 1:32) {
  if(A[i,2]==1 && A[i,3]>3 && A[i,1]==1){
    B = 1 + B
  }
  if(A[i,2]==1 && A[i,3]>3){
    Ba = 1 + Ba
  }
  if(A[i,2]==0 && A[i,3]>3 && A[i,1]==1){
    C = 1 + C
  }  
  if(A[i,2]==0 && A[i,3]>3){
    Ca = 1 + Ca
  }  
}

B/Ba #P(passed = 1|gpa > 3,psi = 1) 
C/Ca #P(passed = 1|gpa > 3,psi = 0)




################
#c

B = 0
Ba = 0
C = 0
Ca = 0
for (i in 1:32) {
  if(A[i,2]==1 && A[i,1]==1){
    B = 1 + B
  }
  if(A[i,2]==1){
    Ba = 1 + Ba
  }
  if(A[i,2]==0 && A[i,1]==1){
    C = 1 + C
  }  
  if(A[i,2]==0 ){
    Ca = 1 + Ca
  }  
}

b = B/Ba #P(passed = 1|psi = 1) 
ba = 1- B/Ba #P(passed = 0|psi = 1) 
c = C/Ca #P(passed = 1|psi = 0)
ca = 1- C/Ca #P(passed = 0|psi = 0) 

(b/ba)/(c/ca)

