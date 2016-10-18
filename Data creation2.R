#libraries


#install.packages(c("lavaan", "msm", "randomNames", "doParallel", "kernlab", "ggplot2", "e1071", "rpart", "rattle", "rpart.plot", "randomForest", "ROCR"))
library(lavaan)
library(msm)
library(randomNames)
library(doParallel)
library(kernlab)
library(ggplot2)
library(e1071)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)
library(ROCR)

set.seed(34000)
overzicht = function(x){
  print(names(x))
  print(c("minimum" ,min(x)))
  print(c("maximum" ,max(x)))
  print(c("mean", mean(x)))
  print(c("sd", sd(x)))
}
#demographics company
business.line = rep(c("Wholesale Banking", "C&G", "Market Leaders", "corporate staff department"), 2000 )
business.line[business.line == "Wholesale Banking"] = 1
business.line[business.line == "C&G"] = 2
business.line[business.line == "Market Leaders"] = 3
business.line[business.line == "corporate staff department"] = 4
countries=rep(c("Netherlands", "Belgium", "UK", "US", "Spain", "Germany", "Poland",
                "Turkey", "China", "Russia"), each=800)


x=as.data.frame(cbind(business.line, countries))
#demographics
gender=NULL
gender[x$business.line=="1"]=rbinom(2000,1, 0.433 )
gender[x$business.line=="2"]=rbinom(2000,1, 0.59)
gender[x$business.line=="3"]=rbinom(2000,1, 0.4425)
gender[x$business.line=="4"]=rbinom(2000,1, 0.2595)
gender1=NULL
gender1[gender == 1] = "female"
gender1[gender == 0] = "male"
names = randomNames(8000, gender1)  
  
age=rtnorm(8000,37, sd=3, lower=16, upper=75)

length.of.service = rtnorm(8000, 11, 9.75, lower=0, upper=Inf)

#personal characteristics measured on a 1-10 scale. 
extraverted = NULL
extraverted =1*as.numeric(business.line)+rtnorm(8000, 5, lower =1, upper =10)


openess = NULL
openess[business.line == 1] = rtnorm(2000, 4, lower = 1, upper = 10 )
openess[business.line == 2] = rtnorm(2000, 2, lower = 1, upper = 10)
openess[business.line == 3] = rtnorm(2000, 4, lower = 1, upper = 10)
openess[business.line == 4] = rtnorm(2000, 5, lower = 1, upper = 10)

stable = NULL
stable[business.line == 1] = rtnorm(2000, 8, lower = 1, upper = 10)
stable[business.line == 2] = rtnorm(2000, 4, lower = 1, upper = 10)
stable[business.line == 3] = rtnorm(2000, 6, lower = 1, upper = 10)
stable[business.line == 4] = rtnorm(2000, 2, lower = 1, upper = 10)

conscientious = NULL
conscientious[business.line == 1] = rtnorm(2000, 7, lower = 1, upper = 10)
conscientious[business.line == 2] = rtnorm(2000, 6, lower = 1, upper = 10)
conscientious[business.line == 3] = rtnorm(2000, 8, lower = 1, upper = 10)
conscientious[business.line == 4] = rtnorm(2000, 7, lower = 1, upper = 10)

agreeableness = NULL
agreeableness[business.line == 1] = rtnorm(2000, 3, lower = 1, upper = 10)
agreeableness[business.line == 2] = rtnorm(2000, 6, lower = 1, upper = 10)
agreeableness[business.line == 3] = rtnorm(2000, 5, lower = 1, upper = 10)
agreeableness[business.line == 4] = rtnorm(2000, 8, lower = 1, upper = 10)

#antecedent  variables
service.climate = rtnorm (8000, 3, lower=1, upper=5)
leadership =  rtnorm(8000, mean=3, sd=0.5, upper=5, lower=1)
crosssl = service.climate * leadership

#hard variables
salary= +10000+ 200 * length.of.service  -10000 * gender + 300 * extraverted + rtnorm(8000,57000, 30000, lower=8000, upper=500000)
lti = 1000 * gender + 200 * conscientious + rtnorm(8000, 25000, 15000, lower=800, upper=1000000)
croslala=salary/lti

#Mediating variables
sustainable.engagement = NULL
sustainable.engagement = -2.7+0.04 * service.climate + 0.22 * leadership + 0.17 * crosssl + rtnorm(8000, 3, 0.01, lower = 1, upper = 5)


burnout = NULL
burnout = -1- 0.34* sustainable.engagement+ 0.3*conscientious +-.022 *service.climate+rtnorm(8000, 3, 0.3, lower = 1, upper = 5)

 

#outcome variables 

flight.risk =+18+1.54*burnout +  -3.61 * sustainable.engagement+ 0.1 *croslala +rbinom(8000, 1, 0.34)
flight.risk=log10(flight.risk)
length(flight.risk[flight.risk<1.23])
flight.risk[flight.risk<1.23]= 0
flight.risk[flight.risk>1.23]=1


performance.score = -0.5+0.34*sustainable.engagement - 0.05 * burnout + 0.03 * leadership + 0.024 * extraverted+ rtnorm(8000, 3.1, 0.20, upper=5, lower=1)

innovative.behaviour = rtnorm(8000, 3, 0.5, upper = 5, lower =1)


# data set for us 
dataset=as.data.frame(cbind(names, gender, age,length.of.service,countries,business.line, agreeableness, conscientious, 
                             extraverted, leadership, service.climate, lti, salary, stable, 
                       burnout,sustainable.engagement, flight.risk,innovative.behaviour, openess, performance.score))

#making them numeric
x1=c(2,3,4,7,8,9,10,11,12,13,14,15,16,17,18,19)
for(i in x1){
  dataset[,i]= as.numeric(as.character(dataset[,i]))
}

#dataset for them
dataset2=as.data.frame(cbind(names, gender, age,length.of.service,countries,business.line, agreeableness, conscientious, 
                            extraverted, leadership, service.climate, lti, salary, stable, 
                            burnout,sustainable.engagement, flight.risk,innovative.behaviour))
#recoding
dataset2$gender=as.numeric(dataset2$gender)
dataset2$gender[dataset2$gender==1]="female"
dataset2$gender[dataset2$gender==2]="male"
dataset2$business.line=as.numeric(dataset2$business.line)
dataset2$business.line[dataset2$business.line==1]= "Wholesale Banking"
dataset2$business.line[dataset2$business.line==2]= "C&G"
dataset2$business.line[dataset2$business.line==3]= "Market Leaders"
dataset2$business.line[dataset2$business.line==4]= "Corporate Staff Department"  


#

write.table(dataset2, file = 'c:/Users/goeyd/r projects/mt simulation/dataset.csv')


