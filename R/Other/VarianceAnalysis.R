library(WOODCARB3R)

############################
#In these series of for loops we are looking to produce random error into one class of halflife and calculate its variance with other halflife classes.
#within each for loop, a normally distributed error cenetered around 1 with a standard deviation of .2 is applied to a specified halflife class.
#this modified halflife is then used with the untouched halflife classes to calculate the final Carbon Contribution.
#each column in the data frame stores each simulation of applying the random error with the exception of the very first column which is removed after the simulation is complete
#the variance is then taken from 


########### fixes to make
##multiple by randomError only since its centered around 1
##lower number of simulations to 1000 from 2000 because laptop cannot handle it

#Some halflife classes have larger variance range than others, large difference(1,4) and the smallest difference is with 7(note that this is a much smaller range than the others ie 4,000 instead of 3,000,000), using randomError+1
#each loop takes approximately 30 minutes
#running all 13 loops takes roughly 6hours

#fixes made, results are still the same as above, it took roughly 4 hours to complete all simulations for the 13 classes



###########as of february 10, 2017
# change years to just individual years :1990,2000,2010


#table of variances: cols are years and rows are the halflives variances
#each histogram will have the max and min labelled on the bottom AND SAME SCALE


# show histograms of the best and worst of one year (2000) (most and least sensitive basically)

#in description: normal distributed error, ...
#find ben, which variables in halflive sheet should i be lookig at.  dont match up with paper's stated halflives (there are 7 not 13)



#The loops for each of the variables to calculate the amount of variance after 1000 simulations incorporating normally distributed error.
ptmAll<-proc.time()
ptm<-proc.time()

##Variable 1
set.seed(1)
Histdfa<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfa<-cbind(Histdfa,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdfa<-Histdfa[,-1]
OneTrial<-proc.time() - ptm




set.seed(1)
Histdfb<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfb<-cbind(Histdfb,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdfb<-Histdfb[,-1]




set.seed(1)
Histdfc<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,1]*(randomError)
  Histdfc<-cbind(Histdfc,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdfc<-Histdfc[,-1]






##Variable 2
set.seed(1)
Histdf2a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,1]<- halflivesmatters[,2]*(randomError)
  Histdf2a<-cbind(Histdf2a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf2a<-Histdf2a[,-1]




set.seed(1)
Histdf2b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,2]<- halflivesmatters[,2]*(randomError)
  Histdf2b<-cbind(Histdf2b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf2b<-Histdf2b[,-1]




set.seed(1)
Histdf2c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,2]<- halflivesmatters[,2]*(randomError)
  Histdf2c<-cbind(Histdf2c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf2c<-Histdf2c[,-1]






##Variable 3
set.seed(1)
Histdf3a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3a<-cbind(Histdf3a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf3a<-Histdf3a[,-1]




set.seed(1)
Histdf3b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3b<-cbind(Histdf3b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf3b<-Histdf3b[,-1]




set.seed(1)
Histdf3c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,3]<- halflivesmatters[,3]*(randomError)
  Histdf3c<-cbind(Histdf3c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf3c<-Histdf3c[,-1]






##Variable 4
set.seed(1)
Histdf4a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4a<-cbind(Histdf4a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf4a<-Histdf4a[,-1]




set.seed(1)
Histdf4b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4b<-cbind(Histdf4b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf4b<-Histdf4b[,-1]




set.seed(1)
Histdf4c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,4]<- halflivesmatters[,4]*(randomError)
  Histdf4c<-cbind(Histdf4c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf4c<-Histdf4c[,-1]






##Variable 5
set.seed(1)
Histdf5a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5a<-cbind(Histdf5a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf5a<-Histdf5a[,-1]




set.seed(1)
Histdf5b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5b<-cbind(Histdf5b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf5b<-Histdf5b[,-1]




set.seed(1)
Histdf5c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,5]<- halflivesmatters[,5]*(randomError)
  Histdf5c<-cbind(Histdf5c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf5c<-Histdf5c[,-1]






##Variable 6
set.seed(1)
Histdf6a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6a<-cbind(Histdf6a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf6a<-Histdf6a[,-1]




set.seed(1)
Histdf6b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6b<-cbind(Histdf6b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf6b<-Histdf6b[,-1]




set.seed(1)
Histdf6c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,6]<- halflivesmatters[,6]*(randomError)
  Histdf6c<-cbind(Histdf6c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf6c<-Histdf6c[,-1]






##Variable 7
set.seed(1)
Histdf7a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7a<-cbind(Histdf7a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf7a<-Histdf7a[,-1]




set.seed(1)
Histdf7b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7b<-cbind(Histdf7b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf7b<-Histdf7b[,-1]




set.seed(1)
Histdf7c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,7]<- halflivesmatters[,7]*(randomError)
  Histdf7c<-cbind(Histdf7c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf7c<-Histdf7c[,-1]






##Variable 8
set.seed(1)
Histdf8a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8a<-cbind(Histdf8a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf8a<-Histdf8a[,-1]




set.seed(1)
Histdf8b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8b<-cbind(Histdf8b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf8b<-Histdf8b[,-1]




set.seed(1)
Histdf8c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,8]<- halflivesmatters[,8]*(randomError)
  Histdf8c<-cbind(Histdf8c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf8c<-Histdf8c[,-1]






##Variable 9
set.seed(1)
Histdf9a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9a<-cbind(Histdf9a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf9a<-Histdf9a[,-1]




set.seed(1)
Histdf9b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9b<-cbind(Histdf9b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf9b<-Histdf9b[,-1]




set.seed(1)
Histdf9c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,9]<- halflivesmatters[,9]*(randomError)
  Histdf9c<-cbind(Histdf9c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf9c<-Histdf9c[,-1]






##Variable 10
set.seed(1)
Histdf10a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10a<-cbind(Histdf10a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf10a<-Histdf10a[,-1]




set.seed(1)
Histdf10b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10b<-cbind(Histdf10b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf10b<-Histdf10b[,-1]




set.seed(1)
Histdf10c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,10]<- halflivesmatters[,10]*(randomError)
  Histdf10c<-cbind(Histdf10c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf10c<-Histdf10c[,-1]






##Variable 11
set.seed(1)
Histdf11a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11a<-cbind(Histdf11a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf11a<-Histdf11a[,-1]




set.seed(1)
Histdf11b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11b<-cbind(Histdf11b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf11b<-Histdf11b[,-1]




set.seed(1)
Histdf11c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,11]<- halflivesmatters[,11]*(randomError)
  Histdf11c<-cbind(Histdf11c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf11c<-Histdf11c[,-1]






##Variable 12
set.seed(1)
Histdf12a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12a<-cbind(Histdf12a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf12a<-Histdf12a[,-1]




set.seed(1)
Histdf12b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12b<-cbind(Histdf12b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf12b<-Histdf12b[,-1]




set.seed(1)
Histdf12c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,12]<- halflivesmatters[,12]*(randomError)
  Histdf12c<-cbind(Histdf12c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf12c<-Histdf12c[,-1]






##Variable 13
set.seed(1)
Histdf13a<-data.frame(nrow=21)
for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13a<-cbind(Histdf13a,as.data.frame(finalCarbonContribution(Years=1990,halflives = halflivesmatters)))
}
Histdf13a<-Histdf13a[,-1]




set.seed(1)
Histdf13b<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13b<-cbind(Histdf13b,as.data.frame(finalCarbonContribution(Years=2000,halflives = halflivesmatters)))
}
Histdf13b<-Histdf13b[,-1]




set.seed(1)
Histdf13c<-data.frame(nrow=21)

for(i in 1:1000){
  halflivesmatters <- halfLives
  randomError<-rnorm(length(halflivesmatters[,1]),1,.2)
  halflivesmatters[,13]<- halflivesmatters[,13]*(randomError)
  Histdf13c<-cbind(Histdf13c,as.data.frame(finalCarbonContribution(Years=2010,halflives = halflivesmatters)))
}
Histdf13c<-Histdf13c[,-1]

#

##Variable 14

LastOne<-proc.time()
set.seed(1)
Histdf14a<-data.frame(nrow=21)
for(i in 1:1000){
  randomError<-rnorm(1,1,.2)
  Histdf14a<-cbind(Histdf14a,as.data.frame(finalCarbonContribution(Years=1990,paperHL = 2.53087281800454*(randomError))))
}
Histdf14a<-Histdf14a[,-1]
LastVariable<-proc.time() - LastOne


set.seed(1)
Histdf14b<-data.frame(nrow=21)
for(i in 1:1000){
  randomError<-rnorm(1,1,.2)
  Histdf14b<-cbind(Histdf14b,as.data.frame(finalCarbonContribution(Years=2000,paperHL = 2.53087281800454*(randomError))))
}
Histdf14b<-Histdf14b[,-1]




set.seed(1)
Histdf14c<-data.frame(nrow=21)
for(i in 1:1000){
  randomError<-rnorm(1,1,.2)
  Histdf14c<-cbind(Histdf14c,as.data.frame(finalCarbonContribution(Years=2010,paperHL = 2.53087281800454*(randomError))))
}
Histdf14c<-Histdf14c[,-1]
#
#








#timing to see how long it takes to run all 13 variables for each of the three years (1990, 2000, 2010)
AllOfThem<-proc.time() - ptmAll

OneTrial;LastOne;AllOfThem


#variance of each variable for each year put into a matrix.  Columns are the years, Rows are the variables
VarianceTable<-t(as.matrix(c(var(t(Histdfa)),var(t(Histdfb)),var(t(Histdfc)))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf2a)),var(t(Histdf2b)),var(t(Histdf2c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf3a)),var(t(Histdf3b)),var(t(Histdf3c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf4a)),var(t(Histdf4b)),var(t(Histdf4c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf5a)),var(t(Histdf5b)),var(t(Histdf5c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf6a)),var(t(Histdf6b)),var(t(Histdf6c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf7a)),var(t(Histdf7b)),var(t(Histdf7c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf8a)),var(t(Histdf8b)),var(t(Histdf8c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf9a)),var(t(Histdf9b)),var(t(Histdf9c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf10a)),var(t(Histdf10b)),var(t(Histdf10c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf11a)),var(t(Histdf11b)),var(t(Histdf11c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf12a)),var(t(Histdf12b)),var(t(Histdf12c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf13a)),var(t(Histdf13b)),var(t(Histdf13c))))))
VarianceTable<-rbind(VarianceTable,t(as.matrix(c(var(t(Histdf14a)),var(t(Histdf14b)),var(t(Histdf14c))))))

colnames(VarianceTable)<-c(1990,2000,2010)
rownames(VarianceTable)<-c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5",
                           "Variable 6","Variable 7","Variable 8","Variable 9","Variable 10",
                           "Variable 11","Variable 12","Variable 13","Variable 14")


#Minimum and Maximum amount of variance
#finding the minimum amount of variance for year 1990
Min_1990<-min(var(t(Histdfa)),
    var(t(Histdf2a)),
    var(t(Histdf3a)),
    var(t(Histdf4a)),
    var(t(Histdf5a)),
    var(t(Histdf6a)),
    var(t(Histdf7a)),
    var(t(Histdf8a)),
    var(t(Histdf9a)),
    var(t(Histdf10a)),
    var(t(Histdf11a)),
    var(t(Histdf12a)),
    var(t(Histdf13a)),
    var(t(Histdf14a))
    )

#finding the minimum amount of variance for year 2000
Min_2000<-min(var(t(Histdfb)),
    var(t(Histdf2b)),
    var(t(Histdf3b)),
    var(t(Histdf4b)),
    var(t(Histdf5b)),
    var(t(Histdf6b)),
    var(t(Histdf7b)),
    var(t(Histdf8b)),
    var(t(Histdf9b)),
    var(t(Histdf10b)),
    var(t(Histdf11b)),
    var(t(Histdf12b)),
    var(t(Histdf13b)),
    var(t(Histdf14b))
    )

#finding the minimum amount of variance for year 2010
Min_2010<-min(var(t(Histdfc)),
    var(t(Histdf2c)),
    var(t(Histdf3c)),
    var(t(Histdf4c)),
    var(t(Histdf5c)),
    var(t(Histdf6c)),
    var(t(Histdf7c)),
    var(t(Histdf8c)),
    var(t(Histdf9c)),
    var(t(Histdf10c)),
    var(t(Histdf11c)),
    var(t(Histdf12c)),
    var(t(Histdf13c)),
    var(t(Histdf14c))
    )
#




#finding the maximum amount of variance for year 1990
Max_1990<-max(var(t(Histdfa)),
    var(t(Histdf2a)),
    var(t(Histdf3a)),
    var(t(Histdf4a)),
    var(t(Histdf5a)),
    var(t(Histdf6a)),
    var(t(Histdf7a)),
    var(t(Histdf8a)),
    var(t(Histdf9a)),
    var(t(Histdf10a)),
    var(t(Histdf11a)),
    var(t(Histdf12a)),
    var(t(Histdf13a)),
    var(t(Histdf14a))
)

#finding the maximum amount of variance for year 2000
Max_2000<-max(var(t(Histdfb)),
    var(t(Histdf2b)),
    var(t(Histdf3b)),
    var(t(Histdf4b)),
    var(t(Histdf5b)),
    var(t(Histdf6b)),
    var(t(Histdf7b)),
    var(t(Histdf8b)),
    var(t(Histdf9b)),
    var(t(Histdf10b)),
    var(t(Histdf11b)),
    var(t(Histdf12b)),
    var(t(Histdf13b)),
    var(t(Histdf14b))
)

#finding the maximum amount of variance for year 2010
Max_2010<-max(var(t(Histdfc)),
    var(t(Histdf2c)),
    var(t(Histdf3c)),
    var(t(Histdf4c)),
    var(t(Histdf5c)),
    var(t(Histdf6c)),
    var(t(Histdf7c)),
    var(t(Histdf8c)),
    var(t(Histdf9c)),
    var(t(Histdf10c)),
    var(t(Histdf11c)),
    var(t(Histdf12c)),
    var(t(Histdf13c)),
    var(t(Histdf14c))
)

#
Min_1990;Min_2000;Min_2010
Max_1990;Max_2000;Max_2010

#Minimum Amount of Variance for 1990,2000,2010 are in Variable 7 for all three years
#no other Variables are close enough to Variable 7 to be a close second in minimum variance

#Maximum Amount of Variance for 1990,2000,2010 are in Variable 4 for all three years
#A close second for the maximum variance is Variable 2, 1, 1 respectively

#Finding the Minimum and Maximum values of the finalCarbonContribution from all simulates manipulating the variables
HistMIN<-min(Histdfa,
    Histdf2a,
    Histdf3a,
    Histdf4a,
    Histdf5a,
    Histdf6a,
    Histdf7a,
    Histdf8a,
    Histdf9a,
    Histdf10a,
    Histdf11a,
    Histdf12a,
    Histdf13a,
    Histdfb,
    Histdf2b,
    Histdf3b,
    Histdf4b,
    Histdf5b,
    Histdf6b,
    Histdf7b,
    Histdf8b,
    Histdf9b,
    Histdf10b,
    Histdf11b,
    Histdf12b,
    Histdf13b,
    Histdfc,
    Histdf2c,
    Histdf3c,
    Histdf4c,
    Histdf5c,
    Histdf6c,
    Histdf7c,
    Histdf8c,
    Histdf9c,
    Histdf10c,
    Histdf11c,
    Histdf12c,
    Histdf13c,
    Histdf14a,
    Histdf14b,
    Histdf14c
    )


HistMAX<-max(Histdfa,
    Histdf2a,
    Histdf3a,
    Histdf4a,
    Histdf5a,
    Histdf6a,
    Histdf7a,
    Histdf8a,
    Histdf9a,
    Histdf10a,
    Histdf11a,
    Histdf12a,
    Histdf13a,
    Histdfb,
    Histdf2b,
    Histdf3b,
    Histdf4b,
    Histdf5b,
    Histdf6b,
    Histdf7b,
    Histdf8b,
    Histdf9b,
    Histdf10b,
    Histdf11b,
    Histdf12b,
    Histdf13b,
    Histdfc,
    Histdf2c,
    Histdf3c,
    Histdf4c,
    Histdf5c,
    Histdf6c,
    Histdf7c,
    Histdf8c,
    Histdf9c,
    Histdf10c,
    Histdf11c,
    Histdf12c,
    Histdf13c,
    Histdf14a,
    Histdf14b,
    Histdf14c
)
#
HistMIN;HistMAX

#Density plot as a visual representation of the variation between variance distribution between all 14 variables
plot(density(as.numeric(Histdf6b)), ylim=c(0,.029), xlim=c(-113500,-112500), main = "Densites of Halflives", xlab = "Carbon Contribution")

lines(density(as.numeric(Histdfb)), col="black")
lines(density(as.numeric(Histdf2b)), col="red")
lines(density(as.numeric(Histdf3b)), col="orange")
lines(density(as.numeric(Histdf4b)), col="yellow")
lines(density(as.numeric(Histdf5b)), col="green")
lines(density(as.numeric(Histdf6b)), col="blue")
lines(density(as.numeric(Histdf7b)), col="purple")
lines(density(as.numeric(Histdf8b)), col="pink")
lines(density(as.numeric(Histdf9b)), col="brown")
lines(density(as.numeric(Histdf10b)), col="chocolate3")
lines(density(as.numeric(Histdf11b)), col="skyblue")
lines(density(as.numeric(Histdf12b)), col="tan2")
lines(density(as.numeric(Histdf13b)), col="seagreen")
lines(density(as.numeric(Histdf14b)), col="rosybrown")
legend("topright", legend=c("Variable 1","Variable 2","Variable 3","Variable 4","Variable 5",
                            "Variable 6","Variable 7","Variable 8","Variable 9","Variable 10",
                            "Variable 11","Variable 12","Variable 13", "Variable 14"), 
       col = c("black","red", "orange", "yellow", "green", "blue", "purple", "pink",
               "brown", "chocolate3", "skyblue", "tan2", "seagreen", "rosybrown"),
       cex=.8, pch=8, pt.cex = 1)

       
#
#This is so that the density plot can be remade without needing to rerun the for loops
setwd("~/USFS/R-Scripts/Hannah")
write.csv(Histdfa,file = "Histdfa.csv", row.names = F)
write.csv(Histdfb,file = "Histdfb.csv", row.names = F)
write.csv(Histdfc,file = "Histdfc.csv", row.names = F)

write.csv(Histdf2a,file = "Histdf2a.csv", row.names = F)
write.csv(Histdf2b,file = "Histdf2b.csv", row.names = F)
write.csv(Histdf2c,file = "Histdf2c.csv", row.names = F)

write.csv(Histdf3a,file = "Histdf3a.csv", row.names = F)
write.csv(Histdf3b,file = "Histdf3b.csv", row.names = F)
write.csv(Histdf3c,file = "Histdf3c.csv", row.names = F)

write.csv(Histdf4a,file = "Histdf4a.csv", row.names = F)
write.csv(Histdf4b,file = "Histdf4b.csv", row.names = F)
write.csv(Histdf4c,file = "Histdf4c.csv", row.names = F)

write.csv(Histdf5a,file = "Histdf5a.csv", row.names = F)
write.csv(Histdf5b,file = "Histdf5b.csv", row.names = F)
write.csv(Histdf5c,file = "Histdf5c.csv", row.names = F)

write.csv(Histdf6a,file = "Histdf6a.csv", row.names = F)
write.csv(Histdf6b,file = "Histdf6b.csv", row.names = F)
write.csv(Histdf6c,file = "Histdf6c.csv", row.names = F)

write.csv(Histdf7a,file = "Histdf7a.csv", row.names = F)
write.csv(Histdf7b,file = "Histdf7b.csv", row.names = F)
write.csv(Histdf7c,file = "Histdf7c.csv", row.names = F)

write.csv(Histdf8a,file = "Histdf8a.csv", row.names = F)
write.csv(Histdf8b,file = "Histdf8b.csv", row.names = F)
write.csv(Histdf8c,file = "Histdf8c.csv", row.names = F)

write.csv(Histdf9a,file = "Histdf9a.csv", row.names = F)
write.csv(Histdf9b,file = "Histdf9b.csv", row.names = F)
write.csv(Histdf9c,file = "Histdf9c.csv", row.names = F)

write.csv(Histdf10a,file = "Histdf10a.csv", row.names = F)
write.csv(Histdf10b,file = "Histdf10b.csv", row.names = F)
write.csv(Histdf10c,file = "Histdf10c.csv", row.names = F)

write.csv(Histdf11a,file = "Histdf11a.csv", row.names = F)
write.csv(Histdf11b,file = "Histdf11b.csv", row.names = F)
write.csv(Histdf11c,file = "Histdf11c.csv", row.names = F)

write.csv(Histdf12a,file = "Histdf12a.csv", row.names = F)
write.csv(Histdf12b,file = "Histdf12b.csv", row.names = F)
write.csv(Histdf12c,file = "Histdf12c.csv", row.names = F)

write.csv(Histdf13a,file = "Histdf13a.csv", row.names = F)
write.csv(Histdf13b,file = "Histdf13b.csv", row.names = F)
write.csv(Histdf13c,file = "Histdf13c.csv", row.names = F)

write.csv(Histdf14a,file = "Histdf14a.csv", row.names = F)
write.csv(Histdf14b,file = "Histdf14b.csv", row.names = F)
write.csv(Histdf14c,file = "Histdf14c.csv", row.names = F)















#####Creating histogram image#####
setwd("~/WOODCARB3RB/R/SensitivityAnalysis")
Histdf6b <- read.csv("Histdf6b.csv")
Histdf10a <- read.csv("Histdf10a.csv")
Histdf13a <- read.csv("Histdf13a.csv")
Histdf14a <- read.csv("Histdf14a.csv")


png(file = "Example histogram.png")
plot(density(as.numeric(Histdf6b)), col = "blue", main = NA, ylim = c(0, .029), xlim = c(-113500, -112500), xlab = "", ylab = "", cex.lab = 0.7, cex.axis = 0.7)
title(xlab = "Carbon Contribution", line = 2, cex.lab = 0.7)
title(ylab = "Density", line = 2, cex.lab = 0.7)

dev.off()

png(file = "Example histogram2.png")
plot(density(as.numeric(Histdf10a)), col = "red", main = NA, xlab = "", ylab = "", cex.lab = 0.7, cex.axis = 0.7)
title(xlab = "Carbon Contribution", line = 2, cex.lab = 0.7)
title(ylab = "Density", line = 2, cex.lab = 0.7)

dev.off()

png(file = "Example histogram3.png")
plot(density(as.numeric(Histdf13a)), col = "orange", main = NA, xlab = "", ylab = "", cex.lab = 0.7, cex.axis = 0.7)
title(xlab = "Carbon Contribution", line = 2, cex.lab = 0.7)
title(ylab = "Density", line = 2, cex.lab = 0.7)

dev.off()

png(file = "Example histogram4.png")
plot(density(as.numeric(Histdf14a)), col = "pink", main = NA, xlab = "", ylab = "", cex.lab = 0.7, cex.axis = 0.7)
title(xlab = "Carbon Contribution", line = 2, cex.lab = 0.7)
title(ylab = "Density", line = 2, cex.lab = 0.7)

dev.off()


png(file = "Example histogramCombine.png")
plot(density(as.numeric(Histdf13a)), col = "orange", main = NA, xlab = "", ylab = "", cex.lab = 0.7, cex.axis = 0.7)
lines(density(as.numeric(Histdf6b)), col="blue")
lines(density(as.numeric(Histdf14a)), col="pink")
lines(density(as.numeric(Histdf10a)), col="red")
title(xlab = "Carbon Contribution", line = 2, cex.lab = 0.7)
title(ylab = "Density", line = 2, cex.lab = 0.7)

dev.off()



#