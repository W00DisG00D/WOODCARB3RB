##how to clone (with github, may need a walkthrough)
##we can do it :D
#fix years (use max instead of manually fixing)
#plot more than one thing on the same graph
#use plot() not plot = TRUE for multiple lines on same graph

#example of sensitivity histogram
#code for snesitivity
#maybe create function for sasid thing
#collect additional scripts that should be here.
#just stay here rather than through slides
#have the code for loops and histogram stuff
#put in all csv files for analysis




##package tutorial
install.packages("devtools")
devtools::install_github("benjones2/WOODCARB3RB", build_vignettes=TRUE) #B is arbitrary 
library(WOODCARB3R)


#access to vignettes
??WOODCARB3R

#list functions and datasets
ls("package:WOODCARB3R")

#data for intermediate calculations
?returnData
returnData(years = 1950:2015)

##finalVariables and finalCarbonContribution=main functions
#to see code behind the scenes, just type the function
finalVariables

finalVariables(2001) #returns final variables for the year 2001
?finalVariables #to see different inputs for function


#all values
finalCarbonContribution(2001)
#plotting final carbon contribution for all years
finalCarbonContribution(plot = TRUE)

#selecting a subset of years
finalCarbonContribution(2001:2007, plot = TRUE)
?finalCarbonContribution
#defaults to atmospheric flow approach
#different decay distributions, defaults to Exponential..change to k=2
finalCarbonContribution(approach = "Production", decaydistribution ="K=2")
finalCarbonContribution(approach = "Production", decaydistribution ="K=2", plot=TRUE)

ls("package:WOODCARB3R")