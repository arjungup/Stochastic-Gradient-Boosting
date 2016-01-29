#### Working with GBM using Quantile Regression ###

### Load Some Data ###
load("Paper4Data.rdata")
working<-Paper4Data
working<-na.omit(working)
summary(working)
###############
test<-lm(working$avrain~working$Year+working$Jan+working$Feb+working$Mar+working$Apr+working$May+working$Jun+working$Jul+working$Aug+working$Sep+working$Oct+working$Nov+working$Dec)
summary(test)
hist(working$Jan, breaks=50)
summary(working$Jan)
hist(working$Feb, breaks=50)
summary(working$Feb)
hist(working$Mar, breaks=50)
summary(working$Mar)
hist(working$Apr, breaks=50)
summary(working$Apr)
hist(working$May, breaks=50)
summary(working$May)
hist(working$Jun, breaks=50)
summary(working$Jun)
hist(working$Jul, breaks=50)
summary(working$Jul)
hist(working$Aug, breaks=50)
summary(working$Aug)
hist(working$Sep, breaks=50)
summary(working$Sep)
hist(working$Oct, breaks=50)
summary(working$Oct)
hist(working$Nov, breaks=50)
summary(working$Nov)
hist(working$Dec, breaks=50)
summary(working$Dec)

hist(working$avrain, breaks=50)
hist(working$year)
cor(working, use="all.obs", method= "pearson")
install.packages("corrplot")

hist(working$avrain, breaks=50)

### Get GBM ###
install.packages("gbm")
library(help=gbm) # See what is in the package
library(gbm)
help(gbm) 

### Run GBM ###

## Fit boosted quantile regression in which the quantile allows for
## asymmetric costs. For example, fitting alpha=.75 means that cost 
## ratio is .75/.25 = 3 to 1. It is 3 times worse to underestimate
## than overestimate. Positives deviations are weighted as 3 times worse.
## You are, in effect, fiting the median but with 3 to 1 asymmetric loss.

# Print the best number of trees
#justify alpha, and justify depth of interaction effects
#each tree will find intereaction effects if they're big
#can specify what interaction variables if you want
#may say look I'm not a climate scientist no one knows about this shit anyway, so this is just a first exploratory pass through the data
#triple interaction effect is product of 3 variables OR you split on x, then split on y (withing x), then split on z (within y)
#can keep trees small with up min bucket size and/or amount of depth
#just want a small tree vs. small number of interactions so we don't have 
#bucket size is hack for when you don't really know how interaction depth should be
#cv fold just affects statistic you use to evaluate the tree--it computes some measure of fit, basically mse. Each time you pass through
#cv fold--how many slices of data before the cv. Think about how many observations you have
#trees are sample size dependent--up n up complexity of tree
#unless you have real test data (and you could, we do allow for it) you have to 
#cv rebuilds tree for each fold
#we're minimizing deviance still, cv helps you determine number of passes through data/when to stop (when it stops decreasing)
#cv n=100, takes 80, 
#average from 5 folds, then go through each iteration
#small step lets the weak predictors play
#unless you're doing a whole lot of fiddling there's not a huge concern about over-fitting
#cv is not part of algortihm to do the fitting, it's after the fact. 
#vertical axis has the loss function, whatever the loss is.

#just trying it with regular cost ratios
out1<-gbm(working$avrain~working$Year+working$Jan+working$Feb+working$Mar+working$Apr+
working$May+working$Jun+working$Jul+working$Aug+working$Sep+working$Oct+working$Nov+
  working$Dec,data=working,distribution=list(name="quantile",alpha=.25),interaction.depth=3,
          shrinkage=.001,bag.fraction=.5,n.cores=2,n.trees=10000,cv.folds=3)
best.iter <- gbm.perf(out1,method="cv") # Find the number of trees needed
print(best.iter) 

plot(out1$fit,working$avrain)

lm1<-lm(working$avrain~out1$fit) # Least Squares fit
summary(lm1) # Use residual distribution and RMSE as fit measure
abline(lm1) # Linear overlay --- Maybe tune or change profile.
library(mgcv) # GAM overlay --- Maybe more insight?
temp1<-data.frame(working$avrain,out1$fit) # New data frame
summary(temp1)
gam1<-gam(working$avrain~s(out1$fit)) # GAM in mgcv
summary(gam1)
plot(gam1,residuals=T,rug=T,se=T,cex=5)
####### linear fit is very very good, strange.
out2<-gbm(working$avrain~working$Year+working$Jan+working$Feb+working$Mar+working$Apr+
            working$May+working$Jun+working$Jul+working$Aug+working$Sep+working$Oct+working$Nov+
            working$Dec,data=working,distribution=list(name="quantile",alpha=.25),interaction.depth=3,
          shrinkage=.0001,bag.fraction=.5,n.cores=2,n.trees=10000,cv.folds=3)
best.iter <- gbm.perf(out2,method="cv") # Find the number of trees needed
print(best.iter) 

plot(out2$fit,working$avrain)

lm2<-lm(working$avrain~out2$fit) # Least Squares fit
summary(lm2) # Use residual distribution and RMSE as fit measure
abline(lm2)
residuals=working$avrain-out2$fit
summary(residuals)
####with a shrinkage of .001 R^2 of .39, with I believe 9:1 cost ratio of underestimates to over
summary(out2,n.trees=best.iter) # Scaled Reduction in Loss Function

plot(out2,1,best.iter) # Partial Plot # Standard partial plot
pp<-plot(out2,1,best.iter,return.grid=T) # Save standard partial plot output
scatter.smooth(pp$Age,pp$y,span=1/2) # Make your own nicer plot
plot(out1,2,best.iter)
plot(out1,3,best.iter) # Partial Plot # Standard partial plot
plot(out1,4,best.iter) # Partial Plot # Standard partial plot
plot(out1,5,best.iter)
plot(out1,6,best.iter)
plot(out1,7,best.iter)
plot(out1,8,best.iter)
plot(out1,9,best.iter)
plot(out1,10,best.iter)
plot(out1,11,best.iter)
plot(out1,12,best.iter)
plot(out1,13,best.iter)



