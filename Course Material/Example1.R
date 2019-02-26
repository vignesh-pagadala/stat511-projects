#R Example 1
#Create a data vector using c()
# = and <- are “assignment operators”
y = c(1, 2, 3, 4)
y
mean(y)
#R is case sensitive!
Y <- 20
Y
y
#Loading a "built-in" data set
#Use str() or View() to look at the data
data(chickwts)
str(chickwts)
View(chickwts)
summary(chickwts)
#Summary Statistics
#Approaches to referencing a single column in a data.frame
#1 $ Approach
mean(chickwts$weight)
#2 with() function
with(mean(weight), data = chickwts)
#3 data = option (not available with all functions!)
aggregate(weight ~ feed, data = chickwts, FUN = mean)
#4 attach(): Use with caution!
mean(weight)
attach(chickwts)
mean(weight)
detach(chickwts)
#Summary Graphics
boxplot(weight ~ feed, data = chickwts, col = "grey")