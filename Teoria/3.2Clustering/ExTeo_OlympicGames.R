## The Olympic Games Dataset

library(FactoMineR)
library(car)
library(missMDA)
data(JO)
dim(JO)
summary(JO)
# JO event and medals by country: can each country be considered to have the same athletic profile? Can each event be considered to have the same geographical profile ?
JO
# Margin profiles have to be calculated otherwise countries with the most medals have a greater influence on the analysis
res.ca <- CA(JO)
res.ca

# Independence: X2 test, careful interpretation since a lot of zeros are present (consider 80% of the theoretical sample size to be greater than 5, and the others to be greater than 1). Anyway, pvalue is so small, independence can be rejected
chisq.test(JO)
# How many components to choose?
res.ca$eig
sum(res.ca$eig[,1])
mean(res.ca$eig[,1])
barplot(res.ca$eig[,1],main="Eigenvalues",names.arg=paste("dim",1:nrow(res.ca$eig)))
# The first 2 dimensions express 24.4% of the total inertia (more dimensions have to be considered)
plot(res.ca)
plot(res.ca,axes=3:4)
plot(res.ca,invisible="row",axes=1:2)
plot(res.ca,invisible="col")
# Interpretation of Results: projection of rows. Long distance races are separated from the other events on first dimension
# Countries with negative first component won many medals in endurance events (African countries). The most extreme elements are not necessarily those which contributed the most to the construction of the dimensions. Ethiopia, Kenya, and Morocco account for 65% of the construction of the first dimension
res.ca$col$contrib[rev(order(res.ca$col$contrib[,1])),1]

# The second dimension separates sprinting events from the discus, the hammer, and walking (20 km and 50 km)

res.ca$col$contrib[rev(order(res.ca$col$contrib[,2])),2]

#Dimensions 3 and 4 also separate the discus and the hammer from the walking events (20 km and 50 km)
plot(res.ca,axes=3:4)


#row and column margins 
res.ca$call$marge.row
res.ca$call$marge.row*sum(JO)
res.ca$call$marge.col
res.ca$call$marge.col[rev(order(res.ca$call$marge.col))]*sum(JO)
sum(JO)

