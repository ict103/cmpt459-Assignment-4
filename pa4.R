##############################################
# CMPT 459 Programming Assignment 4
# Ivy Tse
##############################################

#***** QUESTION 1 *****#
library(arules)
data(Groceries)
items_in_trans <- summary(Groceries)@lengths
plot(items_in_trans, xlab="Number of Items per Transaction", 
ylab="Total Number of Transactions",, main="Histogram")

#***** QUESTION 2 *****#
frequent_itemsets <- 
apriori(Groceries, parameter=list(supp=0.001,target="frequent"))
frequent_itemsets # frequent itemsets = 13492

closed_fi <- apriori(Groceries, parameter = list(supp=0.001, target="closed"))
closed_fi # closed frequent itemsets = 13464

maximal_fi <- apriori(Groceries, parameter = list(supp=0.001, target="maximal"))
maximal_fi # maximal frequent itemsets = 7794

#***** QUESTION 3 *****#
fi1 <- apriori(Groceries, parameter = list(supp=0.01, target="frequent"))
fi1 # frequent itemsets = 333

cfi1 <- apriori(Groceries, parameter = list(supp=0.01, target="closed"))
cfi1 # closed frequent itemsets = 333

mfi1 <- apriori(Groceries, parameter = list(supp=0.01, target="maximal"))
mfi1 # maximal frequent itemsets = 243

# sort the fi1, cfi1, and mfi1 by support in decreasing order, then display
# top 10 results
fi1_sorted <- sort(fi1)
inspect(fi1_sorted[1:10,])

cfi1_sorted <- sort(cfi1)
inspect(cfi1_sorted[1:10,])

mfi1_sorted <- sort(mfi1)
inspect(mfi1_sorted[1:10,])

#***** QUESTION 4 *****#
Groceries # Transactions = 9835, Items = 169

#***** QUESTION 5 *****#
fi90 <- apriori(Groceries, parameter = list(supp=0.01, conf=0.9 ,
target="rules"))
fi90 # set of 0 rules

fi52 <- apriori(Groceries, parameter = list(supp=0.01, conf=0.52 ,
target="rules"))
fi52 # set of 9 rules

fi51 <- apriori(Groceries, parameter = list(supp=0.01, conf=0.51 ,
target="rules"))
fi51 # set of 12 rules

# The minimum confidence need to be reduces to 0.51 in order for there to be
# more than 10 rules. At a minimum confidence of 0.52, there are only 9 rules.
# The minumum confidence needs to be lowered to almost 0.40 for there to be 
# more than 10 rules.

#***** QUESTION 6 *****#
fi50 <- apriori(Groceries, parameter = list(supp=0.01, conf=0.50 ,
target="rules"))

whole_milk <- subset(fi50, subset = rhs %pin% "whole milk")
 
plot(whole_milk@quality$support, whole_milk@quality$confidence, 
xlab="Support", ylab="Confidence", main="RHS = whole milk")

#***** QUESTION 7 *****#
# Rules with the top 3 highest lifts

whole_milk_sorted <- sort(whole_milk, by = "lift")
inspect(whole_milk_sorted[1:3])
