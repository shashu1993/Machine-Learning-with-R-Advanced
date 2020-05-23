RNGversion(vstr = 3.6)

# Section 1: Market basket analysis

#install.packages('arules')
#install.packages('arulesViz')
library(arules)
library(arulesViz)


data(Groceries)
class(Groceries)
?Groceries
inspect(head(Groceries))

#q1
dim(Groceries)[1]
#a1: 9835

#q2
dim(Groceries)[2]
as(Groceries,'matrix')[1,]
summary(Groceries)

itemFrequencyPlot(Groceries, support = 0.0, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue2", las = 1, topN=5,
                  xlab = paste("Proportion of Market Baskets Containing Item"))

#a2: whole milk, other vegetables, rolls/buns, soda, yogurt

#q3
rules_all = apriori(Groceries,parameter=list(support=0.01,confidence=0.01))
summary(rules_all)
#a3: 610

#q4
rules_all2 = apriori(Groceries,parameter=list(support=0.001,confidence=0.001))
summary(rules_all2)
#a3: 41100

#q5
x = inspect(rules_all)
x = x[x$count!=0,]
x[order(x$lift,x$support, decreasing = T),]
#a5: 3.372304

#q6
rules_all3 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01, maxlen = 2))
summary(rules_all3)
inspect(rules_all3)
#a6: 426


#q7

rules_1 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='root vegetables', rhs ='beef'))
inspect(rules_1)
#confidence = 0.15951493
rules_2 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='whipped/sour cream', rhs ='curd'))
inspect(rules_2)
#confidence = 0.14609929
rules_3 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='other vegetables', rhs ='whole milk'))
inspect(rules_3)
#confidence = 0.3867578
rules_4 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='butter', rhs ='whole milk'))
inspect(rules_4)
#confidence = 0.4972477
rules_5 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='sugar', rhs ='whole milk'))
inspect(rules_5)
#confidence = 0.4444444
#a7: butter -> whole milk

#q8
rules_6 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='soda', rhs ='whole milk'))
inspect(rules_6)
#a8: support = 0.04006101

#q9
rules_7 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='soda'))
x = inspect(rules_7)
x = x[x$count!=0,]
x[order(x$lift,x$support, decreasing = T),]
#a9: curd

#q10
rule_yogurt = apriori(Groceries,parameter=list(support=0.01,confidence=0.01),appearance = list(lhs='yogurt'))
inspect(rule_yogurt)
#highest confidence

#tropical fruit =   0.2099
#soda =  0.196
#whipped/sour cream =  0.148
#whole milk =   0.4
#frozen dessert = - 

#a10: whole milk

