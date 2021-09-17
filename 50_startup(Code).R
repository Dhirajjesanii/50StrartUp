# 50 Startuup -------------------------------------------------------------

#Q1  Load/Import dataset

setwd("D:/MCA SEM1/DATA SCIENCE USING R/practice task")
Fifty_Startups = read.csv('50_Startups.csv',header = T)


#Q2  Create column called rgrup

Fifty_Startups$rgup = c(1:dim(Fifty_Startups)[1])


#Q3 Split data into test(40) & train(60)

Train = subset(Fifty_Startups,Fifty_Startups$rgup > 20)
test = subset(Fifty_Startups,Fifty_Startups$rgup <= 20)


#Q4 Apply lm method on particular columns

ln_formula = as.formula("Profit~MarketingSpend+State+Administration")
ln_model = lm(ln_formula,data = Train)


#Q5 Apply glm method on particular columns

logic_formla = as.formula("Profit~MarketingSpend+State+Administration")
logic_model = glm(logic_formla,data = Train)


#Q6 Apply K-means on profit column

kmeans = kmeans(Train$Profit,centers = 2 )


#Q7 Apply naive bayes on particular columns

library("e1071")
naive_formla = as.formula("Profit~MarketingSpend+State+Administration")
naive_model = naiveBayes(naive_formla,data = Train)






