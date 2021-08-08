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



# -----Social_Network_Ads-------------- -----------------------------------


setwd("C:/Users/Dell/Desktop/practice task")
Social_Network_Ads = read.csv('Social_Network_Ads.csv',header = T)

Social_Network_Ads$rgup = (1:400)

Social_train = subset(Social_Network_Ads,Social_Network_Ads$rgup > 50)
social_test = subset(Social_Network_Ads,Social_Network_Ads$rgup <= 50)

social_ln_formula = as.formula("Age~EstimatedSalary+Purchased")
social_ln_model = lm(ln_formula,data = Social_train)

social_logic_formla = as.formula("Age~EstimatedSalary+Purchased")
social_logic_model = glm(logic_formla,data = Social_train)

social_kmeans = kmeans(Social_train$EstimatedSalary,centers = 2)
library("e1071")
social_naive_formla = as.formula("Age~EstimatedSalary+Purchased")
social_naive_model = naiveBayes(social_naive_formla,data = Social_train)










# Extra work --------------------------------------------------------------

library(ggplot2)
ggplot(Fifty_Startups) + geom_line(aes(x = State,y = Profit),fill ="pink")

set = setdiff(colnames(Fifty_Startups),list("State"))
formula = as.formula(paste("Profit + RnDSpend",paste(set,collapse = '+'),sep = '~'))
model = lm(formula,data =  Fifty_Startups)


