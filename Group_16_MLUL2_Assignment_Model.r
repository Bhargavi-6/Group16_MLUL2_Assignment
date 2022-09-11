
## Install and Import Required Packages

requiredPackages = c('Matrix','arules','arulesViz','recommenderlab','reshape2','RCurl')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

## Load Dataset

#setwd("C:/Users/ADMIN/Desktop/data science/ISB/3_Term3/MLUSL2/Group Assignment")
terror_df <-read.csv("https://media.githubusercontent.com/media/Bhargavi-6/Group16_MLUL2_Assignment/main/terror_model_df.csv")
terror <- na.omit(terror_df)
head(terror_df)
dim(terror_df)


## Rename Group or Shorten lengthy group names

terror_df$Group[terror_df$Group == "Kurdistan Workers' Party (PKK)"] <- "PKK"
terror_df$Group[terror_df$Group == "Al-Qaida in the Arabian Peninsula (AQAP)"] <- "AQAP"
terror_df$Group[terror_df$Group == "Houthi extremists (Ansar Allah)"] <- "Houthi_Extrm"
terror_df$Group[terror_df$Group == "Tehrik-i-Taliban Pakistan (TTP)"] <- "TTP"
terror_df$Group[terror_df$Group == "Al-Nusrah Front"] <- "Al-Nusrah"
terror_df$Group[terror_df$Group == "Islamic State of Iraq and the Levant (ISIL)"] <-"ISIL"
terror_df$Group[terror_df$Group == "Donetsk People's Republic"] <- "Donetsk_PR"

## Create 2 number of datasets
## Dataset #01 : Rules Mining
## Dataset #02 : Recommender System

terror_df_rules <- terror_df[1:6]
head(terror_df_rules)

terror_df_recommender <- subset(terror_df,terror_df$Country == "India")
str(terror_df_recommender)
head(terror_df_recommender)

## Convert attributes to factor

terror_df_rules[] <- lapply(terror_df_rules, factor)
str(terror_df_rules)


## Apriori model

## Islamic State (ISIL)

## Set rhs as Terror Group

params <- list(support = 0.001, confidence = 0.3, minlen = 2)
group_ISIL <- list(rhs='Group=ISIL', default="lhs")

rules <- apriori(data = terror_df_rules, parameter= params, appearance = group_ISIL)

## Remove redundant rules if any

rules <- rules[!is.redundant(rules)] 

## Top 5 patterns (ISIL) based on confidence

subrules <- head(sort(rules, by="confidence"), 5)
inspect(head(subrules, 5))

## Network Graph

plot(rules, method="graph", control=list(type="items"))

## Scatter Graph

plot(rules, method="scatter", control=list(type="items"))


## Taliban

## Set rhs as Terror Group

group_taliban <- list(rhs='Group=Taliban', default="lhs")
params <- list(support = 0.001, confidence = 0.3, minlen = 2)

rules <- apriori(data = terror_df_rules, parameter= params, appearance = group_taliban)

## Remove redundant rules if any

rules <- rules[!is.redundant(rules)] 

#### Top 5 patterns (Taliban) based on confidence

subrules <- head(sort(rules, by="confidence"), 5)
inspect(head(subrules, 5))

## Network Graph

plot(rules, method="graph", control=list(type="items"))

## Scatter Plot

plot(rules, method="scatter", control=list(type="items"))

## Boko Haram

## Set rhs as Terror Group

group_boko_haram <- list(rhs='Group=Boko Haram', default="lhs")
params <- list(support = 0.001, confidence = 0.3, minlen = 2)

rules <- apriori(data = terror_df_rules, parameter= params, appearance = group_boko_haram)
## Remove redundant rules if any

rules <- rules[!is.redundant(rules)] 

## Top 5 patterns (Boko Haram) based on confidence

subrules <- head(sort(rules, by="confidence"), 5)
inspect(head(subrules, 5))

## Network Graph

plot(rules, method="graph", control=list(type="items"))

## Scatter Graph

plot(rules, method="scatter", control=list(type="items"))


## Recommendation System Across India

## Recommendation System of Terrorist Group and City

## covert to matrix format

kill_matrix <- as.matrix(acast(terror_df_recommender, Group~city, value.var="nkill", fun.aggregate = mean))

dim(kill_matrix)

## recommendarlab realRatingMatrix format

R <- as(kill_matrix, "realRatingMatrix")

## User-based collaborative filtering

rec1 = Recommender(R, method="UBCF") 

## Item-based collaborative filtering

rec2 = Recommender(R, method="IBCF") 
rec3 = Recommender(R, method="SVD")
rec4 = Recommender(R, method="POPULAR")

## create n recommendations for a Terrorist group
group_name = "Sikh Extremists" 
result <- subset(terror_df_recommender, terror_df_recommender$Group==group_name)
print(result)
print(paste("The most at risk city from this Terrorist group : ",group_name))
prediction <- predict(rec1, R[group_name], n=3) 
as(prediction, "list")
prediction <- predict(rec2, R[group_name], n=3) 
as(prediction, "list")
prediction <- predict(rec3, R[group_name], n=3) 
as(prediction, "list")
prediction <- predict(rec4, R[group_name], n=3) 
as(prediction, "list")


## Recommendation System of City and Attack Type

## covert to matrix format

kill_matrix <- as.matrix(acast(terror_df_recommender, city~AttackType, value.var="nkill", fun.aggregate = mean))

dim(kill_matrix)

## recommendarlab realRatingMatrix format

R <- as(kill_matrix, "realRatingMatrix")

## User-based collaborative filtering

rec1 = Recommender(R, method="UBCF") 

## Item-based collaborative filtering

rec2 = Recommender(R, method="IBCF") 
rec3 = Recommender(R, method="SVD")
rec4 = Recommender(R, method="POPULAR")

## create n recommendations for a Terrorist group
city = "Bombay" 
result <- subset(terror_df_recommender, terror_df_recommender$city==city)
print(result)
print(paste("The attack type anticipated in the city : ",city))
prediction <- predict(rec1, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec2, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec3, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec4, R[city], n=3) 
as(prediction, "list")


## Recommendation System of City and Target Type

## covert to matrix format

kill_matrix <- as.matrix(acast(terror_df_recommender, city~Target_type, value.var="nkill", fun.aggregate = mean))

dim(kill_matrix)

## recommendarlab realRatingMatrix format

R <- as(kill_matrix, "realRatingMatrix")

## User-based collaborative filtering

rec1 = Recommender(R, method="UBCF") 

## Item-based collaborative filtering

rec2 = Recommender(R, method="IBCF") 
rec3 = Recommender(R, method="SVD")
rec4 = Recommender(R, method="POPULAR")

## create n recommendations for a Terrorist group
city = "Bombay" 
result <- subset(terror_df_recommender, terror_df_recommender$city==city)
print(result)
print(paste("The target type anticipated in the city : ",city))
prediction <- predict(rec1, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec2, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec3, R[city], n=3) 
as(prediction, "list")
prediction <- predict(rec4, R[city], n=3) 
as(prediction, "list")


