#calling libraries####
install.packages("gplots")
install.packages("prabclus")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")
install.packages("RColorBrewer")
library(arules)
library(arulesViz)
library(prabclus)
library(gplots)
library(plyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)


#reading the dataset####
tread.csv
electronidex<- read.transactions(file.choose(),format = "basket",
                                 sep = ",",cols = NULL,rm.duplicates = FALSE)
electronidex
#inspecting the data set####
inspect(electronidex)
electronidex
length(electronidex)
size(electronidex)
itemLabels(electronidex)
image(sample(electronidex,10))

#plot the top 20 ####
itemFrequencyPlot(electronidex,type="absolute",col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")

itemFrequencyPlot(electronidex,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), 
                  main="Relative Item Frequency Plot")




#finding association rules####
rules<- apriori(electronidex, parameter = list(minlen=1,supp=0.001,conf= 0.8))
rules
inspect(rules)
summary(rules)
plot(rules,method = "grouped")

#find relations with imac and other items####
imac.rules<- apriori(electronidex, parameter = list(minlen=2,supp=0.001,conf= 0.08),
                     appearance =  list(lhs="iMac"))
View(inspect(imac.rules))
plot(imac.rules,method = "grouped")
plot(imac.rules,method = "graph")

#find relations with HP Laptop and other items####
hplaptop.rules<- apriori(electronidex, parameter = list(minlen = 2,supp=0.001,conf= 0.08),
                         appearance =  list(lhs="HP Laptop" ))
View(inspect(hplaptop.rules))

#plot the findings####
subRules<- rules[quality(rules)$confidence > 0.8]
subRules
plot(subRules)
plot(rules,method="grouped")
plot(rules, method= "graph")

