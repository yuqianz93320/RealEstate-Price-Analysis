#import the stats packege for the ananlysis, the ggbiplot package for the visualization
#input the BK.csv
BK <- read.csv("~/Downloads/nyc_pluto_16v1/BK.csv", header=TRUE)
View(BK)

# Data preparation
Var<-c("Block","LotArea","BldgArea","NumBldgs","UnitsTotal","BldgDepth","BldgFront","NumFloors","AssessTot")
BK1<-BK[Var]
Sample.BK1<-BK1[sample(nrow(BK1),6000), ]# Randomly choose 6000 sample values
Sample.BK1[Sample.BK1==0]<-NA # Replace the 0 value into NA
str(Sample.BK1)# check the cleaning process
colSums(is.na(Sample.BK1)) # count the missing data in every column
for(i in 1:9){ Sample.BK1[is.na(Sample.BK1[,i]),i]<- mean(Sample.BK1[ ,i], na.rm = TRUE) } # replace the NA into the mean value
#of each columns
colSums(is.na(Sample.BK1))# check the missing value replacement


#PCA analysis process
BKpca<-princomp(Sample.BK1,cor = TRUE)
summary(BKpca) # print variance accounted for out put the table
loadings(BKpca) #PC loadings out put the table
plot(BKpca,type="lines") # plot for each comp
BKpca$scores #The principal components schore
biplot(BKpca) # The biplot for the score
