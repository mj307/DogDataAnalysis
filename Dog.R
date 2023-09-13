# data loading
data = read.csv('/Users/medhavijam/Downloads/dog_data.csv',check.names = FALSE)

# clean data table
data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[-1,] # this deletes the first row
rownames(data) <- 1:nrow(data) #this updates all the row names

#creating a smaller data frame over the data we want to analyze
newDogData <- data.frame(data$`Dog breed`,data$`POPULARITY IN US`,data$`LIFETIME COST`,data$LONGEVITY,data$INTELLIGENCE,data$`# OF GENETIC AILMENTS`,data$`SIZE CATEGORY`)
newDogData
colnames(newDogData)[c(1,2,3,4,5,6,7)] <- c("DOG BREED", "POPULARITY IN US",'LIFETIME COST','LONGEVITY','INTELLIGENCE','# OF GENETIC AILMENTS','SIZE CATEGORY')
newDogData <- newDogData[,colSums(is.na(newDogData))<nrow(newDogData)]
newDogData <- na.omit(newDogData)
newDogData <- newDogData[!(newDogData$`POPULARITY IN US`=="no data" | newDogData$INTELLIGENCE == 'no data' | newDogData$`LIFETIME COST`=="no data"| newDogData$`DOG BREED`=="Looked - nothing"|newDogData$`DOG BREED`=="NOT POSSIBLE IF NO INTELLIGENCE DATA"),]

# converting data into a numerical format
library(readr)
newDogData$`LIFETIME COST`<- parse_number(newDogData$`LIFETIME COST`)

#data analysis between each category
plot(newDogData$LONGEVITY,newDogData$`POPULARITY IN US`,main="Relationship between Dog Popularity and Longevity",xlab = "Longevity",ylab="Popularity",col="lightgreen",pch=20)

plot(newDogData$LONGEVITY,newDogData$`LIFETIME COST`,main="Relationship between Longevity and Lifetime Cost",xlab = "Longevity",ylab="Lifetime Cost",col="pink",pch=20)

plot(newDogData$`# OF GENETIC AILMENTS`,newDogData$`POPULARITY IN US`,main="Relationship between Dog Popularity and Number of Genetic Ailments",xlab = "# of Genetic Ailments",ylab="Longevity",col="lightblue",pch=20)

plot(newDogData$INTELLIGENCE,newDogData$`POPULARITY IN US`,main="Relationship between Dog Popularity and Intelligence",xlab = "Intelligence",ylab="Popularity",col="orange",pch=20)
newDogData

# more data analysis between small, medium, and big dogs

#analysis for small dogs
smallDogData <- newDogData[(newDogData$`SIZE CATEGORY`=="small"),]
smallDogData <- subset(smallDogData,select=-`SIZE CATEGORY` )
smallDogData$`POPULARITY IN US` <- as.numeric(as.character(smallDogData$`POPULARITY IN US`))
smallDogData$`LIFETIME COST` <- as.numeric(as.character(smallDogData$`LIFETIME COST`))
smallDogData$LONGEVITY <- as.numeric(as.character(smallDogData$LONGEVITY))
smallDogData$INTELLIGENCE <- as.numeric(as.character(smallDogData$INTELLIGENCE))
smallDogData$`# OF GENETIC AILMENTS` <- as.numeric(as.character(smallDogData$`# OF GENETIC AILMENTS`))
mean(smallDogData$`# OF GENETIC AILMENTS`)
smallDogDF <- data.frame("Popularity in US"=mean(smallDogData$`POPULARITY IN US`),
                         "Lifetime Cost"=mean(smallDogData$`LIFETIME COST`),
                         "Longevity" = mean(smallDogData$LONGEVITY),
                         "Intelligence"=mean(smallDogData$INTELLIGENCE),
                         "# of Genetic Ailments"=mean(smallDogData$`# OF GENETIC AILMENTS`),check.names = FALSE)
smallDogDF

#analysis for medium dogs
mediumDogData <- newDogData[(newDogData$`SIZE CATEGORY`=="medium"),]
mediumDogData <- subset(mediumDogData,select=-`SIZE CATEGORY` )
mediumDogData$`POPULARITY IN US` <- as.numeric(as.character(mediumDogData$`POPULARITY IN US`))
mediumDogData$`LIFETIME COST` <- as.numeric(as.character(mediumDogData$`LIFETIME COST`))
mediumDogData$LONGEVITY <- as.numeric(as.character(mediumDogData$LONGEVITY))
mediumDogData$INTELLIGENCE <- as.numeric(as.character(mediumDogData$INTELLIGENCE))
mediumDogData$`# OF GENETIC AILMENTS` <- as.numeric(as.character(mediumDogData$`# OF GENETIC AILMENTS`))
mediumDogDF <- data.frame("Popularity in US"=mean(mediumDogData$`POPULARITY IN US`),
                         "Lifetime Cost"=mean(mediumDogData$`LIFETIME COST`),
                         "Longevity" = mean(mediumDogData$LONGEVITY),
                         "Intelligence"=mean(mediumDogData$INTELLIGENCE),
                         "# of Genetic Ailments"=mean(mediumDogData$`# OF GENETIC AILMENTS`),check.names = FALSE)
mediumDogDF

#analysis for large dogs
largeDogData <- newDogData[(newDogData$`SIZE CATEGORY`=="large"),]
largeDogData <- subset(largeDogData,select=-`SIZE CATEGORY` )
largeDogData$`POPULARITY IN US` <- as.numeric(as.character(largeDogData$`POPULARITY IN US`))
largeDogData$`LIFETIME COST` <- as.numeric(as.character(largeDogData$`LIFETIME COST`))
largeDogData$LONGEVITY <- as.numeric(as.character(largeDogData$LONGEVITY))
largeDogData$INTELLIGENCE <- as.numeric(as.character(largeDogData$INTELLIGENCE))
largeDogData$`# OF GENETIC AILMENTS` <- as.numeric(as.character(largeDogData$`# OF GENETIC AILMENTS`))
largeDogDF <- data.frame("Popularity in US"=mean(largeDogData$`POPULARITY IN US`),
                          "Lifetime Cost"=mean(largeDogData$`LIFETIME COST`),
                          "Longevity" = mean(largeDogData$LONGEVITY),
                          "Intelligence"=mean(largeDogData$INTELLIGENCE),
                          "# of Genetic Ailments"=mean(largeDogData$`# OF GENETIC AILMENTS`),check.names = FALSE)
largeDogDF
