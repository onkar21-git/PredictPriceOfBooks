library("readxl")
library(xlsx)
library(stringr)
library(dummies)

Book_Details_train <- read_excel("Data_Train.xlsx")
Book_Details_Test <- read_excel("Data_Test.xlsx")
Book_Details_Test$Price=0
Book_Details <- rbind(Book_Details_train,Book_Details_Test)


#Book_Details = rbind(Book_Details_Train,Book_Details_Test)
na.omit(Book_Details)

Book_Details$Author = as.factor(Book_Details$Author)
Book_Details$Cust_Edition = as.factor(Book_Details$Cust_Edition)
Book_Details$Cust_Reviews = as.numeric(Book_Details$Cust_Reviews)

Book_Details$Cust_Ratings = str_replace(Book_Details$Cust_Ratings,",","")
Book_Details$Cust_Ratings = as.numeric(Book_Details$Cust_Ratings)

Book_Details$BookCategory = as.factor(Book_Details$BookCategory)
levels(Book_Details$BookCategory)

class(Book_Details$Year)
Book_Details$Genre = as.factor(Book_Details$Genre)
levels(Book_Details$Genre)

Book_Details$Year = as.numeric(Book_Details$Year)

Book_Details = Book_Details[,is.na(Book_Details$Year) == FALSE]

Book_Details = subset(Book_Details,is.na(Book_Details$Year)==FALSE)
sum(is.na(Book_Details$Year))

Book_Details$AgeOfBook = 2019 - Book_Details$Year

library(randomForest)


model = randomForest(Book_Details$Price ~ Book_Details$Cust_Reviews
                     + Book_Details$Cust_Ratings 
                     + Book_Details$BookCategory
                     + Book_Details$AgeOfBook
                     + Book_Details$Cust_Edition,
                     data = Book_Details[,-1],
                     ntree = 1000, nodeSize = 20, importnace=TRUE)



rmse_function(model$predicted[1:6237], Book_Details$Price[1:6237])


write.csv(x = model$predicted[6208:7767], file = "output.csv")
rmse_function <- function(pred, actual) {
  sqrt(sum(pred - actual)^2)
}
