library("readxl")

library(stringr)
library(dummies)
Book_Details <- read_excel("Data_Train.xlsx")


#Book_Details = rbind(Book_Details_Train,Book_Details_Test)
na.omit(Book_Details)

Book_Details$Author = as.factor(Book_Details$Author)
Book_Details$Cust_Edition = as.factor(Book_Details$Cust_Edition)
Book_Details$Cust_Reviews = as.numeric(Book_Details$Cust_Reviews)

Book_Details$Cust_Ratings = str_replace(Book_Details$Cust_Ratings,",","")
Book_Details$Cust_Ratings = as.numeric(Book_Details$Cust_Ratings)

Book_Details$BookCategory = as.factor(Book_Details$BookCategory)
levels(Book_Details$BookCategory)

class(Book_Details$Genre)
Book_Details$Genre = as.factor(Book_Details$Genre)
levels(Book_Details$Genre)

levels(Book_Details$Cust_Edition)


library(randomForest)

model = randomForest(Book_Details$Price ~ 
                       Book_Details$Cust_Reviews
                     + Book_Details$Cust_Ratings 
                     + Book_Details$BookCategory,
                     data = Book_Details[,-1],
                     ntree = 1000, nodeSize = 20, importnace=TRUE)

rmse_function(model$predicted, Book_Details$Price)

rmse_function <- function(pred, actual) {
  sqrt(sum(pred - actual)^2)
}

#----------------------------- Test Data -------------

Book_Details_Test <- read_excel("Data_Test.xlsx")
na.omit(Book_Details_Test)

Book_Details_Test$Author = as.factor(Book_Details_Test$Author)
Book_Details_Test$Cust_Edition = as.factor(Book_Details_Test$Cust_Edition)
Book_Details_Test$Cust_Reviews = as.numeric(Book_Details_Test$Cust_Reviews)

Book_Details_Test$Cust_Ratings = str_replace(Book_Details_Test$Cust_Ratings,",","")
Book_Details_Test$Cust_Ratings = as.numeric(Book_Details_Test$Cust_Ratings)

Book_Details_Test$BookCategory = as.factor(Book_Details_Test$BookCategory)
levels(Book_Details_Test$BookCategory)

class(Book_Details_Test$Genre)
Book_Details_Test$Genre = as.factor(Book_Details_Test$Genre)
levels(Book_Details_Test$Genre)

levels(Book_Details_Test$Cust_Edition)


nn = predict(model,Book_Details_Test[,-1])
