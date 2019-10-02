library("readxl")
library(stringr)
library(dummies)
Book_Details <- read_excel("Data_Train.xlsx")
na.omit(Book_Details)

Book_Details$Author = as.factor(Book_Details$Author)
Book_Details$Cust_Edition = as.factor(Book_Details$Cust_Edition)
Book_Details$Cust_Reviews = as.numeric(Book_Details$Cust_Reviews)

Book_Details$Cust_Ratings = str_replace(Book_Details$Cust_Ratings,",","")
Book_Details$Cust_Ratings = as.numeric(Book_Details$Cust_Ratings)

regression_model = lm(x = Book_Details$Author+Book_Details$Cust_Edition
                      +Book_Details$Cust_Reviews+ Book_Details$Cust_Ratings
                      + Book_Details,y = Book_Details$Price,data = Book_Details)
