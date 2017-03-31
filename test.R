my_data <-read.csv("haberman.data", sep=",", header=TRUE) 
names(my_data) <- c("Age","nodes detected", "year of operation","Survival")
View(my_data)

class(my_data)

getwd()
