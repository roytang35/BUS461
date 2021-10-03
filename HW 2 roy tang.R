url <- "https://raw.githubusercontent.com/NagaVemprala/R_Programming/main/Datasets/bank/bank-full.csv"
download.file(url, destfile = "Bankinfodataset")
bankdf<-(read.csv("Bankinfodataset",TRUE,sep = ";"))
head(bankdf)

Question 2:
  
targetCustomers <- function(df, job, loan, education, minThreshold=4) {
  bankdf[df$job=="job"] 
  bankdf[df$loan=="loan"]
  bankdf[df$education=="education"] 
}
df1 <- targetCustomers(bankdf, "management", "no", "secondary",2)



#Question 3
targetCustomersV1 <- function(df, 
                              criteria1, 
                              criteria2, 
                              criteria3,
                              minThreshold=4) {
  
  bankdf [bankdf[,c(criteria1$param1)] == criteria1$param2,]      
  bankdf [bankdf[,c(criteria2$param2)] == criteria2$param3,]
  bankdf [bankdf[,c(criteria3$param3)] == criteria3$param4,]         
           
}


df1 <- targetCustomersV1(bankFull, criteria1 = list(param1 = "job", param2 = "management"), 
                         criteria2 = list(param1 = "marital", param2 = "married"), 
                         criteria3 = list(param1 = "education", param2 = "secondary")
                         , 2)



