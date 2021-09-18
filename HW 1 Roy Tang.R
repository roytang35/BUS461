#' Q1. Create two vectors yearsOfExperience	and annualSalary, using the below values:

yearsOfExperience<- c(1.1,1.3,1.5,2.0,2.2,2.9,3.0,3.2,3.2,3.7,3.9,4.0,4.0,4.1,4.5,4.9,5.1,5.3,5.9,6.0,6.8,7.1,7.9,8.2,8.7,9.0,9.5,9.6,10.3,10.5)
annualSalary<-c(39343,46205,37731,43525,39891,56642,60150,54445,64445,57189,63218,55794,56957,57081,61111,67938,66029,83088,81363,93940,91738,98273,101302,113812,109431,105582,116969,112635,122391,121872)


#' Q2. Take a screenshot of the environment variables created.
#' (Include the screenshot in the word document you upload to moodle)
#' Please describe what is the view in which you can see these variables. 
#' 

#' Q3. Print type of each vector
typeof(yearsOfExperience)
typeof(annualSalary)



#' Q4. Create a dataframe as employees using the same two vectors created in Q1 and 
#' name the columns of the dataframe as yearsOfExperience	and annualSalary
dataemployees<- data.frame(yearsOfExperience=yearsOfExperience,annualSalary=annualSalary)

print(dataemployees)


#' Q5. Create a new column perYearExperience to the dataframe. 
#' The value for this column should be annualSalary/yearsOfExperience
perYearExperience=(annualSalary/yearsOfExperience)
dataemployees1<-cbind(dataemployees,perYearExperience)
print(dataemployees1)


#' Q6. Create a logical vector extractRows of five elements with all the element values as TRUE 

extractRows<- c(TRUE,T,T,T,T)




    
#' Q7. Extract first five rows of employees dataframe using the logical vector extractRows
#' Compare and validate the output using head function. Provide a screenshot in the word document. 

dim(dataemployees)
dataemployees[extractRows,]


#' Q8. Create an integer vector of values 1 to 30 using a sequence operator and name it as filterCriteria
filterCriteria<-c(seq(1:30))


#' Q9. Create a logical vector of 30 elements with every 5th element as TRUE value and rest of the elements as FALSE. 
#' Name the vector the same as filterCriteria 
#' (Hint) filterCriteria can be created with a logical operation such as filterCriteria <- filterCriteria < 8 
#' With the above command, first seven elements are TRUE and rest all are FALSE. Use such an arithmatic operation 
#' to create filterCriteria. 
#' e.g: First 6 elements of the newly created filterCriteria vector should look like 
#' filterCriteria : FALSE FALSE FALSE FALSE TRUE FALSE 

filterCriteria<-c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,
                   FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,
                   FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE)


#' Q10. Create a new dataframe filteredEmployees from the original dataframe employees using the logical vector
#' filterCriteria created in Q9. 


filteredEmployees<-cbind(dataemployees1,filterCriteria)
print(filteredEmployees)

#' Q11. Display the first 6 records of the dataframe filteredEmployees using head function 
head(filteredEmployees)

#' Q12. Validate that the first record of filteredEmployees dataframe should be the fifth record of the original 
#' dataframe employees. It should be same as the record extracted using the filter criteria 
#' filteredEmployees[filteredEmployees$yearsOfExperience == 2.2,]
filteredEmployees[c(5,10,15,20,25,30)]
filteredEmployees[seq(1:30)%%5==0,]

filteredEmployees[yearsOfExperience == 2.2,]
#' Q13. Display the structure of the prebuilt dataset diamonds 

str(diamonds)


#' Q14. Display the first 10 records of the diamonds dataset using head() function and override the parameters 
#' Provide explanation for what do I mean by overriding default parameters. 
#' Hint: Look for online help provided by RStudio 
#' 
head(diamonds,n=10)

#' Q15. Create dataframe goodDiamonds from diamonds dataframe with each diamond cut being "Good" 

goodDiamonds<-data.frame(diamonds[diamonds$cut=="Good",])
print(goodDiamonds)

#' Q16. display unique values of cut colums of diamonds dataframe 
unique(diamonds$cut)

#' Q17. Assume that diamonds is a sales dataset. You would like to give discount as follow: 
#' 10%, 15%, 20% discount on price of Fair, Good, Very Good diamonds and 25% on Premium & Ideal diamonds 
#' Create a new dataframe column with the updated price 
dim (diamonds[diamonds$color=="E",])

discountedPrice<- function(color,price){
      if (color=="E"){
        price*0.75
        }else {
          price*1
        }
}
for (i in 1:dim(diamonds)[1]){
  diamonds[i,"discountedPrice"]=discountedPrice(diamonds[i, "color"],diamonds[i, "price"])
}
#' Q18. Group by diamonds cut and display the count. Output should look like below: 
#' cut       countDiamonds
#' 
#' 1 Fair               1610
#' 2 Good               4906
#' 3 Very Good         12082
#' 4 Premium           13791
#' 5 Ideal             21551 

tapply(diamonds$cut, diamonds$cut, FUN = function(x) length(x))
length(diamonds$cut)

#' Q19. Only display data from diamonds that have a cut value of Fair or Good and a price at or under $600
goodfairDiamonds <- subset(diamonds, ((cut == "Good") & (price <= 600) )|( (cut == "Fair") & (price <= 600) ),)
goodfairDiamonds

#' Q20. Display the dimensions of preloaded mtcars dataset 
#' 
dim(mtcars)

#' Q21. Use preloaded mtcars dataset and create a character vector of cars whose mileage is 21.0

mtcars %>%
  filter(mpg == 21)%>%
  print(4)%>%
  print()


#' Q22. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl"
mtcars$FactorCyl <- factor(mtcars$cyl, 
                           level = c(4,6,8), 
                           labels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"),
                           ordered = FALSE)
str(mtcars)

#' Q23. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl". Make the factor of order type 

mtcars$FactorCyl <- factor(mtcars$cyl, 
                           level = c(4,6,8), 
                           labels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"),
                           ordered = TRUE)
str(mtcars)

#' Q24. Display unique values of new column factorCyl and write what you observe 
#' 
unique(mtcars$FactorCyl)
#' Q25. Use subset function to extract automatic cars into autoCars and manual cars into manualCars dataframe
#' Use the am column of mtcars dataset to separate the records  
manual <- mtcars[mtcars$am ==1,]
manual

auto <- mtcars[mtcars$am == 1,]
