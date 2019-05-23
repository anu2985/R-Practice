getwd()

install.packages("rlang")
install.packages("pillar")
library(readxl)
library(rlang)
library(pillar)
library(ggplot2)
library(ggcorrplot)
#import the dataset
Food<- read_excel("/Users/anupama/Documents/R-Practice/FoodNutritionProject/Food Nutrition.xlsx")

#check the summary, class and structure of the data
str(Food)
summary(Food) # data looks clean with no missing values

# set the column names
# Clean column names and set right data types
colnames(Food) = make.names(colnames(Food))
attach(Food)
#change the NDB_No as factor
Food$NDB_No<- as.factor(Food$NDB_No)
Food$Shrt_Desc<- as.character(Food$Shrt_Desc)

#Data Extraction & Data Cleaning
#Function to extract the first element of the attribute Shrt_Desc
first.word <- function(my.string){
  unlist(strsplit(my.string, ","))[1]
}

# Apply across all rows
Food$prod_type<- sapply(Food$Shrt_Desc, first.word)

unique(Food$Shrt_Desc) #there are 600 different unique levels
unique(Food$prod_type) #the unique levels have reduced to 109 levels

summary(as.factor(Food$prod_type))

#Setting a Product Category as Dairy and Non Dairy Products
Food$prod_cat<- ifelse(Food$prod_type == "CHEESE" | Food$prod_type == "MILK" |
                         Food$prod_type == "YOGURT" | Food$prod_type == "EGG" |
                         Food$prod_type == "CREAM" | Food$prod_type == "BUTTER" |
                         Food$prod_type == "CHEESE PRODUCT" | Food$prod_type == "CHEESE SPRD" |
                         Food$prod_type == "CHEESE FD" | Food$prod_type == "MILK SHAKES", "DAIRY", "NON-DAIRY")

summary(as.factor(Food$prod_cat))
# setting the Energy_Kcal as low, Medium, High Categories
Food$Energy_LMH<- ifelse(Food$Energ_Kcal <= 150 , "Low Energy" ,
                         ifelse(Food$Energ_Kcal > 150 & Food$Energ_Kcal <= 350 , "Medium Energy","High Energy"
                            ))

#checking distributions and creating tables

dairy_energy<- table(Food$prod_cat, Food$Energy_LMH)

dairy_energy


#Understanding the data qplot

## Histogram
qplot(Energ_Kcal, data = Food,
      main= "Distribution of Energy in Kcal",
      ylab= "Frequency",
      xlab= "Energy_Kcal")
      

# density plot
qplot(Energ_Kcal, data = Food,
      geom = "density",
      main ="Distribution of Energy in Kcal")

# Detailed plot showing Enegry Kcal by Dairy and Non Dairy Product Category
qplot(Food$Energ_Kcal, fill= Food$prod_cat,  data = Food, 
      main= "Energy of Different Product type") 


qplot(Food$Energ_Kcal, Food$Water_.g., 
      color = Food$prod_cat, data= Food,
      main= " Scatterplot to understand Water and Energy in Dairy & Non Dairy Products",
      xlab = "Energy_Kcal",
      ylab = "Water(g)")


qplot(Food$Protein_.g., Food$Energ_Kcal, 
      color = Food$prod_cat, data= Food,
      main= " Scatterplot to understand Protein and Energy Content in Dairy & Non Dairy Products",
      xlab = "Energy_Kcal",
      ylab = "Protein(g)")

qplot(Food$Water_.g., Food$Protein_.g., color = Food$Energy_LMH,
      data= Food,
      main = "Relationship between Protein, Water Content & Energy Levels provided by the different Products",
      xlab= "Water(g)",
      ylab= "Protein(g)")

# Correlation matrix of all variables which are numeric.
FoodCorrData<-Food[,c(3:20)]
corr <- round(cor(FoodCorrData), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Nutritional Value provided in different Products", 
           ggtheme=theme_bw)

