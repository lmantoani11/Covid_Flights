#Load the necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") #data manipulation
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") #machine learning
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") #data upload
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") #data visualization
if(!require(stargazer)) install.packages("stargazer", repos = "http://cran.us.r-project.org") #data visualization

if(!require(neuralnet)) install.packages("knn", repos = "http://cran.us.r-project.org") #neural nets
if(!require(cforest)) install.packages("cforest", repos = "http://cran.us.r-project.org")# random forest
if(!require(gamloess)) install.packages("gamloess  
                                        ", repos = "http://cran.us.r-project.org") #gamloess

#Necessary Libraries
library(tidyverse)
library(caret)
library(readxl)
library(gridExtra)
library(stargazer)
#Access the Data. Format Data Types
Flight_Information <- read_excel("EDX_Final_Dataset.xlsx",
                                           col_types = c("date", "numeric",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric",
                                                         "numeric", "numeric"))
#View the first few data rows
head(Flight_Information)
#Examine the basic statistics of the data
summary(Flight_Information)
#Structure of Data
str(Flight_Information)
# Create Time Series Variables
Flight_Information$Month <- month(Flight_Information$Period)
Flight_Information$Year <- year(Flight_Information$Period)
#Graph Revenue Passenger Miles
RPM_Graph <- Flight_Information %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = (International_RPM/1000000), color = "International")) +
  geom_line(aes(y = (Domestic_RPM/1000000), color = "Domestic")) +
  geom_line(aes(y = (System_RPM/1000000), color = "System")) +
  labs(title = "Revenue Passenger Miles Accross Time",x = "Time",
       y = "Revenue Passenger Miles in Millions", color = "Lines")
RPM_Graph

# View the seasons of the graph
Flight_Information %>% filter(Year %in% c(2005:2006)) %>%
  ggplot(aes(x = Period)) +
  geom_line(aes(y = (International_RPM/1000000), color = "International")) +
  geom_line(aes(y = (Domestic_RPM/1000000), color = "Domestic")) +
  geom_line(aes(y = (System_RPM/1000000), color = "System")) +
  labs(title = "Revenue Passenger Miles Accross Time",x = "Time",
       y = "Revenue Passenger Miles in Millions", color = "Lines")
# Create a season dummy variable
Flight_Information <- Flight_Information %>% mutate(Season = case_when(
  Month %in% c(12, 1, 2) ~ "Winter",
  Month %in% c(3, 4, 5) ~ "Spring",
  Month %in% c(6, 7, 8) ~ "Summer",
  Month %in% c(9, 10, 11) ~ "Fall"
))

# Make dummy variables
dummy_variables <- model.matrix(~ Season, data = Flight_Information)
Flight_Information <- cbind(Flight_Information, dummy_variables)
#Graph Disposable Income
RDI_Graph <- Flight_Information %>% ggplot(aes(Period,Real_Disposable_Income)) +
  geom_line()+
  labs(title = "Disposable Income Accross Time",x = "Year",
       y = "Disposable Income")
RDI_Graph
# Examine Graph and Add Vertical Lines at Stimulus Dates
RDI_Graph_Stimulus <- Flight_Information %>% filter(Real_Disposable_Income > 15000) %>% ggplot(aes(Period, Real_Disposable_Income)) +
  geom_line() +
  geom_vline(aes(xintercept = as.POSIXct(as.Date("2020-03-01")), color = "Stimulus1"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct(as.Date("2020-12-01")), color = "Stimulus2"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct(as.Date("2021-03-01")), color = "Stimulus3"), linetype = "dashed") +
  labs(title = "Disposable Income Across Time", x = "Period", y = "Disposable Income") +
  scale_color_manual(values = c("Stimulus1" = "red", "Stimulus2" = "blue", "Stimulus3" = "orange"),
                     labels = c("Stimulus 1 (Mar 2020)", "Stimulus 2 (Dec 2020)", "Stimulus 3 (Mar 2021)")) +
  guides(color = guide_legend(title = "Stimulus")) +
  guides(color = guide_legend(override.aes = list(linetype = 1)))  
RDI_Graph_Stimulus
# Graph Fare CPI
CPI_Airline_Graph <- ggplot(Flight_Information, aes(x = Period, y = CPI_Airline_Fares)) +
  geom_line(color = "Red") +
  labs(title = "Airfare CPI", x = "Time",
       y = "CPI") 

# Graph Stable CPI
Stable_CPI_Graph <- ggplot(Flight_Information, aes(x = Period, y = CPI_Stable_Baskets)) +
  geom_line(color = "Blue") +
  labs(title = "Stable CPI", x = "Time",
       y = "CPI")
# Display CPI's next to each other
grid.arrange(CPI_Airline_Graph, Stable_CPI_Graph, nrow = 1)

#Graph GDP
GDP_Graph <- Flight_Information %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = GDP, color = 'GDP')) +
  labs(title = "GDP over the Given Period", x = "Time",
       y = "Gross Domestic Product", color = "Lines")

GDP_Graph

Money_Supply_Graph <- Flight_Information %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = M1, color = 'M1')) +
  geom_line(aes(y = M2, color = 'M2')) +
  labs(title = "Money Supply over the Given Period", x = "Time",
       y = "Money Supply", color = "Lines")

Money_Supply_Graph
grid.arrange(GDP_Graph,Money_Supply_Graph,nrow = 1)

#Graph Rent Prices
Rent_Graph <- Flight_Information %>% ggplot(aes(Period,Rent_Prices)) +
  geom_line() +
  labs(title = "Rent Prices Across Time",x = "Time","Average Rent Price In The U.S")
# Graph food prices
Food_Price_Graph <- Flight_Information %>% 
  ggplot(aes(Period,Food_Price_Index)) +
  geom_line() +
  labs(title = "Food Price Index Across Time",x = "Time", y = "Index" )
# Graph Gas Prices
Gas_Price_Graph <- Flight_Information %>%
  ggplot(aes(Period,Ave_Gas_Prices)) +
  geom_line()+
  labs(title = "Gas Price Across Time",x = "Time", y = "Price")

# Display Rent, Food, and Gas prices on the same display 
grid.arrange(Rent_Graph,Food_Price_Graph,Gas_Price_Graph, nrow = 1)

Model_Data <- Flight_Information %>%
  filter(!(Period >= "2001-09-01" & Period <= "2001-11-01") & 
           !(Period >= "2020-03-01" & Period <= "2022-03-01")) %>%
  mutate(International_RPM = International_RPM / 1000000,
         Domestic_RPM = Domestic_RPM / 1000000,
         System_RPM = System_RPM / 1000000,
         Real_Disposable_Income = Real_Disposable_Income /1000 
  )
# Run 1st regression
linear_model_1 <- lm(System_RPM ~ Real_Disposable_Income, data = Model_Data)
#View Summary Statistics
summary(linear_model_1)
# Run 2nd Regression
linear_model_2 <- lm(System_RPM ~ Real_Disposable_Income +
                       CPI_Airline_Fares +
                       Rent_Prices +
                       Food_Price_Index +
                       Ave_Gas_Prices, data = Model_Data)
# View Summary Statistics
summary(linear_model_2)

# Create 3rd Regression
linear_model_3 <- lm(System_RPM ~ Real_Disposable_Income +
                       CPI_Airline_Fares +
                       Rent_Prices +
                       Food_Price_Index +
                       Ave_Gas_Prices +
                       Month +
                       Year +
                       SeasonSpring +
                       SeasonSummer +
                       SeasonWinter, data = Model_Data)
# View Summary Statistics
summary(linear_model_3)
#Regression Table Data 
models_list <- list(linear_model_1, linear_model_2, linear_model_3)
model_names <- c("Base", "Full Regression", "Time Series")

regression_table <- stargazer(models_list, 
                              title = "Regression Results", 
                              type = "text", 
                              keep.stat = c("n", "rsq", "adj.rsq"), 
                              column.labels = model_names)

# Store the Data From The Pandemic
Holdout_Data <- Model_Data %>% filter(Period >= "2022-03-01" & Period <= "2023-11-01")

# Training Data
ML_Data <- Model_Data %>% filter(Year %in% c(2000:2020))

# Calculate the mean absolute error
calculate_MAE <- function(actual, predicted) {
  absolute_errors <- abs(actual - predicted)
  mean_absolute_error <- mean(absolute_errors, na.rm = TRUE)
  return(mean_absolute_error)
}
# Create Function For Mean Percentage Error
MPE <- function(actual, predicted) {
  percentage_error <- ((actual - predicted) / actual) * 100
  mean_percentage_error <- mean(percentage_error, na.rm = TRUE)
  
  return(mean_percentage_error)
}

#Creation of Predictive Models 
# Set seed for reproducibility
set.seed(129)

# Split the data into training and testing sets
test_index <- createDataPartition(y = ML_Data$System_RPM, times = 1, p = 0.3, list = FALSE)
train_set <- ML_Data[-test_index,]
test_set <- ML_Data[test_index,]

#Store object for simplification
ML_Model <- System_RPM ~ Real_Disposable_Income + CPI_Airline_Fares + Rent_Prices + Food_Price_Index + Ave_Gas_Prices + Period + SeasonSpring + SeasonSummer + SeasonWinter

#One standard deviation as base for testing
sd(test_set$System_RPM)

#GamLoess Model
set.seed(129)
train_gamLoess <- train(ML_Model, method = "gamLoess", data = train_set)
gamLoess_preds <- predict(train_gamLoess, test_set)
GamLoess_Test_MAE <- calculate_MAE(test_set$System_RPM,gamLoess_preds)
GamLoess_Test_MPE <-MPE(test_set$System_RPM,gamLoess_preds)

#Random Forest Model
set.seed(129)
train_randomForest <- train(ML_Model, method = "cforest", data = train_set)
randomForest_preds <- predict(train_randomForest, test_set)
RandomForest_Test_MAE<-calculate_MAE(test_set$System_RPM,randomForest_preds)
RandomForest_Test_MPE<- MPE(test_set$System_RPM,randomForest_preds)
#Knn Model
set.seed(129)
train_Knn  <- train(ML_Model, method = "knn", data = train_set)
Knn_preds <- predict(train_Knn, test_set)
knn_Test_MAE <-calculate_MAE(test_set$System_RPM,Knn_preds)
knn_Test_MPE <-MPE(test_set$System_RPM,Knn_preds)

#Model Prediction on 2023 Data
gamLoess_valid <- predict(train_gamLoess,Holdout_Data)
gamLoess_Hold_MAE <-calculate_MAE(Holdout_Data$System_RPM,gamLoess_valid)
gamLoess_Hold_MPE <- MPE(Holdout_Data$System_RPM,gamLoess_valid)
gamLoess_Graph <- Holdout_Data %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = System_RPM, color = "Actual")) +
  geom_line(aes(y = gamLoess_valid, color = "Predicted"))+
  labs(title = "gamLoess Predition", x = "Time",
       y = "RPM", color = "Lines")


randomForest_valid <- predict(train_randomForest,Holdout_Data)
randomForest_Hold_MAE <- calculate_MAE(Holdout_Data$System_RPM,gamLoess_valid)
randomForest_Hold_MPE <- MPE(Holdout_Data$System_RPM,randomForest_valid)
randomForest_Graph <- Holdout_Data %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = System_RPM, color = "Actual")) +
  geom_line(aes(y = randomForest_valid, color = "Predicted"))+
  labs(title = "Random Forest Predition", x = "Time",
       y = "RPM", color = "Lines")

Knn_valid <- predict(train_Knn, Holdout_Data)
knn_Hold_MAE <- calculate_MAE(Holdout_Data$System_RPM,gamLoess_valid)
knn_Hold_MPE <- MPE(Holdout_Data$System_RPM,Knn_valid)
Knn_Graph <- Holdout_Data %>% ggplot(aes(x = Period)) +
  geom_line(aes(y = System_RPM, color = "Actual")) +
  geom_line(aes(y = Knn_valid, color = "Predicted"))+
  labs(title = "K Nearest Neighbor Prediction", x = "Time",
       y = "RPM", color = "Lines")

grid.arrange(gamLoess_Graph,randomForest_Graph,Knn_Graph,ncol() = 1)