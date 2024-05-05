# Recipe-Site-Traffic-Prediction
This project aims to predict high-traffic recipes on a recipe website using machine learning techniques. The predictions will help the website's product manager make data-driven decisions to improve user engagement and overall traffic on the website.


## **1. Data Validation**

This data set has 947 rows, and 8 columns. I have validated all variables and I have made several changes after validation: remove rows with null values in calories, carbohydrates, sugar, and protein and replace null values in high_traffic with “Low”.

-   recipe: 947 unique identifiers without missing values (895 after dataset cleaning). No cleaning is needed.

-   calories: 895 non-null values. I fill 52 missed values with the mean value.

-   carbohydrate: 895 non-null values. I fill 52 missed values with the mean value.

-   sugar: 895 non-null values. I fill 52 missed values with the mean value.

-   protein: 895 non-null values. I fill 52 missed values with the mean value.

-   category: 11 unique values without missing values, whereas there were 10 values in the description. The extra valie is ‘Chicken Breast’. I united it with the ‘Chicken’ value.

-   servings: 6 unique values without missing values. By description, it should be numeric variable, but now it’s character. Has two extra values: ‘4 as a snack’ and ‘6 as a snack’. I united them with ‘4’ and ‘6’ and changed the column’s type to integer.

-   high_traffic: only 1 non-null value (“High”). Replaced null values with “Low”.

## **load in necessary packages**

```{r warning=FALSE, message=FALSE}
library(tidyverse)
library(janitor)
library(tidymodels)
library(modeltime)
library(vip)
library(caret)
############
theme_set(theme_test())
```

## **Overview of the data set**

```{r warning=FALSE, message=FALSE}
recipe_data <- read_csv("recipe_site_traffic_2212.csv")
head(recipe_data)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/38f4b616-a258-42f0-87df-1ba3c2008e10)

## **look at the missing values**
-   validating the dataset for missing values
    ```{r warning=FALSE, message=FALSE}
    data.frame(
      colSums(is.na(recipe_data))
    )
    ```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/b53c9b51-31a0-42b9-9db3-e9f23c8fdbad)

## **data wrangling and exploration & Dealing with Missing Values**
-   There are only 2 and 1 recipes of **4 as a snack** and **6 as a snack** servings, so I’ll rename them to “4” and “6” for simplicity and convert to numerical.
-   replace null values of high_traffic with Low
-   chicken breast turned to just chicken
    ```{r warning=FALSE, message=FALSE}
    recipe_data1 <- recipe_data %>% 
      mutate(
    # calories: 895 non-null values. I fill 52 missed values with the mean value.
    calories=ifelse(is.na(calories),mean(calories, na.rm = TRUE), calories),
    # carbohydrate: 895 non-null values. I fill 52 missed values with the mean value.
    carbohydrate=ifelse(is.na(carbohydrate), mean(carbohydrate, na.rm = TRUE), carbohydrate),
    # sugar: 895 non-null values. I fill 52 missed values with the mean value.
    sugar=ifelse(is.na(sugar), mean(sugar, na.rm = TRUE), sugar) ,
    # protein: 895 non-null values. I fill 52 missed values with the mean value.
    protein=ifelse(is.na(protein), mean(protein, na.rm = TRUE), protein), 
    # category: 11 unique values without missing values, whereas there were 10 values in the description. The extra valie is ‘Chicken Breast’. I united it with the ‘Chicken’ value.
    category=ifelse(category=="Chicken Breast","Chicken", category),
    # servings: 6 unique values without missing values. By description, it should be a numeric variable, but now it’s character. Has two extra values: ‘4 as a snack’ and ‘6 as a snack’. I united them with ‘4’ and ‘6’ and changed the column’s type to integer.
    servings=ifelse(servings=="6 as a snack", 6, 
                    ifelse(servings== "4 as a snack", 4, servings)), 
    # high_traffic: only 1 non-null value (“High”). Replaced null values with “Low”.
     high_traffic=ifelse(is.na(high_traffic), 'Low', high_traffic)   
      )
    # print(str(recipe_data1))
    # print(glimpse(recipe_data1))
    #remove id 
    recipe_data1$recipe <- NULL
    ```

```{r warning=FALSE, message=FALSE}
#check the new data
data.frame(
  colSums(is.na(recipe_data1))
)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/d4f68c9f-305d-4912-99d3-d1461f590c32)
-   inspect the data for the new changes
    ```{r warning=FALSE, message=FALSE}
    d <- recipe_data1 %>% 
      tabyl(servings) %>% 
       adorn_pct_formatting() %>% arrange()
    d %>% 
      ggplot(aes(x=fct_infreq(servings,n), y=n))+
      geom_col(fill=rainbow(4))+
      geom_text(aes(label=n), 
                vjust=-.3)
    ```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/c1578823-eb0d-4757-8ebf-d1e029b8cecc)

```{r warning=FALSE, message=FALSE}
recipe_data1 %>% 
  tabyl(category) %>% 
    adorn_pct_formatting() %>% 
  arrange(desc(n))
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/fd7566f2-f4e5-494f-829b-dd509ba7f82b)

```{r warning=FALSE, message=FALSE}
recipe_data1 %>% 
  tabyl(high_traffic) %>% 
  adorn_pct_formatting()
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/817825a1-b3c7-43d5-b587-5930ede8ec90)

## **Data visualization**
```{r,warning=FALSE, message=FALSE}
recipe_data1 %>% 
    ggplot(aes(x=fct_infreq(factor(servings)), fill=high_traffic))+
    geom_bar(position = 'dodge', col='deepskyblue') +
     geom_text(aes(label=after_stat(count)),                      
     stat='count',
    position=position_dodge(1.0),
     vjust= -0.5, 
     size=3)+
  theme(legend.position = 'bottom')+
  xlab('Servings')+
  ylab('No.Servings')+
  ggtitle('Servings Per Traffic')
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/e96f537d-10ab-4451-92f2-f7412ba10b78)

-   This feature doesn’t have a big influence on the target variable because recipes with high traffic are are many for each servings as compared to those in with low traffic.
```{r warning=FALSE, message=FALSE}
recipe_data1 %>% #filter(high_traffic=="High") %>% 
ggplot(aes(x=fct_infreq(factor(category)), fill=high_traffic))+
  geom_bar(position = 'dodge', col='red')+
  geom_text(
    aes(label=after_stat(count)),
    stat = 'count',
    position=position_dodge(1.0),
     vjust= -.4,
     size=3
  )+
  theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Type of Category')+
  ylab('Count of Each category Per Traffic')+
  ggtitle('Category Per Trafic',  subtitle = 'Low and High Traffic')
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/09f8b124-2fae-4a2c-b28b-795fef59092e)
### **Conclusion:**
-   Potato, Pork, and Vegetable categories have **a lot more recipes with high traffic** than with low traffic.
-   One Dish Meal, Lunch/Snacks, Meat, and Dessert categories have just **more recipes with high traffic** than with low traffic.
    ## **Correlations**
```{r warning=FALSE, message=FALSE}
##select the numeric
cor_data <- recipe_data1 %>% select(is.numeric)
cor_data <- recipe_data1 %>% keep(is.numeric)
#create correlation matrix
corr <-cor(cor_data) 
library(corrplot)
corrplot::corrplot(corr,method="color",addCoef.col = "black")
# corrplot(corr, method = 'number')
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/7686dd39-3966-496c-a422-31fbf96752ea)

-   the **heat-map** above suggests that there is little to no linear negative relationship in 4 variables

-   calories, carbohydrates, sugar, protein. All values are close to 0, so we can say there is a weak relationship between the variables.

## **box plots**
-   individual plots of both nutrients are shown in the facets below
-   looking if there outliers in the nutrients
```{r warning=FALSE, message=FALSE}
# charcter to a factor varable
recipe_data1$high_traffic <- as.factor(recipe_data1$high_traffic)
recipe_data1$category <- as.factor(recipe_data1$category)
recipe_data1$servings <- as.factor(recipe_data1$servings)
levels(recipe_data1$high_traffic) <- c(1,0)
recipe_data1 %>% 
  select(starts_with(c('cal', 'car', 'sug','pr'))) %>% 
  gather() %>% 
  ggplot(
    aes(value, fill=key)
  ) +
  geom_histogram(position = 'dodge') +
  facet_wrap(vars(key), scales = 'free')+
  theme(legend.position = 'left')
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/b27bbeca-82b8-44ed-8502-560c11c76666)
-   from the histograms above, both nutrients are seen to be right-skewed
## **Let’s visually inspect single variables**
-   look at calories
```{r warning=FALSE, message=FALSE}
recipe_data1 %>% 
  select(carbohydrate, calories,protein, sugar, high_traffic) %>% 
  gather('key','value',  -high_traffic) %>% 
  ggplot(aes(value, col=high_traffic))+
  geom_density()+
  facet_wrap(~key, scales = 'free')
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/1fde885f-a7cc-45bd-936e-77073946e2d0)
## **Conclusion:**
the density plots show that there are no significant dependencies of the traffic and the following numerical features: calories, carbohydrates, protein, sugar, servings.
## **Modeling data**
```{r warning=FALSE, message=FALSE}
glimpse(recipe_data1)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/6ed97c9c-a322-418d-8c3c-9d2f4586ab16)

## **Train and Evaluate a Binary Classification Model**
OK, now we’re ready to train our model by fitting the training features to the training labels (`high_trafffic`).
## **Preprocess the data for modeling**
-   normalize all numerical features
-   turn categorical data to numerical data by creating dummy variables
## **fit the model**
### 1.Logistic Regression
```{r warning=FALSE, message=FALSE}
# normalize numerics
recipe_data1$calories <- scale(recipe_data1$calories)
recipe_data1$carbohydrate <- scale(recipe_data1$carbohydrate)
recipe_data1$sugar <- scale(recipe_data1$sugar)
recipe_data1$protein <- scale(recipe_data1$protein)
#################
set.seed(13579)
split <- initial_split(recipe_data1, 0.85)
train <- training(split)
test <- testing(split)
# library(report)
log <- glm(high_traffic~., family = "binomial",data = train)

summary(log)
# log %>% report()
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/bdec12d6-0196-4633-ac73-4c4ab87f196c)

```{r warning=FALSE, message=FALSE}
#make prediction
test$high_traffic <- as.factor(test$high_traffic)
pred <- predict(log, test, type = 'response')
# confusionMatrix(pred, test$high_traffic)
pred <-as.factor(ifelse(pred>=.5,1,0))
# table(pred)
table(test$high_traffic, pred)
confusionMatrix(pred, test$high_traffic)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/7e9a8d17-1433-4c1c-adae-cbd1ed9579b5)
  -   all variables whose p-value lies below the black line are `statistically significant`

```{r}
#accuracy
(accuracy=sum(80,34)/sum(80,34,16,13))
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/fc4cabe6-f537-43c6-ac34-5715f88dfbc3)

### 2.DecisionTree
```{r warning=FALSE, message=FALSE}
library(rpart)
library(rpart.plot)
fit <- rpart(high_traffic~., data = train)
test$high_traffic <- as.factor(test$high_traffic)
pred2 <- predict(fit,data=test, type='class')
table(train$high_traffic,pred2)
# confusionMatrix(pred2,test$high_traffic)
```

```{r}
#Accuracy
sum(422,207)/sum(422,207,59,116)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/d3aca372-e660-4490-9b43-e08caa756095)

## 3 NaiveBayes
```{r warning=FALSE, message=FALSE}
library(e1071)
set.seed(13579)
#########
train$high_traffic <- as.factor(train$high_traffic)
test$high_traffic <- as.factor(test$high_traffic)
########
naive <- naiveBayes(high_traffic ~., data=train) 
pred3 <- predict(naive,test)
confusionMatrix(pred, test$high_traffic)
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/0c3609c2-4f77-422e-b5e5-70b0516dbb6c)

## 4 RandomForest Model
```{r}
library(randomForest)
set.seed(13579)
rf <- randomForest(high_traffic ~ ., data=train, 
                   importance=TRUE,
                   proximity=TRUE)
round(importance(rf), 2)
pred4 <- predict(rf, test, type="response")
cm <- confusionMatrix(pred4, test$high_traffic)
cm
```
![image](https://github.com/LangatErick/Recipe-Site-Traffic-Prediction/assets/124883947/fe490347-3b38-4551-a636-9e81f238a848)

# **Conclusion:**

Recall the accuracy Score of High traffic by the Logistic Regression model is 0.79, and by the **Random Forest** model is 0.77. That means the **Logistic Regression model fits the features better and has less error in predicting values**.

## **Recommendations for future actions**

To help the Product Manager predict the high traffic of the recipes, we can deploy this Logistic Regression Model into production. By implementing this model, about 80% of the prediction will make sure the traffic will be high. This will help the Product Manager build their confidence in generating more traffic to the rest of the website.

To implement and improve the model, I will consider the following steps:

-   Looking for the best ways to deploy this model in terms of performance and costs. The ideal way is to deploy this machine learning model on edge devices for its convenience and security and test the model in newly hired product analysts.

-   Collecting more data, e.g. **time to make**, **cost per serving**, **ingredients**, **site duration time** (how long users were at the recipe page), **income links** (from what sites users came to the recipe page), **combinations of recipes** (what recipes user visited at the same session with the current recipe).

-   Feature Engineering, e.g. increase the number of values in **category**, create more meaningful features from the variables.

## **KPI and the performance of 2 models using KPI**

The company wants to increase the accuracy of prediction of high traffic. Therefore, we would consider using **accuracy** of predictions which predicted high traffic as a KPI to compare 2 models again. The higher the percentage, the better the model performs. The **Logistic Regression model has 80%** of the accuracy whereas the accuracy of the Random Forest is lower (**77%**).
