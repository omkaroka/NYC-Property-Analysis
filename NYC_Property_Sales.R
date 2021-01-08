## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----NYC_Data, message = FALSE, warning = FALSE-----------------------------------------------------------------

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(anytime)) 
  install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrr)) 
  install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) 
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) 
  install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) 
  install.packages("randomForest", repos = "http://cran.us.r-project.org")

#Loading Libraries:
library("stringr")
library("tidyverse")
library("caret")
library("anytime")
library("lubridate")
library("corrr")
library("knitr")
library("corrplot")
library("randomForest")
options("scipen" = 10)

nyc <- 
  as_data_frame(fread("C:/Users/omkar.oka/Desktop/DataScience/NYC House/nyc-rolling-sales.csv")) 
##Replace the path with your local computer path while executing the code.



## ----structure, message = FALSE, warning = FALSE----------------------------------------------------------------
str(nyc)


## ----dups,  message = FALSE, warning = FALSE--------------------------------------------------------------------
nyc <- nyc %>% select(-V1) ##Removing Row ID column V1
dup <- nyc %>% filter(duplicated(nyc) == TRUE) %>% nrow()


## ----cleanup,  message = FALSE, warning = FALSE, echo = FALSE---------------------------------------------------
nyc <- nyc %>%
  separate(col = "BUILDING CLASS CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE DATE", into = c("SALE DATE", "TIME"), sep = " ")
#Separating the Building class category into the category and its unique identifier number
nyc <- nyc[,c(-8,-23)] #removing columns 'EASE-MENT' and 'TIME OF SALE'
nyc <- unique(nyc)
str(nyc)


## ----Building_Age,  message = FALSE, warning = FALSE------------------------------------------------------------
nyc <- nyc %>% mutate(`BUILDING AGE` = as.integer(format(as.Date(`SALE DATE`, format="%Y-%m-%d"),"%Y")) - `YEAR BUILT`)
# Creating a new column called 'Building Age' transforming the variable, 'Year Built'


## ----conversion,  message = FALSE, warning = FALSE, echo=FALSE--------------------------------------------------
fac <- c(1,3,4,5,8,11,18,19)
nyc <- nyc %>% mutate_at(fac, funs(factor(.)))
levels(nyc$BOROUGH) <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")#We will be converting the character and discrete numeric columns to factors.
num <- c(15,16,17,20)
nyc <- nyc %>% mutate_at(num, funs(as.numeric(.)))

chr <- c(6,7)
nyc <- nyc %>% mutate_at(chr, funs(as.character(.)))
#Converting the character columns that have numbers to numeric and 
##some columns to character for further analysis.  

nyc$`SALE DATE` <- as.Date(nyc$`SALE DATE`, format="%Y-%m-%d")
str(nyc)


## ----sale_prep, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
nyc_data <- nyc #Keeping backup in case we need to revert back

nyc <- nyc_data %>% filter((!`SALE PRICE` == 0) & !is.na(`SALE PRICE`)) 
#Removing the Sale Price of zero and NA since these are just Property transfers




## ----Data_Description, message = FALSE, warning = FALSE, echo= FALSE--------------------------------------------
variable.type <- lapply(nyc, class)
options(width = 60)


variable.description <- c("Name of the borough where the property is located", 
"Neighbourhood name", "Building class category code to identify similar properties", 
"Building class category title to identify similar properties", 
"Assigned tax class of the property - Classes 1, 2, 3 or 4", 
"Sub-division of the borough for property location", 
"Sub-division of a Tax Block for every property location",
"Used to describe a property's constructive use", "Property's street address", 
"Property's apartment number", "Property's postal code", 
"Number of residential units at the listed property",
"Number of commercial units at the listed property",
"Total number of units at the listed property", 
"Land area of the property listed in square feet", 
"Total area of all the floors of a building", "Property's construction year", 
"Assigned tax class of the property at sale",
"Used to describe a property's constructive use at sale",
"Price paid for the property", "Date of property sale", "Age of the Building")

variable.name <- colnames(nyc)

nyc_datadesc <- as_data_frame(cbind(variable.name, variable.type, variable.description))
colnames(nyc_datadesc) <- c("Variable Name","Data Type","Variable Description")
library(knitr)
kable(nyc_datadesc)



## ----sale_analysis, message = FALSE, warning = FALSE, echo= FALSE-----------------------------------------------
quantile(nyc$`SALE PRICE`, probs = seq(from = 0, to = 1, by = .1))


## ----sale_price_1000, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------
nyc %>% filter(`SALE PRICE` <= 1000) %>% nrow()


## ----sale_price cleanup, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------
## Remove sale price < 1000$
nyc <- nyc %>% filter(!`SALE PRICE` <= 1000)  

quantile(nyc$`SALE PRICE`, probs = seq(from = 0, to = 1, by = .1))


## ----land_sq_ft1, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------
summary(nyc$`LAND SQUARE FEET`)


## ----land_sq_ft2, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------
quantile(nyc$`LAND SQUARE FEET`, probs = seq(from = 0, to = 1, by = .1), na.rm = TRUE)




## ----gross_sq_ft1, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------
summary(nyc$`GROSS SQUARE FEET`)


## ----gross_sq_ft2, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------
quantile(nyc$`GROSS SQUARE FEET`, probs = seq(from = 0, to = 1, by = .1), na.rm = TRUE)



## ----units1, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------------

summary(nyc$`RESIDENTIAL UNITS`)


## ----units2, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------------
nyc %>% filter(`RESIDENTIAL UNITS` > 5) %>% nrow()


## ----units3, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------------

summary(nyc$`COMMERCIAL UNITS`)


## ----building_age1, message = FALSE, warning = FALSE, echo= FALSE-----------------------------------------------
summary(nyc$`BUILDING AGE`)


## ----building_age2, message = FALSE, warning = FALSE, echo= FALSE-----------------------------------------------
summary(nyc$`YEAR BUILT`)


## ----building_age3, message = FALSE, warning = FALSE, echo= FALSE-----------------------------------------------
nyc %>% filter(`BUILDING AGE` > 200 & `YEAR BUILT` != 0) %>% 
  arrange(desc(`BUILDING AGE`)) %>% nrow()


## ----borough1, message = FALSE, warning = FALSE, echo= FALSE----------------------------------------------------
ggplot(data = nyc, aes(x = `BOROUGH`, fill = `BOROUGH`)) +
  geom_bar() +
  ggtitle("Most In-Demand Borough in NYC", subtitle = "Borough-wise # of property sales in NYC") +
  scale_y_continuous("# of Property Sales", labels = scales::comma) +
  scale_x_discrete("Borough")


## ----borough2, message = FALSE, warning = FALSE, echo= FALSE----------------------------------------------------

ggplot(data = nyc, aes(x = `BOROUGH`, y = mean(`SALE PRICE`), fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  ggtitle("Most Expensive Borough in NYC", subtitle = "Borough-wise Avg Property Sale Price in NYC") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Borough")




## ----borough3, message = FALSE, warning = FALSE, echo= FALSE----------------------------------------------------

nyc %>% group_by(BOROUGH, NEIGHBORHOOD) %>% summarise(SOLD = n()) %>% arrange(desc(SOLD)) %>% head(10) %>% 
        ggplot(aes(x = `NEIGHBORHOOD`, y = SOLD, fill = `BOROUGH`)) + geom_bar(position = 'dodge', stat='identity') +
  ggtitle("Top 10 boroughs as per number of sold properties") +
     geom_text(aes(label=SOLD), position=position_dodge(width=0.9), hjust=-0.25) + coord_flip() +
  theme(legend.position = "bottom")


## ----neighbrhd1, message = FALSE, warning = FALSE, echo= FALSE--------------------------------------------------
df1 <- as.data.frame(table(nyc$BOROUGH, nyc$NEIGHBORHOOD))
names(df1) <- c('BOROUGH','NEIGHBORHOOD', 'Freq')
df1 <- df1 %>% arrange(desc(Freq)) %>% head(10)

ggplot(df1, aes(x = `NEIGHBORHOOD`, y = `Freq`, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Most-in demand Neighborhood in NYC", subtitle = "Top Neighborhoods by Number") +
  theme(legend.position = "bottom") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Neighborhood") 


## ----neighbrhd2, message = FALSE, warning = FALSE, echo= FALSE--------------------------------------------------
df2 <- 
  nyc %>% group_by(BOROUGH, NEIGHBORHOOD) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(desc(MeanSP)) %>% head(10)

ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Most Expensive Neighborhoods in NYC", 
          subtitle = "Top Neighborhoods by Avg Price") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 


## ----neighbrhd3, message = FALSE, warning = FALSE, echo= FALSE--------------------------------------------------
df2 <- 
  nyc %>% group_by(BOROUGH, NEIGHBORHOOD) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(MeanSP) %>% head(10)

ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Least Expensive Neighborhoods in NYC", subtitle = "Top Neighborhoods by the lowest avg. Price") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Neighborhood")  


## ----building1, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
df1 <- as.data.frame(table(nyc$BOROUGH, nyc$`BUILDING CLASS CATEGORY`))
names(df1) <- c('BOROUGH','BUILDING CLASS CATEGORY', 'Freq')
df1 <- df1 %>% group_by(BOROUGH) %>% arrange(desc(Freq)) %>% head(10)

ggplot(df1, aes(x = `BOROUGH`, y = `Freq`, fill = `BUILDING CLASS CATEGORY`)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Most In-Demand Buildings in NYC by Borough", subtitle = "Top types of Buildings sold in NYC") +
  scale_y_continuous("# of Sales", labels = scales::comma) +
  scale_x_discrete("Borough") 


## ----building2, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
df2 <- 
  nyc %>% group_by(BOROUGH, `BUILDING CLASS CATEGORY`) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(desc(MeanSP)) %>% head(10)

ggplot(data = df2, aes(x = `BUILDING CLASS CATEGORY`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Most Expensive Buildings in NYC", subtitle = "Top Types of Property by Value") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) + scale_x_discrete("Building Type") 


## ----building3, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
df2 <- 
  nyc %>% group_by(BOROUGH, `BUILDING CLASS CATEGORY`) %>% 
  summarise(MeanSP = mean(`SALE PRICE`)) %>% 
  arrange(MeanSP) %>% head(10)

ggplot(data = df2, aes(x = `BUILDING CLASS CATEGORY`, y = MeanSP, fill = `BOROUGH`)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("Least Expensive Buildings in NYC", subtitle = "Lowest Types of Property by Value in NYC in 2016") +
  scale_y_continuous("Avg Sale Price", labels = scales::dollar) +
  scale_x_discrete("Building Type") 


## ----building4, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
nyc %>% group_by(BOROUGH, `TAX CLASS AT TIME OF SALE`) %>% summarise(SOLD_UNITS = n()) %>% 
        ggplot(aes(x = reorder(`TAX CLASS AT TIME OF SALE`, -SOLD_UNITS), y = SOLD_UNITS, fill = BOROUGH)) + geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Tax Class Distribution") +
  theme(legend.position = "right") + scale_x_discrete("Tax Class")




## ----propsize1, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
ggplot(data = nyc, aes(x = log(`LAND SQUARE FEET`), y = log(`SALE PRICE`), color = `BOROUGH`)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme(legend.position = "bottom") +
  facet_wrap(~ BOROUGH) +
  ggtitle("Price Vs Land Square Footage in NYC", 
          subtitle = "Distribution of Sale Price vs Land Square feet Borough-wise") +
  scale_y_continuous("Property Sale Price", labels = scales::dollar) +
  scale_x_continuous("Land Square Footage", labels = scales::comma)


## ----propsize2, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
ggplot(data = nyc, aes(x = log(`GROSS SQUARE FEET`), y = log(`SALE PRICE`), color = `BOROUGH`)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~ BOROUGH) +
  ggtitle("Price Vs Gross Square Footage in NYC",
          subtitle = "Distribution of Sale Price vs Gross Square feet Borough-wise") +
  scale_y_continuous("Property Sale Price", labels = scales::dollar) +
  scale_x_continuous("Gross Square Footage", labels = scales::comma) +
  theme(legend.position = "bottom")


## ----propsize3, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
df1 <- nyc %>% filter(`LAND SQUARE FEET` != 0) %>%
      mutate(PriceLSF = `SALE PRICE`/`LAND SQUARE FEET`) %>%
      group_by(`BOROUGH`, `NEIGHBORHOOD`) %>%
      summarise(MeanPriceLSF = mean(PriceLSF, na.rm = TRUE)) %>%
      arrange(desc(MeanPriceLSF)) %>% head(15)

ggplot(data = df1, aes(x = `NEIGHBORHOOD`, y = MeanPriceLSF, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("NYC Price per Square Feet", subtitle = "Top Price/sqft Neighborhood") +
  scale_y_continuous("Price/sqft", labels = scales::dollar) +
  scale_x_discrete("Neighborhood") 


## ----propsize4, message = FALSE, warning = FALSE, echo= FALSE---------------------------------------------------
df2 <- nyc %>% filter(`LAND SQUARE FEET` != 0) %>%
  mutate(PriceLSF = `SALE PRICE`/`LAND SQUARE FEET`) %>%
  group_by(`BOROUGH`, `NEIGHBORHOOD`) %>%
  summarise(MeanPriceLSF = mean(PriceLSF, na.rm = TRUE)) %>%
  arrange(MeanPriceLSF) %>% head(15)

ggplot(data = df2, aes(x = `NEIGHBORHOOD`, y = MeanPriceLSF, fill = `BOROUGH`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggtitle("NYC Price per Square Feet", subtitle = "Lowest Price/sqft Neighborhood") +
  scale_y_continuous("Price/ sqft", labels = scales::dollar) +
  scale_x_discrete("Neighborhood")


## ----buildingage1, message = FALSE, warning = FALSE, echo= FALSE------------------------------------------------

nyc %>% filter(!`BUILDING AGE` >= 500) %>% ggplot(aes(`BUILDING AGE`, log(`SALE PRICE`), color = BOROUGH)) + geom_jitter() +
geom_smooth() +
theme(legend.position = "bottom")  + 
ggtitle("Age of Properties vs Sale price") +
scale_y_continuous("Sale Price in Log Scale", labels = scales::dollar)


## ----dataprep, message = FALSE, warning = FALSE-----------------------------------------------------------------
nyc$`SALE MONTH` <- as.factor(months(nyc$`SALE DATE`))
nyc$NEIGHBORHOOD <- as.factor(nyc$NEIGHBORHOOD)

nyc_final <- nyc[, -c(3, 5, 6, 7, 8, 9, 10, 17, 19, 21)]
nyc_final <- nyc_final[c(1:10, 12,13,11)]




## ----corr1, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------------
nyc_corr <- cor(nyc_final[sapply(nyc_final, is.numeric)], use = "complete.obs")



## ----corr2, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------------
corrplot(nyc_corr, type = "upper",order = "hclust", 
         tl.col = "black", tl.srt = 45)


## ----corr3, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------------
temp <- nyc_final[,c(1:4,10,12,13)]    
num <- c(1:7)
temp <- temp %>% mutate_at(num, funs(as.numeric(.)))



## ----corr4, message = FALSE, warning = FALSE, echo= FALSE-------------------------------------------------------
corrplot(cor(temp), type = "upper",order = "hclust", 
         tl.col = "black", tl.srt = 45)


## ----pred_analysis1, message = FALSE, warning = FALSE, echo= FALSE----------------------------------------------

nyc$`SALE MONTH` <- as.factor(months(nyc$`SALE DATE`))

nyc_pred <- nyc[, -c(3, 5, 6, 7, 8, 9, 10, 17, 19, 21)]
nyc_pred$NEIGHBORHOOD <- as.factor(nyc_pred$NEIGHBORHOOD)

nyc_pred <- nyc_pred[c(1:10, 12,13,11)]

str(nyc_pred)


## ----pred_analysis2, message = FALSE, warning = FALSE-----------------------------------------------------------

set.seed(1)
index <- sample(nrow(nyc_pred),nrow(nyc_pred)*0.80)
nyc_pred.train <- nyc_pred[index,]
nyc_pred.test <- nyc_pred[-index,]


## ----pred_analysis3, message = FALSE, warning = FALSE-----------------------------------------------------------
# 1. Single Factor Regression - Borough
model1 <- lm(nyc_pred.train$`SALE PRICE` ~ nyc_pred.train$BOROUGH)
res_borough <- summary(model1)

res_borough


## ----pred_analysis3b, message = FALSE, warning = FALSE----------------------------------------------------------
pf(res_borough$fstatistic[1],res_borough$fstatistic[2],res_borough$fstatistic[3],lower.tail = FALSE)


## ----pred_analysis4, message = FALSE, warning = FALSE-----------------------------------------------------------
# 2. Single Factor Regression - Neighborhood

model1 <- lm(nyc_pred.train$`SALE PRICE` ~ nyc_pred.train$NEIGHBORHOOD)
res_neigh <- summary(model1)

# F-statistic p-value
pf(res_neigh$fstatistic[1],res_neigh$fstatistic[2],res_neigh$fstatistic[3],lower.tail = FALSE)



## ----pred_analysis5, message = FALSE, warning = FALSE-----------------------------------------------------------
nyc_pred.train$N_BLOOMFIELD = ifelse(nyc_pred.train$NEIGHBORHOOD == "BLOOMFIELD", 1,0)
nyc_pred.train$N_FASHION = ifelse(nyc_pred.train$NEIGHBORHOOD == "FASHION", 1,0)
nyc_pred.train$`N_JAVITS CENTER` = ifelse(nyc_pred.train$NEIGHBORHOOD == "JAVITS CENTER", 1,0)
nyc_pred.train$`N_MIDTOWN CBD` = ifelse(nyc_pred.train$NEIGHBORHOOD == "MIDTOWN CBD", 1,0)
nyc_pred.train$`N_OTHERS` = ifelse((nyc_pred.train$NEIGHBORHOOD != "MIDTOWN CBD") &
                                                       (nyc_pred.train$NEIGHBORHOOD != "JAVITS CENTER") &
                                                       (nyc_pred.train$NEIGHBORHOOD != "FASHION") &
                                                       (nyc_pred.train$NEIGHBORHOOD != "BLOOMFIELD"), 1,0)

# Removing the original Neighborhood predictor
nyc_pred.train <- nyc_pred.train[,-2] 


## ----pred_analysis6, message = FALSE, warning = FALSE-----------------------------------------------------------
# 3. Single Factor Regression - BCC
model1 <- lm(nyc_pred.train$`SALE PRICE` ~ nyc_pred.train$`BUILDING CLASS CATEGORY`)
res_bcc <- summary(model1) 

# F-statistic p-value
pf(res_bcc$fstatistic[1],res_bcc$fstatistic[2],res_bcc$fstatistic[3],lower.tail = FALSE)


## ----pred_analysis7, message = FALSE, warning = FALSE-----------------------------------------------------------
# 4. Single Factor Regression - Tax class
model1 <- lm(nyc_pred.train$`SALE PRICE` ~ nyc_pred.train$`TAX CLASS AT TIME OF SALE`)
res_tax <- summary(model1)  

res_tax


## ----pred_analysis8, message = FALSE, warning = FALSE-----------------------------------------------------------
pf(res_tax$fstatistic[1],res_tax$fstatistic[2],res_tax$fstatistic[3],lower.tail = FALSE)


## ----pred_analysis9, message = FALSE, warning = FALSE-----------------------------------------------------------
# 5. Single Factor Regression - Sale Month
model1 <- lm(nyc_pred.train$`SALE PRICE` ~ nyc_pred.train$`SALE MONTH`)
res_month <- summary(model1) 

res_month


## ----pred_analysis10, message = FALSE, warning = FALSE----------------------------------------------------------
  pf(res_month$fstatistic[1],res_month$fstatistic[2],res_month$fstatistic[3],lower.tail = FALSE)


## ----pred_analysis11, message = FALSE, warning = FALSE----------------------------------------------------------
model <- lm(`SALE PRICE` ~ .  -`SALE MONTH`, data = nyc_pred.train)

