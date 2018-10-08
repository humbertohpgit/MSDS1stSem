#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readxl")
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# 1st Data set - Calorie Intake Impact on Snack Substitution

## (Fruits & Vegetables Prices/Calories data sets - https://www.ers.usda.gov/data-products/fruit-and-vegetable-prices.aspx)

## Load data set (CSV) into R
#### Download target file locally from github "caloricimpacts (untidy dataset).xls"
setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
calorieimpact_raw <- data.frame(read_excel("caloricimpacts (untidy dataset).xls", skip = 1, col_names = TRUE))
dim(calorieimpact_raw)

## Data Wrangling

### Elminating Headers, Footers and Calculated columns
calorieimpact_raw <- calorieimpact_raw[-(22:26),]
calorieimpact_raw <- calorieimpact_raw[,-23]
calorieimpact_raw <- calorieimpact_raw[-1,]

### Pivoting columns to a key column and Renaming columns
calorieimpact <- gather(calorieimpact_raw, "FruitVeg", "DiffCalories", 3:22)
names(calorieimpact) <- c("Snack", "SnackCalories", "FruitVeg", "DiffCalories")
head(calorieimpact,22)

### Casting data types, creating calculated-columns, arranging column layout
calorieimpact$SnackCalories <- as.numeric(calorieimpact$SnackCalories)
calorieimpact$DiffCalories <- as.numeric(calorieimpact$DiffCalories)
calorieimpact <- mutate(calorieimpact, FruitVegCalories = SnackCalories-DiffCalories)
avgCalChgbyFruitVeg <- summarise(group_by(calorieimpact, FruitVeg), AvgCalChgbyFruitVeg = round(mean(DiffCalories)))
calorieimpact <- calorieimpact %>% inner_join(avgCalChgbyFruitVeg, by = c("FruitVeg"))
calorieimpactfinal <- select(calorieimpact, Snack, SnackCalories, FruitVeg, FruitVegCalories, AvgCalChgbyFruitVeg, DiffCalories)
head(calorieimpactfinal,22)

## Analysis of the calorie intake impact of substituting Snacks with Fruits/Vegetables
library(ggplot2)

ggplot(calorieimpactfinal, aes(x=DiffCalories, y=Snack, colour = FruitVeg)) + geom_point() + facet_wrap(~FruitVeg) + labs(y = "Snack", x = "Differential of Calories Snack vs Fruit/Vegetable", 
        title = "Analysis of the calorie intake impact of substituting Snacks with Fruits/Vegetables") + theme_bw()

###### (Values < 0 mean calorie intake increase due to substitution)

#### Conclusion: With the exception of very few cases like: Sweet Potatoes, Raisins & Apple Sauce, the calorie intake is substantially decreased when substituting Snacks with Fruits/Vegetables

###################################################################

# 2nd Data set - Cost Impact on Snack Substitution

## Load data set (CSV) into R
#### Download target file locally from github "costimpacts (untidy dataset).xls"
setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
costimpact_raw <- data.frame(read_excel("costimpacts (untidy dataset).xls", skip = 1, col_names = TRUE))
dim(costimpact_raw)

## Data Wrangling

### Elminating Headers, Footers and Calculated columns
costimpact_raw <- costimpact_raw[-(22:25),]
costimpact_raw <- costimpact_raw[,-23]
costimpact_raw <- costimpact_raw[-1,]

### Pivoting columns to a key column and Renaming columns
costimpact <- gather(costimpact_raw, "FruitVeg", "DiffCost", 3:22)
names(costimpact) <- c("Snack", "SnackCost", "FruitVeg", "DiffCost")
head(costimpact,22)

### Casting data types, creating calculated-columns, arranging column layout
costimpact$SnackCost <- as.numeric(format(costimpact$SnackCost, nsmall = 2))
costimpact$DiffCost <- as.numeric(format(costimpact$DiffCost, nsmall = 2))
costimpact <- mutate(costimpact, FruitVegCost = SnackCost-DiffCost)
totCostchgbyFruitVeg <- summarise(group_by(costimpact, FruitVeg), TotCostChgbyFruitVeg = sum(DiffCost))
costimpact <- costimpact %>% inner_join(totCostchgbyFruitVeg, by = c("FruitVeg"))
costimpactfinal <- select(costimpact, Snack, SnackCost, FruitVeg, FruitVegCost, TotCostChgbyFruitVeg, DiffCost)
head(costimpactfinal,22)

## Analysis of the cost impact of substituting Snacks with Fruits/Vegetables
library(ggplot2)

ggplot(costimpactfinal, aes(x=DiffCost, y=Snack, colour = FruitVeg)) + geom_point() + facet_wrap(~FruitVeg) + labs(y = "Snack", x = "Differential of Cost Snack vs Fruit/Vegetable ", 
        title = "Analysis of the cost impact of substituting Snacks with Fruits/Vegetables") + theme_bw()
ggplot(costimpactfinal, aes(y = FruitVeg, x = TotCostChgbyFruitVeg)) + geom_point(aes(col=FruitVeg, size=TotCostChgbyFruitVeg)) + labs(y = "Fruit/Vegetable", x = "Total Cost Replacing all Snacks with specific Fruit/Veg", 
        title = "Total Cost of Replacing all Snacks with a specific Fruit/Vegetable ") + theme_bw()

######  (Values < 0 mean cost increase due to subsitution)

#### Conclusion: With the exception of very few cases like: Red Peppers, Tangerines & Tomatoes, the cost is substantially decreased when substituting Snacks with Fruits/Vegetables. Some of the best savings contributors are: Celery, Carrots, Broccoli & Bananas

###################################################################

# 3rd Data set - United Nations Migration Data

## http://www.un.org/en/development/desa/population/migration/data/estimates2/estimates17.shtml

## Load data set (CSV) into R and eliminate headers
#### Download target file locally from github "UN_MigrantStockByOriginAndDestination_2017.xlsx"
setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
migration_raw <- data.frame(read_excel("UN_MigrantStockByOriginAndDestination_2017.xlsx", sheet = "Table 1", skip = 15, col_names = TRUE))
dim(migration_raw)
head(migration_raw,5)

## Data Wrangling

### Elminating irrelevant columns, including totals
migration_raw <- migration_raw[,-2]
migration_raw <- migration_raw[,-(3:6)]

### Pivoting columns to a key column and Renaming columns
migration <- gather(migration_raw, "CountryOrig", "MigrantStock", 3:236)
names(migration) <- c("Year", "CountryDest", "CountryOrig", "MigrantStock")
head(migration,5)

### Casting data types and filtering NAs and certain summary categories like: World, Developed, High Income Regions, Continents & Subcontinents, etc.
str(migration)
migration$MigrantStock <- as.numeric(migration$MigrantStock)
migration_final <- migration %>% filter(!is.na(MigrantStock))
migration_final <- migration_final %>% filter(!str_detect(CountryDest,"regions|countries|WORLD"))
migration_final <- migration_final %>% filter(!str_detect(CountryDest,"ASIA|AFRICA|EUROPE|LATIN|CARIBBEAN|OCEANIA|Central|Eastern|Western|Southern|Northern|NORTHERN|Other|Sub"))
migration_final <- migration_final %>% filter(!str_detect(CountryOrig,"Other"))
head(migration_final,10)

### Preparing datasets for Ranking Top CountryDest and CountryOrigin by MigrationStock
TopCountryDest <- summarise(group_by(migration_final, CountryDest), MigrantStock=sum(MigrantStock)) %>% arrange(desc(MigrantStock)) %>% head(., 20)
TopCountryOrig <- summarise(group_by(migration_final, CountryOrig), MigrantStock=sum(MigrantStock)) %>% arrange(desc(MigrantStock)) %>% head(., 20)

### Preparing dataset for Migration by CountryDest over time
TopCountryDestOT <- summarise(group_by(migration_final, CountryDest, Year), MigrantStock=sum(MigrantStock)) %>% arrange(desc(MigrantStock)) %>% filter(str_detect(CountryDest,"United States of America|Russia|Germany|Saudi|France|United Kingdom|Canada|Australia|India|New Zealand|Ukraine|United Arab|Pakistan|Italy|Spain|Iran|China"))
  
## Analysis of the Migration Patterns
library(ggplot2)

### Most Attractive Countries for Immigrants
ggplot(TopCountryDest, aes(x=reorder(CountryDest, MigrantStock), y=MigrantStock, fill=CountryDest)) + geom_col(position = "dodge") + coord_flip() + labs(y = "Migration Stock", x = "Destination Country", title = "Most Attractive Countries for Immigrants") + theme(axis.text.y=element_blank()) ##+ stat_identity()
### Top Countries by # of Migrants
ggplot(TopCountryOrig, aes(x=reorder(CountryOrig, MigrantStock), y=MigrantStock, fill=CountryOrig)) + geom_col(position = "dodge") + coord_flip() + labs(y = "Migration Stock", x = "Origin Country", title = "Top Countries by # of Migrants") + theme(axis.text.y=element_blank())  ##+ stat_identity()

### MigrationStock over the Years for Top Destination Countries
ggplot(TopCountryDestOT, aes(x=Year, y=MigrantStock, group=CountryDest)) + geom_line(aes(color=CountryDest)) + geom_point(aes(color=CountryDest)) + labs(y = "Migration Stock", x = "Year", title = "MigrationStock over the Years for Top Destination Countries") + theme_bw()  ##+ stat_identity()
#### Faceted View
ggplot(TopCountryDestOT, aes(x=Year, y=MigrantStock, group=CountryDest)) + geom_line(aes(color=CountryDest)) + geom_point(aes(color=CountryDest)) + facet_wrap(~CountryDest) + labs(y = "Migration Stock", x = "Year", title = "MigrationStock over the Years for Top Destination Countries") + theme_bw()  ##+ stat_identity()

#### Conclusion: The United States of America stays as the top destination country for immigrants WW with a linear growth over the last decades. Ukraine, Pakistan, India and Iran experiencing a decline or a flat trend. 
####              Developed countries and economies like: UK, UAE, Canada, Australia, France and Germany, showing and steady growth trend over the last decades. Saudi Arabia showing up with an almost linear growth rate during the last two decades