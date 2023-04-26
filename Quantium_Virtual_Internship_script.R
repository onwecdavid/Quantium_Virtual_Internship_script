### Project for the Category Manager for Chips, who wants to better understand the types of customers who purchase Chips and their purchasing behaviour within the region.

#Load important Libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(corrplot)
library(dplyr)
library(tidyverse)
library(readr)
library(datasets)
library(stringr)
library(dplyr)
library(lubridate)

#Load Files
library(forcats)
filepath <- "C:/Users/Nairabet/OneDrive/Data_Analysis/R_Programming/Projects/Quantium_Virtual_Internship_Retail_Strategy_and_Analytics/Quantium_Virtual_Internship/"
transaction_data <- fread(paste0(filepath, "QVI_transaction_data.csv"))
purchase_behaviour <- fread(paste0(filepath, "QVI_purchase_behaviour.csv"))

#Check transactionData
str(transaction_data)

#### Convert DATE column to a date format
#### A quick search online tells us that CSV and Excel integer dates begin on 30 Dec 1899
transaction_data$DATE <- as.Date(transaction_data$DATE, origin = "1899-12-30")

#Lets check that we are looking at the right products by taking a closer look at PROD_NAME
transaction_data[, .N, by = PROD_NAME]

#### Examine the words in PROD_NAME to see if there are any incorrect entries
#### such as products that are not chips
productWords <- data.table(unlist(strsplit(unique(transaction_data[, PROD_NAME]), " ")))
setnames(productWords, 'words')
####Creating list of products
productInfo <- data.table(PROD_NBR = unique(transaction_data$PROD_NBR),
                          PROD_NAME = unique(toupper(transaction_data$PROD_NAME)))
productInfo <- productInfo %>% arrange(PROD_NBR)

#Removing numbers and extra spaces
#On transaction_data
transaction_data$PROD_NAME = substr(transaction_data$PROD_NAME,1,nchar(transaction_data$PROD_NAME))
transaction_data$PROD_NAME = gsub("\\s+"," ",transaction_data$PROD_NAME)
#and on productInfo
productInfo$PROD_NAME = substr(productInfo$PROD_NAME,1,nchar(productInfo$PROD_NAME))
productInfo$PROD_NAME = gsub("\\s+"," ",productInfo$PROD_NAME)
#check the new data subset
head(productInfo)

#### Remove salsa products
VPN <- productInfo %>% pull(PROD_NAME)
grep("salsa", VPN, ignore.case = TRUE, value = TRUE)

# took the salsas product number: 57, 64, 39, 101, 65, 35, 59, 76, 41. and deleted all salsa products
# from transaction data & list of Products.
transaction_data = filter(transaction_data, PROD_NBR != 57, PROD_NBR != 64, PROD_NBR != 39,
                          PROD_NBR != 101, PROD_NBR != 65, PROD_NBR != 35, PROD_NBR != 59,
                          PROD_NBR != 76, PROD_NBR != 41)
productInfo = filter(productInfo, PROD_NBR != 57, PROD_NBR != 64, PROD_NBR != 39,
                     PROD_NBR != 101, PROD_NBR != 65, PROD_NBR != 35, PROD_NBR != 59,
                     PROD_NBR != 76, PROD_NBR != 41)

#### Summarise the data to check for nulls and possible outliers
summary(transaction_data)

#### Filter the dataset to find the outlier
transaction_data[PROD_QTY == 200, ]

#### Let's see if the customer has had other transactions
which(grepl(226000, transaction_data$LYLTY_CARD_NBR))

#### Filter out the customer based on the loyalty card number
transaction_data = filter(transaction_data, LYLTY_CARD_NBR != 226000) #deleted two transactions.
summary(transaction_data)

#### Count the number of transactions by date
tab <- table(cut(transaction_data$DATE, 'day'))
transactions_by_day <- data.frame(tab)
transactions_by_day <- data.frame(as.Date(transactions_by_day$Var1), transactions_by_day$Freq)
transactions_by_day <- transactions_by_day %>%
  rename( DATE = as.Date.transactions_by_day.Var1., N = transactions_by_day.Freq)
summary(transactions_by_day$N)

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line(color = "deepskyblue3", size = 1) +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.7),
        panel.background = element_rect(fill = "aliceblue"))

#### Filter to December and look at individual days
ggplot(transactions_by_day[transactions_by_day$DATE >= "2018-12-01" & transactions_by_day$DATE <= "2018-12-31", ],
       aes(x = DATE, y = N)) +  geom_line(color = "deepskyblue3", size = 1) +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
productInfo$Brand <- gsub("([A-Za-z]+).*", "\\1", productInfo$PROD_NAME)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
productInfo$PackSize_g <- substrRight(productInfo$PROD_NAME, 4)
productInfo$PackSize_g <- str_sub(productInfo$PackSize_g,1,nchar(productInfo$PackSize_g)-1)
productInfo$PackSize_g <- as.numeric(productInfo$PackSize_g)

#when sorting I found that one value was typed incorrectly so I fixed it, and run the last line of code again
productInfo[58, "PROD_NAME"] <- "Kettle Swt Pot Sea Salt 135g"
productInfo[58, "PackSize_g"] <- "135g"
productInfo[5, "PROD_NAME"] <- "Natural Chip Compny SeaSalt 175g"
productInfo[36, "PROD_NAME"] <- "Smiths Thinly Swt Chli&S/Cream 175G"

#Plot a histogram showing the number of transactions by pack size.
hist(productInfo[, PackSize_g], main = "Histogram of the chips's packaging size", xlab = "Size (g)", border = "black", col = "deepskyblue3")

#Now to create brands, we can use the first word in PROD_NAME to work out the brand name
productInfo[, .N, by = Brand][order(-N)]

#Some of the brand names look like they are of the same brands - such as RED and RRD, which are both Red Rock Deli chips. Let's combine these together.
#### Clean brand names
productInfo[Brand == "RED", Brand :="RRD"]
productInfo[Brand == "SNBTS", Brand :="SUNBITES"]
productInfo[Brand == "INFZNS", Brand :="INFUZIONS"]
productInfo[Brand == "WW", Brand :="WOOLWORTHS"]
productInfo[Brand == "SMITH", Brand :="SMITHS"]
productInfo[Brand == "NCC", Brand :="NATURAL"]
productInfo[Brand == "DORITO", Brand :="DORITOS"]
productInfo[Brand == "GRAIN", Brand :="GRNWVES"]
productInfo[, .N, by = Brand][order(-N)]

#additional brand adjustments you think may be required.
transaction_data$Brand <- toupper(gsub("([A-Za-z]+).*", "\\1", transaction_data$PROD_NAME))
transaction_data$PackSize_g <- substrRight(transaction_data$PROD_NAME, 4)
transaction_data$PackSize_g <- str_sub(transaction_data$PackSize_g,1,nchar(transaction_data$PackSize_g)-1)
transaction_data$PackSize_g <- as.numeric(transaction_data$PackSize_g)

#Lets also clean brand name on the transaction_data data frame.
transaction_data[Brand == "RED", Brand :="RRD"]
transaction_data[Brand == "SNBTS", Brand :="SUNBITES"]
transaction_data[Brand == "INFZNS", Brand :="INFUZIONS"]
transaction_data[Brand == "WW", Brand :="WOOLWORTHS"]
transaction_data[Brand == "SMITH", Brand :="SMITHS"]
transaction_data[Brand == "NCC", Brand :="NATURAL"]
transaction_data[Brand == "DORITO", Brand :="DORITOS"]
transaction_data[Brand == "GRAIN", Brand :="GRNWVES"]
transaction_data[, .N, by = Brand][order(-N)]

#We added data from ‘productInfo’ into the transaction data and cleaned the data. now I will reorder colnames.
colnames(transaction_data)
Transaction_Data <- transaction_data[, c(1, 2, 3, 4, 5, 6, 9, 10, 7, 8)]


#### Examining customer data
str(purchase_behaviour)

#purchase behavior by age
purchase_behaviour[, .N, by = LIFESTAGE][order(N)]

#Check with plot
ggplot(purchase_behaviour[, .N, by = LIFESTAGE][order(N)], aes(x=LIFESTAGE, y=N)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Life Stage", y = "Number of customers", title = "Number of Customers on each Life Stage") +
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

#purchase behavior by premium customers
purchase_behaviour[, .N, by = PREMIUM_CUSTOMER][order(N)]

#Check with plot
ggplot(purchase_behaviour[, .N, by = PREMIUM_CUSTOMER][order(N)], aes(x=PREMIUM_CUSTOMER, y=N)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Budged type", y = "Number of customers", title = "Number of Customers on each budged type")+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

#As there do not seem to be any issues with the customer data, we can now go ahead and join the transaction and customer data sets together
data <- merge(transaction_data, purchase_behaviour, all.x = TRUE)

#As the number of rows in data is the same as that of transaction_data, we can be sure that no duplicates were created. This is because we created data by setting all.x = TRUE (in other words, a left join) which means take all the rows in transaction_data and find rows with matching values in shared columns and then joining the details in these rows to the x or the first mentioned table.
#Let’s also check if some customers were not matched on by checking for nulls.
data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]

#Great, there are no nulls! So all our customers in the transaction data has been accounted for in the customer dataset.

#####Data exploration is now complete!####

#####Data analysis on customer segments#####

#Let’s start with calculating total sales by LIFESTAGE and PREMIUM_CUSTOMER and plotting the split by these segments to describe which customer segment contribute most to chip sales.
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
#### Create plot
p <- ggplot(data = sales) +
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of
 sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))

###Sales are coming mainly from Budget - older families, Mainstream - young singles/couples, and Mainstream - retirees Let’s see if the higher sales are due to there being more customers who buy chips.

#### Number of Customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]
#### Create Plot
p <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of customers
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =
                                                      (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,
                                                                                                  '%'))))
#There are more Mainstream - young singles/couples and Mainstream - retirees who buy chips. This contributes to there being more sales to these customer segments but this is not a major driver for the Budget - Older families segment.
#Higher sales may also be driven by more units of chips being bought per customer.

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##Older families and young families in general buy more chips per customer

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##Mainstream midage and young singles and couples are more willing to pay more per packet of chips compared to their budget and premium counterparts. This may be due to premium shoppers being more likely to buy healthy snacks and when they buy chips, this is mainly for entertainment purposes rather than their own consumption. This is also supported by there being fewer premium midage and young singles and couples buying chips compared to their mainstream counterparts.

#As the difference in average price per unit is not large, we can check if this difference is statistically significant. To do so, I will perform an independent t-test between mainstream vs premium midage young singles and couplesto see if thedifference is significant.Our data will yield revelant results about the statistical significance of the price diference. we have all the data which uses ordinal scale as measurement applied to the data, if we plot price data it result on a normal distribution, we can rely on the results with reasonable assurance.
#Histogram of PRice per Unit
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
hist(pricePerUnit[, price], main = "Histogram of the price per unit",
     xlab = "Price", border = "black", col = "deepskyblue3")

#### Perform an independent t‐test between mainstream vs premium and budget midage and
#### young singles and couples
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
              & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")

###The t-test results in a p-value < 2.2e-16, i.e. the unit price for mainstream, young and mid-age singles and couples are significantly higher than that of budget or premium, young and midage singles and couples.

## Deep dive into specific customer segments for insights

#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]
#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = Brand]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = Brand]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

#We can see that :
  
#  • Mainstream young singles/couples are 23% more likely to purchase Tyrrells chips compared to the rest of the population.

#  • Mainstream young singles/couples are 56% less likely to purchase Burger Rings compared to the rest of the population

####Let’s also find out if our target segment tends to buy larger packs of chips.

#### Preferred pack size compared to the rest of the population
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PackSize_g]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PackSize_g]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

#It looks like Mainstream young singles/couples are 27% more likely to purchase a 270g pack of chips compared to the rest of the population but let’s dive into what brands sell this pack size.
data[PackSize_g == 270, unique(PROD_NAME)]

##Twisties are the only brand offering 270g packs and so this may instead be reflecting a higher likelihood of purchasing Twisties.



############## Let’s recap what we’ve found! ################

#####The sales of our chips have been primarily driven by three customer segments: Budget - older families, Mainstream - young singles/couples, and Mainstream - retirees shoppers. We observed that the high spending on chips by Mainstream - young singles/couples and retirees is due to their higher numbers compared to other buyer groups. Furthermore, Mainstream - midage and young singles/couples are more likely to pay a higher price per packet of chips, indicating impulsive buying behavior.

##Our analysis also revealed that Mainstream - young singles/couples are 23% more likely to buy Tyrrells chips compared to the rest of the population. To enhance the category's performance, we suggest that the Category Manager considers placing Tyrrells and smaller packs of chips in discretionary spaces near areas frequented by young singles and couples to increase visibility and encourage impulsive buying behavior.