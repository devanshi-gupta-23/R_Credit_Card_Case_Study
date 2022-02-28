########################################################
################ Credit Card Study #####################
########################################################


setwd('C:/Users/user/Videos/Data Science 360 course/R/case study R/R - Credit card case study/R case study 2 (Credit card)')
  
########## loading the library ###########
library(dplyr)
########## Importing the data ###############

df.Spend <- read.csv('Spend.csv')
df.Customer <- read.csv('Customer Acqusition.csv')
df.Repayment <- read.csv('Repayment.csv')

df1 <- merge(x=df.Customer,y= df.Spend, by.x = 'Customer',by.y = 'Customer',all = F)

df <- merge(x=df1,y=df.Repayment,by.x = c('Customer','Month'),by.y = c('Customer','Month'),all = F)


#df <- dplyr::rename(df,'Repayment.Amount'='Amount.y','Spend.Amount'='Amount.x')

#df <- dplyr::rename(df,'Spend.Amount'='Amount.x')

############# Loading the Libraries ###########

library(dplyr)
library(lubridate)
library(ggplot2)
require(scales)

################### (1) #######################

#a)
df.Customer[df.Customer$Age<18,] <- mean(df.Customer$Age,na.rm = T)

#b)

df <- dplyr::mutate(df,Spend.Amount=ifelse(Amount.x>Limit,0.5*Limit,Amount.x))

#c)

df <- dplyr::mutate(df,Repayment.Amount=ifelse(Amount.y>Limit,Limit,Amount.y))

# droping old columns #
df$Amount.x <- NULL
df$Amount.y <- NULL

################# converting into the correct datatype ##############33
df$Month <- lubridate::dmy(df$Month)

################### (2) #######################

#a)
nrow(df[!duplicated(df$Customer),]) # Count of distinct cutomers

#b)
nrow(df[!duplicated(df$Product),]) # count of distinct product categories

#c)
avg_monthly_spend_by_cus <- df %>% dplyr::group_by(Customer,month=lubridate::month(Month)) %>% dplyr::summarise(Avg.Spend=mean(Spend.Amount,na.rm=T))
View(avg_monthly_spend_by_cus) # Average monthly spend by customers

#d)
avg_monthly_repayment_by_cus <- df %>% dplyr::group_by(Customer,month=lubridate::month(Month)) %>% dplyr::summarise(Avg.Repayment=mean(Repayment.Amount,na.rm=T))
View(avg_monthly_repayment_by_cus) # Average monthly repayment by customers

#e)
df<- dplyr::mutate(df,Prof=Repayment.Amount-Spend.Amount)
monthly_profit <- df %>% dplyr::group_by(month=lubridate::month(Month)) %>% dplyr::summarise(M_Profit=sum(Prof,na.rm=T))
filter1 <-monthly_profit[monthly_profit$M_Profit>0,] 
Profit_for_each_month <- filter1
Profit_for_each_month <- dplyr::mutate(Profit_for_each_month,Profit=M_Profit*0.029)
Profit_for_each_month$M_Profit <- NULL
View(Profit_for_each_month)## This is the profit for the bank by each month

#f)
agg_prod_type <- df %>% dplyr::group_by(Type) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))
sorted_agg_prod_type <- dplyr::arrange(agg_prod_type,desc(Spend))
View(head(sorted_agg_prod_type,5)) ## Top 5 product types by spend amount

#g)
agg_city_by_spend <- df %>% dplyr::group_by(City) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))
sorted_agg_city_by_spend <- dplyr::arrange(agg_city_by_spend,desc(Spend))
View(head(sorted_agg_city_by_spend,1)) ## City which is having maximum spend 

#h)
agg_age_by_spend <- df %>% dplyr::group_by(Age) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))
sorted_agg_age_by_spend <- dplyr::arrange(agg_age_by_spend,desc(Spend))
View(head(sorted_agg_age_by_spend,1)) ## Age Group which is spending more

#i)
agg_cust <- df %>% dplyr::group_by(Customer) %>% dplyr::summarise(Repayment=sum(Repayment.Amount,na.rm=T))
sorted_agg_cust <- dplyr::arrange(agg_cust,desc(Repayment))
View(head(sorted_agg_cust,10)) ## Top 10 Customer in terms of repayment

################### (3) #######################

agg_year_city_prod <- df %>% dplyr::group_by(year=lubridate::year(Month),City,Product) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))
View(agg_year_city_prod)


p <- ggplot2::ggplot(data = agg_year_city_prod) + 
  aes(x = Product , y = Spend, fill = as.character(City)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_grid(.~year)
p + scale_y_continuous(labels = scales::comma)

################### (4) #######################

#a)
agg_citywise_spend <- df %>% dplyr::group_by(City) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))

p1 <- ggplot2::ggplot(data = agg_citywise_spend) + 
  aes(x = City, y = Spend) +
  geom_bar(stat = 'identity', fill = 'purple', color = 'black')

p1 + scale_y_continuous(labels = scales::comma)

#b)
filter_airticket <- df[df$Type=='AIR TICKET',]
agg_yearwisespend_air_ticket <- filter_airticket %>% dplyr::group_by(year=lubridate::year(Month)) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))

p2 <- ggplot2::ggplot(data = agg_yearwisespend_air_ticket) + 
  aes(x = year, y = Spend) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'black')

p2 + scale_y_continuous(labels = scales::comma)

#c)
agg_monthly_spend_product <- df %>% dplyr::group_by(month=lubridate::month(Month),Product) %>% dplyr::summarise(Spend=sum(Spend.Amount,na.rm=T))
View(agg_monthly_spend_product)

p3 <- ggplot2::ggplot(data = agg_monthly_spend_product) + 
  aes(x = month, y = Spend, fill = Product) + 
  geom_bar(stat = 'identity', position = 'dodge')

p3 + scale_y_continuous(labels = scales::comma)
# It shows that as month is increasing spends are going down.

####################### (5) ########################################

Prod_cat <- readline("Please Enter Product Category and product category should be in Gold/Silver/Platinum: ")
time_period <- readline("Please Enter Time Period and time period should be in yearly/monthly: ")

Top10Cust <- function(Prod_cat,time_period) {
     if(time_period=='yearly'){
       
       filter_by_prod <- df[df$Product==Prod_cat,]
       agg1 <- filter_by_prod %>% dplyr::group_by(Customer,City,Product,year=lubridate::year(Month)) %>% dplyr::summarise(Repayment=sum(Repayment.Amount,na.rm=T))
       sorted_agg1 <- dplyr::arrange(agg1,desc(Repayment))
       Top10 <- head(sorted_agg1[,c('Customer','City','Product','year','Repayment')],10)
       return(Top10)
       
      }else if(time_period=='monthly'){
        
        filter_by_prod1 <- df[df$Product==Prod_cat,]
        agg2 <- filter_by_prod1 %>% dplyr::group_by(Customer,City,Product,month=lubridate::month(Month)) %>% dplyr::summarise(Repayment=sum(Repayment.Amount,na.rm=T))
        sorted_agg2 <- dplyr::arrange(agg2,desc(Repayment))
        Top_10 <- head(sorted_agg2[,c('Customer','City','Product','year','Repayment')],10)
        return(Top_10)
                 
      } else {
           print('Wrong Input')        
      }
   
}
View(Top10Cust(Prod_cat,time_period))
