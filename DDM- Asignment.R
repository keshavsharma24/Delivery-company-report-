#installing libraries
#install.packages("ggplot2")
#install.packages("dpylr")
#install.packages("readxl")
#install.packages("RColorBrewer")
#install.packages("skimr")
#install.packages("gridExtra")
#install.packages("scales")

#loading libraries 
library(ggplot2)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(skimr)
library(gridExtra)
library(scales)

#setting working directory
setwd("D:/MSC Business analytics/DDM/Assignment")

#reading parcel and driver data from excel
parcel <- read_excel("parcel_48.xlsx")
driver <- read_excel("driver_48.xlsx")

#Cleaning NULL and invalid values
parcel_without_NA <- na.omit(parcel)
parcel_without_NA <- filter(parcel_without_NA, parcel_returned==0 | parcel_returned==1)
parcel_without_NA <- filter(parcel_without_NA,parcel_status %in% c("delivered", "lost" ,"returned to warehouse"))
parcel_without_NA <- filter(parcel_without_NA,time_of_delivery %in% c("afternoon", "evening" ,"morning"))

#calculation for pie chart
ps <- parcel_without_NA %>% group_by(parcel_status) %>% summarise(parcel_status_count= n(), parcel_sum_value=sum(parcel_value))
ps <-ps %>% mutate(perc=parcel_status_count/ (sum(parcel_status_count))) %>% mutate(labels = scales::percent(perc))

#Calculation for finding mean delivery time according to delivery location
delivery_time_difference <-parcel_without_NA %>% group_by(delivery_location,priority_delivery) %>% summarise(parcel_arrived_day = round(mean(parcel_arrived),2))

#Only parcels returned to warehouse
pc1 <- filter(parcel_without_NA, parcel_status=="returned to warehouse")
pc1 <- mutate(pc1, parcel_return_case=case_when(parcel_returned==0 ~"Not Returned by Customer",parcel_returned==1 ~"Returned by Customer" ))
pc1 <- mutate(pc1, parcel_arrived_day=case_when(parcel_arrived==0 ~"Same Day",parcel_arrived==1 ~"First Day", parcel_arrived==2 ~"Second Day", parcel_arrived==3 ~"Third Day", parcel_arrived==4 ~"Fourth Day", parcel_arrived==5 ~"Fifth Day"))

#Only parcels returned to warehouse and lost
pc2 <- filter(parcel_without_NA, parcel_status=="returned to warehouse" | parcel_status == "lost")
pc2 <- mutate(pc2, parcel_return_case=case_when(parcel_status== "returned to warehouse" & parcel_returned==0 ~"Returned before delivery", parcel_status== "returned to warehouse" & parcel_returned==1 ~"Returned by Customer" ,parcel_status== "lost" & parcel_returned==1 ~ "Lost During Return",parcel_status== "lost" & parcel_returned==0 ~"Lost During Delivery " ))

#dividing price into segments 
pv <- parcel_without_NA %>% mutate(values = case_when(parcel_value > 0 & parcel_value <= 10 ~ "0-10£",parcel_value > 10 & parcel_value <=20 ~ "10-20£",parcel_value > 20 & parcel_value <=30 ~ "20-30£",parcel_value > 30 & parcel_value <=40 ~ "30-40£",parcel_value > 40 & parcel_value <=50 ~ "40-50£",parcel_value > 50 & parcel_value <=60 ~ "50-60£",parcel_value > 60 & parcel_value <=70 ~ "60-70£",parcel_value > 70 & parcel_value <=80 ~ "70-80£",parcel_value > 80 & parcel_value <=90 ~ "80-90£",parcel_value > 90 & parcel_value <=100 ~ "90-100£", parcel_value > 100 & parcel_value <=110 ~ "100-110£", parcel_value > 110 & parcel_value <=120 ~ "110-120£", parcel_value > 120 & parcel_value <=130 ~ "120-130£", parcel_value > 130 & parcel_value <=140 ~ "130-140£", parcel_value > 140 & parcel_value <=150 ~ "140-150£", parcel_value > 150 & parcel_value <=160~ "150-160£", parcel_value > 160 & parcel_value <=170 ~ "160£-above"))

#transforming data type of driver_id variable to character variable
parcel$driver_id<- as.character(parcel$driver_id)

#Joining Parcel and Driver data
p_d <- left_join(parcel_without_NA, driver, by="driver_id")

#Calculation for count of deliver according to exprience and work pattern
p_d2 <- p_d %>% group_by(experience, work_pattern) %>% summarise(driver_dilvery_count= n())

################################Graphs#######################################################
#Graph 1: Parcel Value Distribution
ggplot(parcel_without_NA, aes(x=parcel_value))+geom_histogram(bins=15, fill="#FF6347")+labs(title = "Parcel Value Distribution", x="Parcel Value (£)", y="Parcel Count") 

#Graph 2 : Percentage of each parcel status
ggplot(ps, aes(x="",y = parcel_status_count, fill = parcel_status)) + geom_bar(stat = "identity", width =1, color = "white") + coord_polar("y", start = 0) +geom_text(aes(label=paste(labels, "\n","£" ,round(parcel_sum_value, 0))),position = position_stack(vjust=0.5)) + theme_void() + labs(title = "Percentage(%) Distribution Of Parcel Status")  + scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Parcel Status", labels=c("Delivered", "Lost", "Returned to Warehouse"))

# Graph 3: Location wise split of parcels status
ggplot(parcel_without_NA, aes(x=delivery_location, fill=parcel_status)) + geom_bar(position = "stack") + scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Parcel Status", labels=c("Delivered", "Lost", "Returned to Warehouse")) + labs(title = "Location wise split of Parcels Status", x="Delivery Location", y="Parcel Count")

#Graph 4 : distribution of parcel value by parcel status
ggplot(pv, aes(y=factor(values, level=c("0-10£", "10-20£", "20-30£", "30-40£", "40-50£", "50-60£", "60-70£", "70-80£", "80-90£", "90-100£", "100-110£", "110-120£", "120-130£", "130-140£", "140-150£", "150-160£", "160£-above")) ,fill=parcel_status)) + geom_bar(position="fill") + scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347", "green"), name="Parcel Status") + labs(title = "Price Distribution of Parcels By Parcel Status", y="Price Of Parcel", x="Parcel Count Ratio")

#Graph 5: Average Delivery Time According To Location And Priority Status 
ggplot(delivery_time_difference,aes(y =delivery_location,parcel_arrived_day,fill = priority_delivery )) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Average Delivery Time According To Location And Priority Status", x= "Average Delivery Time(in days)", y="Delivery location") +scale_fill_manual(values = c("#63c5da","#FF6347"), name="Priority Status", labels=c("No", "Yes")) + theme(legend.position = "bottom")

#Graph 6: Distribution Of Parcels Location By Time Of Delivery
ggplot(pc2, aes(x=parcel_return_case, fill=time_of_delivery)) + geom_bar(position = "stack")  +scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Time of Delivery", labels=c("Afternoon", "Evening", "Morning")) + labs(title = "Distribution Of Parcel Lost/ Retuned to Warehouse By Time Of Delivery", x="Parcel Lost/ Retuned to Warehouse", y="Parcel Count")

#Graph 7: Distribution of parcel returned to warehouse by delivery location
ggplot(pc2, aes(x=parcel_return_case)) + geom_bar(fill="#FF6347") + geom_label(stat = "count", aes(label = ..count..), size = 4) + theme( axis.title.y  = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())  + labs(title = "Distribution Of Parcel Lost/ Returned To Warehouse (New Terms)", x="Parcel Lost/ Retuned to Warehouse")

#Graph 8: Distribution of parcel status by parcel arrived day
ggplot(pc1, aes( y=factor(parcel_arrived_day,level=c("Same Day", "First Day", "Second Day", "Third Day", "Fourth Day", "Fifth Day")),  fill=parcel_return_case)) + geom_bar() +theme( legend.position="bottom", axis.title.y  = element_blank()) +scale_fill_manual(values = c("#63c5da","#FF6347"), name="", labels=c("Not Returned by Customer", "Returned by Customer")) +  geom_label(stat = "count", aes(label = ..count..), size = 4) +labs(title = "Distribution Of Parcel Returned To Warehouse By Parcel Arrival Day", x="Parcel Retuned to Warehouse")

#Graph 9: Distribution of parcel returned to warehouse by parcel payment
ggplot(pc1, aes(fill=delivery_location, x=parcel_payment)) + geom_bar( position = "stack") + geom_label(stat = "count", aes(label = ..count..), size = 4) + theme( axis.title.y  = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())  + labs(title = "Payment Method Used for Parcel Payment Bifurcated by Delivery Location", x="Payment Method") +scale_fill_manual(values = c("#63c5da","#FF6347"), name="Delivery Location")

#Graph 10: Driver's delivery count according to experience  and work pattern
ggplot(p_d2,aes(x =experience,y=driver_dilvery_count, col=work_pattern )) + coord_flip() +geom_segment(aes(x=experience, xend=experience, y=0, yend=driver_dilvery_count))+ geom_point() + labs(title="Driver's delivery count according to experience  and work pattern", y="Driver's Delivery Count", x="Experience") +scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name= "Work Pattern")


###############################################       Grid     #################################################################

#g1 <- ggplot(ps, aes(x="",y = parcel_status_count, fill = parcel_status)) + geom_bar(stat = "identity", width =1, color = "white") + coord_polar("y", start = 0) +geom_text(aes(label=paste(labels, "\n","£" ,round(parcel_sum_value, 0))),position = position_stack(vjust=0.5)) + theme_void() + labs(title = "Percentage(%) Distribution Of Parcel Status")  + scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Parcel Status", labels=c("Delivered", "Lost", "Returned to Warehouse"))
#g2 <- ggplot(parcel_without_NA, aes(x=delivery_location, fill=parcel_status)) + geom_bar(position = "stack") + scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Parcel Status", labels=c("Delivered", "Lost", "Returned to Warehouse")) + labs(title = "Location wise split of Parcels Status", x="Delivery Location", y="Parcel Count")
#g3 <- ggplot(delivery_time_difference,aes(y =delivery_location,parcel_arrived_day,fill = priority_delivery )) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Average Delivery Time According To Location And Priority Status", x= "Average Delivery Time(in days)", y="Delivery location") +scale_fill_manual(values = c("#63c5da","#FF6347"), name="Priority Status", labels=c("No", "Yes")) + theme(legend.position = "bottom")
#g4 <- ggplot(pc2, aes(x=parcel_return_case, fill=time_of_delivery)) + geom_bar(position = "stack")  +scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name="Time of Delivery", labels=c("Afternoon", "Evening", "Morning")) + labs(title = "Distribution Of Parcel Lost/ Retuned to Warehouse By Time Of Delivery", x="Parcel Lost/ Retuned to Warehouse", y="Parcel Count")
#g5 <- ggplot(pc2, aes(x=parcel_return_case)) + geom_bar(fill="#FF6347") + geom_label(stat = "count", aes(label = ..count..), size = 4) + theme( axis.title.y  = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())  + labs(title = "Distribution Of Parcel Lost/ Returned To Warehouse (New Terms)", x="Parcel Lost/ Retuned to Warehouse")
#g6 <- ggplot(p_d2,aes(x =experience,y=driver_dilvery_count, col=work_pattern )) + coord_flip() +geom_segment(aes(x=experience, xend=experience, y=0, yend=driver_dilvery_count))+ geom_point() + labs(title="Driver's delivery count according to experience  and work pattern", y="Driver's Delivery Count", x="Experience") +scale_fill_manual(values = c("#63c5da","#FFD300","#FF6347"), name= "Work Pattern")

#grid.arrange(g1,g2,g3,g4,g5,g6)