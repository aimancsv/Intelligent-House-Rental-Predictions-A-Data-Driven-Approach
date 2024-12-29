## aimancsv@gmail.com

## load libraries
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(corrplot)


## Import the data 
RentalDataset = read.csv("D:\\Desktop\\House_Rent_Dataset.csv" 
                   , header = TRUE)

# View Data Structure type
class(RentalDataset$Published_date)

## Data Pre-processing/cleaning/transformation

# Assign header names
names(RentalDataset)=c("Published_date","BHK","Price",
                       "Square_Feet","Floor_Situated",
                       "Area_Class","Location","City",
                       "Furnishing_Types","Renter_Preferred",
                       "Bathroom","Contact_Type")


# Split Floor_Situated column into Floor_Number and Total_Floor
RentalDataset[c('Floor_Number', 'Total_Floor')] <- 
  str_split_fixed(RentalDataset$Floor, ' out of ', 2)

# Rearrange columns and remove original Floor_Situated column
RentalDataset <- RentalDataset[c('Published_date', 'BHK',
                                 'Price', 'Square_Feet',
                                 'Floor_Number', 'Total_Floor',
                                 'Area_Class', 'Location', 'City',
                                 'Furnishing_Types', 'Renter_Preferred',
                                 'Bathroom', 'Contact_Type')]

# Replace the value Ground with '0'
RentalDataset[RentalDataset == "Ground"] <- 0

#View specific columns
#v = RentalDataset[c("Published_date","Floor_Number")]
#view(v)

# Check data type
#str(v)

#Convert Floor_Number and Total_Floor Columns to Numeric format
RentalDataset$Floor_Number = as.numeric(as.character
                                        (RentalDataset$Floor_Number))
RentalDataset$Total_Floor = as.numeric(as.character
                                       (RentalDataset$Total_Floor))


#Convert Published_date Column to Date/Time format
RentalDataset$Published_date <- mdy(RentalDataset$Published_date)


# Checking missing values
sapply(RentalDataset, function(x) sum(is.na(x)))

# Drop rows with missing values
RentalDataset <- na.omit(RentalDataset)


# Return the first 10 rows in the Dataset
#firstTenRows = head(RentalDataset,10)

# Return the last 10 rows in the Dataset
#LastTenRows = tail(RentalDataset,10)

# View the Dataset in a table form
#View(RentalDataset)

# Dimension of Data
#dim(RentalDataset)


# View the number of length, column and rows of the Dataset
#length(RentalDataset)
#ncol(RentalDataset)
#nrow(RentalDataset)


# Check Outlier (data that are lie away from the other points)
boxplot(RentalDataset$Price, main = "Price Outlier")
boxplot(RentalDataset$Square_Feet, main = "Square Feet Outlier")
boxplot(RentalDataset$Floor_Number, main = "Floor Number Outlier")
boxplot(RentalDataset$Total_Floor, main = "Total Floor Outlier")
boxplot(RentalDataset$BHK, main = "BHK Outlier")

# Outlier Implementation
# Price Outlier
lowwerPrice <- quantile(RentalDataset$Price, 0.010)
upperPrice <- quantile(RentalDataset$Price, 0.990)
RentalDataset <- subset(RentalDataset, RentalDataset$Price >
                          lowwerPrice & RentalDataset$Price < upperPrice)

# Square_Feet Outlier
lowwerSquare_Feet <- quantile(RentalDataset$Square_Feet, 0.010)
upperSquare_Feet <- quantile(RentalDataset$Square_Feet, 0.990)
RentalDataset <- subset(RentalDataset, RentalDataset$
                          Square_Feet > lowwerSquare_Feet &
                          RentalDataset$Square_Feet < upperSquare_Feet)

# Floor_Number Outlier
lowwerFloor_Number <- quantile(RentalDataset$Floor_Number, 1)
UpperFloor_Number <- quantile(RentalDataset$Floor_Number, 0.990)
RentalDataset <- subset(RentalDataset, RentalDataset$Floor_Number <
                          lowwerFloor_Number & RentalDataset$Floor_Number <
                          UpperFloor_Number)

# Correlation
cor_matrix<-cor(RentalDataset[,c(2,3,4)])
str(RentalDataset)
cor_matrix

summary(RentalDataset)


# Question 1: Renting houses on monthly basis
# Analysis 1.1 , 1.2 , 1.3 : Count of Bachelors/Families rented by month

anal1_1 = subset(RentalDataset, select = c("Published_date","Renter_Preferred" ))
anal1_1$Published_date <- format(as.POSIXct.Date(anal1_1$Published_date,
                                       format="%m/%d/%Y"),"%b")
anal1_1 %>%
  # mutate(Renter_Preferred = recode(Renter_Preferred,
  #                                  "Bachelors/Family" = "Family"))%>%
  # mutate(Renter_Preferred = recode(Renter_Preferred,
  #                                  "Bachelors/Family" = "Bachelors"))%>%
  # filter(Renter_Preferred %in% ("Bachelors"))%>%
  ggplot(aes(x= factor(Published_date, levels = c("Apr","May", "Jun","Jul")),
             fill = Renter_Preferred))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values=c('blue', "green","red"))+
  labs(title = "Number of Bachelors/Family rented monthly",
       x = "Months",
       y = "Number of houses rented",
       fill = "Renter type",
       )

# Analysis 1.4 : Average price monthly

anal1_4 <- subset(RentalDataset,
                 select = c("Published_date","Price" ))
  subset(RentalDataset, select = c("Published_date","Price" ))%>%
  group_by(Published_date = month(as.POSIXlt(Published_date,                                                       format="%d/%m/%Y")))%>%
  summarise(Count=mean(Price))%>%
  tail(4) %>%
  ggplot(aes(x= Published_date , y = Count))+
  geom_line(color="#69b3a2", size=1, alpha=0.9) +
  geom_point(shape=21, color="red", fill="#69b3a2", size=4) +
  theme_ipsum() +
    labs(title = "Average price monthly",
         x = "Month",
         y = "Average")  

  
# Analysis 1.5 :  What is the average rent by Furnishing Types?
  
anal1_5<-RentalDataset%>%
  group_by(Furnishing_Types)%>%
  summarise(Avg_rent = mean(Price))%>%
  ggplot(aes(Furnishing_Types, Avg_rent, fill = Avg_rent))+
  geom_bar(stat='Identity') +
  geom_text(aes(label = signif(Avg_rent)),vjust = 1.5, color = "white")+
  labs(title='Average Rent by Furnishing Types', y = ' Average Rent')+
  theme_bw()
  

anal1_5

# Analysis 1.6: Number of houses rented monthly by furnishing types
anal1_6 = subset(RentalDataset, select = c("Published_date","Furnishing_Types" ))
anal1_6$Published_date <- format(as.POSIXct.Date(anal1_6$Published_date,
                                                 format="%m/%d/%Y"),"%b")
anal1_6 %>%
  ggplot(aes(x= factor(Published_date, levels = c("Apr","May", "Jun","Jul")),
             fill = Furnishing_Types))+
  geom_bar(position = "dodge", alpha = 0.5)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  
  scale_fill_manual(values=c('brown', "gray","black"))+
  labs(title = "Number of houses rented monthly by furnishing types",
       x = "Months",
       y = "Count",
       fill = "Furnishing Types")

# Analysis 1.7: When renters usually rent from owners

anal1_7 <- RentalDataset
anal1_7 %>%
  group_by(Published_date = format(Published_date, "%B"),Contact_Type )%>%
  summarise(Count = length(Contact_Type))%>%
  subset( Contact_Type != "Contact Builder")%>%
  view()%>%
  ggplot(aes( x= factor(Published_date, levels = c("April","May", "June","July")),
              y=Count, fill=Contact_Type)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Renters Contact Type") +
  theme_ipsum() +
  xlab("Month")

anal1_7

# Analysis 1.8: Number of BHK in Carpet Area

anal1_8 <- RentalDataset
anal1_8 %>%
  subset(Area_Class == "Carpet Area")%>%
  group_by(Published_date = format(Published_date, "%B"),BHK, Area_Class)%>%
  summarise(Count = length(Contact_Type))%>%
  ggplot(aes( x= BHK,
              y=Count,fill = Area_Class)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~factor(Published_date, levels = c("April","May", "June","July"))) +
  ggtitle("Number of BHK in Carpet Area") +
  theme_ipsum() +
  scale_fill_viridis(discrete = T) +
  xlim(1:6,main='')+
  xlab("BHK")

anal1_8

# Analysis 1.9: Which month has the cheapest price and most BHK in the Super Area

anal1_9 <- RentalDataset
anal1_9 %>%
  subset(Area_Class == "Super Area")%>%
  group_by(Published_date = format(Published_date, "%B"),BHK, Area_Class)%>%
  summarise(Avg_Price = mean(Price))%>%
  view()%>%
  ggplot(aes( x= BHK,
              y=Avg_Price,fill = Area_Class)) + 
  geom_point(color = "white", size = 3) +
  facet_wrap(~factor(Published_date, levels = c("April","May", "June","July"))) +
  ggtitle("Avarege price and most BHK in the Super Area") +
  theme_dark() +
  scale_fill_viridis(discrete = T) +
  xlim(1:6,main='')+
  xlab("BHK")

# Analysis 1.10: When cities has the cheapest price and three bathrooms in the Carpet Area


anal1_10 <- RentalDataset
anal1_10 %>%
  subset(Area_Class == "Carpet Area")%>%
  subset(Bathroom >= 3)%>%
  group_by(Published_date = format(Published_date, "%B"),
           Bathroom, Area_Class, City)%>%
  summarise(Avg_Price = mean(Price))%>%
  view()%>%
  ggplot(aes( x=Bathroom ,
              y=Avg_Price, fill = City)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~factor(Published_date, levels = c("April","May", "June","July"))) +
  ggtitle("Avarege price and more than three bathrooms in Carpet Areas each month") +
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  xlab("Bathroom")

# Analysis 1.11: Conclusion
anal1_11 = RentalDataset
anal1_11 = RentalDataset %>%
  group_by(Published_date = format(Published_date, "%B"),BHK,Price,Area_Class,
                                   Furnishing_Types)%>%
  summarise(Avg_Price = mean(Price))
  ggplot (anal1_11,aes(BHK,Avg_Price,size =Area_Class, color =Furnishing_Types ))+
  geom_point()+
  facet_wrap (~factor(Published_date, levels = c("April","May", "June","July")))+
  labs(title = "Month described by Price and other factors",
       x = "BHK",
       y = "Avarage Price")




# Question 2.0:  Distribution of what Bachelors/Families prefer 
# Analysis 2.1:  What Class Area do Bachelors and families prefer?
anal2_1 = RentalDataset %>%
  group_by(Area_Class)%>%
  filter(Renter_Preferred %in% c("Family"))%>%
  count()%>%
  ungroup()%>%
  mutate(Percentage = `n`/sum(`n`))%>%
  arrange(desc(Area_Class))

lab = scales::percent(anal2_1$Percentage)
ggplot(data = anal2_1)+
  geom_bar(aes("",Percentage,fill= Area_Class),stat = "identity")+
  coord_polar("y",start = 0)+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "What Area Class do Family prefer?",
       fill = "Area Class")

# Analysis 2.2:  What City do Bachelors and families prefer?
anal2_2 = RentalDataset %>%
  group_by(City)%>%
  filter(Renter_Preferred %in% c("Family"))%>%
  count()%>%
  ungroup()%>%
  mutate(Percentage = `n`/sum(`n`))%>%
  arrange(desc(City))

lab = scales::percent(anal2_2$Percentage)
ggplot(data = anal2_2)+
  geom_bar(aes("",Percentage,fill= City),stat = "identity")+
  coord_polar("y",start = 0)+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "What City do Families prefer?",
       fill = "City")+
  scale_fill_manual(values=c("#fff5ef", "#c1d0d7", "#a9ba39",
                             "#63985c",  "#3b6f6c", "#499fa5"))

# Analysis 2.3:  What Contact Types do Bachelors/Families prefer?
anal2_3 = RentalDataset %>%
  group_by(Contact_Type)%>%
  filter(Renter_Preferred %in% c("Family"))%>%
  count()%>%
  ungroup()%>%
  mutate(Percentage = `n`/sum(`n`))%>%
  arrange(desc(Contact_Type))
lab = scales::percent(anal2_3$Percentage)
ggplot(data = anal2_3)+
  geom_bar(aes("",Percentage,fill= Contact_Type),stat = "identity")+
  coord_polar("y",start = 0)+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "What Contact Type do Families prefer?",
       fill = "Contact Type")+
  scale_fill_manual(values=c("#fff5ef", "#c1d0d7"))


# Analysis 2.4:What Furnishing Status do renters prefer if it was Super Area
anal2_4 = RentalDataset %>%
  subset(Area_Class %in% c("Super Area"))%>%
  group_by(Furnishing_Types,Area_Class)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(Furnishing_Types))
lab = scales::percent(anal2_4$Percentage)
ggplot(data = anal2_4)+
  geom_bar(aes("",Percentage,fill= Furnishing_Types),stat = "identity")+
  coord_polar("y",start = 0)+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Super Area ",
       fill = "Furnishing Status")+
  scale_fill_manual(values=c("#fff5ef", "#c1d0d7", "blue"))

# Analysis 2.5: What City tenants prefer if they want a Carpet area?
anal2_5 = RentalDataset %>%
  subset(Area_Class %in% c("Carpet Area"))%>%
  group_by(City ,Area_Class)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(City))
lab = scales::percent(anal2_5$Percentage)
ggplot(data = anal2_5)+
  geom_bar(aes("",Percentage,fill= City),stat = "identity")+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Best city for renters for Carpet Area ",
       fill = "City")+
  scale_fill_manual(values=c("#49678D", "#ED30C7", "#4A192C",
                             "#9D9101", "#F3A505", "#FF2301"))

# Analysis 2.6: What type of Point of Contact renters prefer if the house is furnished
anal2_6 = RentalDataset %>%
  subset(Furnishing_Types %in% c("Furnished"))%>%
  group_by(Furnishing_Types ,Contact_Type)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(Contact_Type))
lab = scales::percent(anal2_6$Percentage)
ggplot(data = anal2_6)+
  geom_bar(aes("",Percentage,fill= Contact_Type),stat = "identity")+
  theme_void()+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Point of Contact if the house is furnished ",
       fill = "Furnished Type")+
  scale_fill_manual(values=c("#2E0182", "#7AAFC3", "#63935A"))

# Analysis 2.7 What type of renters prefer contacting to owners if the is unfurnished 
anal2_7 = RentalDataset %>%
  subset(Furnishing_Types %in% c("Unfurnished"))%>%
  subset(Contact_Type %in% c("Contact Owner"))%>%
  group_by(Furnishing_Types ,Contact_Type,Renter_Preferred)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(Renter_Preferred))
lab = scales::percent(anal2_7$Percentage)
ggplot(data = anal2_7)+
  geom_bar(aes("",Percentage,fill= Renter_Preferred),stat = "identity")+
  theme_void()+
  coord_polar("y",start = 0)+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Unfurnished houses by Owners",
       fill = "Renter Preferred")+
  scale_fill_manual(values=c("#fff5ef", "#c1d0d7", "blue"))

# Analysis 2.8 Analysis: What cities has houses for bachelors to with a Super area and 2 BHKs and 1 bathroom
anal2_8 = RentalDataset %>%
  subset(Area_Class %in% c("Super Area"))%>%
  subset(BHK == 2)%>%
  subset(Bathroom == 1)%>%
  group_by(Area_Class ,BHK,Bathroom,City)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(City))
lab = scales::percent(anal2_8$Percentage)
ggplot(data = anal2_8)+
  geom_bar(aes("",Percentage,fill= City),stat = "identity")+
  theme_void()+
  coord_polar("y",start = 0)+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Super Area Houses with 2 BHKs and 1 bathroom",
       fill = "City")+
  scale_fill_manual(values=c("#fff5ef", "#c1d0d7", "#a9ba39",
                             "#63985c",  "#3b6f6c", "#499fa5"))
                             
# Analysis 2.9 Cities with 3 BHKs, 2 bathrooms, Super Area and through owner
anal2_9 = RentalDataset %>%
  subset(BHK == 4)%>%
  subset(Bathroom == 2)%>%
  subset(Area_Class == c("Super Area"))%>%
  subset(Contact_Type == "Contact Owner")%>%
  group_by(Area_Class ,BHK,Bathroom,City,Contact_Type)%>%
  count( name= "Count" )%>%
  view()%>%
  ungroup()%>%
  mutate(Percentage = `Count`/sum(`Count`))%>%
  arrange(desc(City))
lab = scales::percent(anal2_9$Percentage)
ggplot(data = anal2_9)+
  geom_bar(aes("",Percentage,fill= City),stat = "identity")+
  theme_void()+
  coord_polar("y",start = 0)+
  geom_text(aes(x=1,y=cumsum(Percentage)-Percentage/2,label = lab))+
  labs(title = "Super Area Houses with 3 BHKs and 2 bathroom through owners",
       fill = "City")+
  scale_fill_manual(values=c("#49678D", "#ED30C7", "#4A192C",
                             "#9D9101", "#F3A505", "#FF2301"))


# Analysis 2.10:
anal1_10 = RentalDataset %>%
  group_by(Renter_Preferred,BHK,Area_Class,Contact_Type,Bathroom,
           Furnishing_Types)%>%
  summarise(Count = length(Renter_Preferred))%>%
  view()


# Question 3.0: Distribution of how much tenant pay according to the size of the house?
# Analysis 3.1: Price described by size and multi factors
anal3_1 = RentalDataset %>%
  ggplot (aes(Square_Feet,Price,size =BHK, color =Area_Class ))+
  geom_point()+
  facet_wrap (~Renter_Preferred)+
  labs(title = "Price described by size and multi factors",
        x = "Square Feet",
        y = "Price")

anal3_1


# Analysis 3.2: The number of houses by size
anal3_2<-RentalDataset%>%
   group_by(Square_Feet)%>%
   summarise(Count = length(Square_Feet))%>%
  top_n(10)
  ggplot(anal3_2,aes(Square_Feet, Count))+
  geom_point(size = 3, alpha = 0.8)+
  geom_smooth(color = "red")+
  theme_bw()+
  labs(title = "The number of houses by size",
       x = "Square Feet",
       y = "Count")
anal3_2


# Analysis 3.3: The average size by BHK
anal3_3<-RentalDataset%>%
  group_by(BHK)%>%
  summarise(Avg_size = mean(Square_Feet))

  ggplot(anal3_3,aes(BHK, Avg_size))+
  geom_point(size = 3, alpha = 0.8,color = "green")+
  geom_smooth(color = "white")+
  theme_dark()+
  labs(title = "The average size by BHK",
       x = "BHK",
       y = "Square Feet")

anal3_3


# Analysis 3.4: The average rent by BHK
anal3_4<-RentalDataset%>%
  group_by(BHK)%>%
  summarise(Avg_price = mean(Price))
theme_black = function() {
  theme(
    # Specify panel options
    panel.background = element_rect(fill = "black", color  =  NA),  
    panel.border = element_rect(fill = NA, color = "white"),  
    panel.grid.major = element_line(color = "grey35"),  
    panel.grid.minor = element_line(color = "grey20"),  
    panel.margin = unit(0.5, "lines"))  
}
  ggplot(anal3_4,aes(BHK, Avg_price))+
  geom_point(size = 3, alpha = 0.8,color = "white")+
  geom_smooth(method = "lm", se = FALSE, color = "white")+
  theme_black()+
  labs(title = "The average Price by BHK",
       x = "BHK",
       y = "Price")

anal3_4

# Analysis 3.5: The number of houses by BHK
anal3_5<-RentalDataset%>%
  group_by(BHK)%>%
  summarise(Count = length(BHK))%>%
  top_n(10)
  ggplot(anal3_5, aes(BHK, Count))+
  geom_point(size = 3, alpha = 0.8)+
  geom_smooth(method = 'glm')+
  theme_bw()+
  labs(title = "Number of houses by BHK",
       x = "BHK",
       y = "Count")
anal3_5

# Analysis 3.6: The average size by Area Type 
anal3_6<-RentalDataset%>%
  group_by(Area_Class)%>%
  summarise(Avg_size = mean(Square_Feet))%>%
  view()

# Analysis 3.7: The average price by Area Type 
anal3_7<-RentalDataset%>%
  group_by(Area_Class)%>%
  summarise(Avg_Price = mean(Price))%>%
  view()

# Analysis 3.8: The average size in every city
anal3_8<-RentalDataset%>%
  group_by(City)%>%
  summarise(Avg_Price = mean(Price))%>%
  view()
ggplot(anal3_8, aes(x=City, y=Avg_Price)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(title = "Average size in every city",
       x = "City",
       y = "Avg_Price")


# Analysis 3.9:  verage Price of every city by area and Furnished type .
anal3_9<-RentalDataset%>%
  group_by(City, Area_Class,Furnishing_Types)%>%
  summarise(Avg_Price = mean(Price))%>%
  view()
ggplot(anal3_9, aes(x=Furnishing_Types, y=Avg_Price,  fill = City)) + 
  geom_bar(stat = "identity") +
  facet_wrap (~Area_Class)+
  labs(title = "Average Price of every city by area and Furnished type",
       x = "Furnishing Types",
       y = "Avg_Price")


# Analysis 3.6: Conclusion
anal3_6 = RentalDataset
anal3_6 = merge(x = anal3_3, y = anal3_4,  by = "BHK")
anal3_6 = merge(x = anal3_6,y = anal3_5,  by = "BHK")
anal3_6$BHK<-as.character(anal3_6$BHK)
  ggplot (anal3_6,aes(Avg_size,Avg_price,size =Count,color = BHK ))+
  geom_point()+
  theme_bw()+
  geom_smooth(size = 7)+
  labs(title = "BHK described by the price and size",
       x = "Avarage Size",
       y = "Avarage Price")
anal3_6
  
# Question 4.0: Distribution of which city do tenants prefer to live in and why
# Analysis 4.1: Number of houses by the city

anal4_1 = RentalDataset
anal4_1<-RentalDataset%>%
  group_by(City)%>%
  summarise(Count = length(City))
ggplot(anal4_1,aes(x=City, y=Count, colour = City)) +
  geom_segment( aes(x=City, xend=City, y=1, yend=Count)) +
  geom_point(  size=4, alpha=0.6) +
  stat_summary(fun = mean, geom = "point", size = 5)+
  geom_hline(aes (yintercept = mean (Count)),colour = "gray70",
             size = 0.9)+
  geom_segment(aes(x = City, xend = City,
                   y = 1, yend = Count), size = 2)+
  theme_light() +
  coord_flip() +
  theme (legend.position = "none") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "Number of houses by the city")
view(anal4_1)  

# Analysis 4.2: The most rented location in every city

anal4_2 = RentalDataset
anal4_2<-RentalDataset%>%
  group_by(City, Location)%>%
  summarise(Count = length(City))%>%
  top_n(1)
ggplot(anal4_2,aes(x=Location, y=Count, colour = City)) +
  geom_segment( aes(x=Location, xend=Location, y=1, yend=Count)) +
  geom_point(  size=4, alpha=0.6) +
  stat_summary(fun = mean, geom = "point", size = 5)+
  geom_hline(aes (yintercept = mean (Count)),colour = "gray70",
             size = 0.9)+
  geom_segment(aes(x = Location, xend = Location,
                   y = 1, yend = Count), size = 2)+
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+labs (title = "The most rented location in every city")
view(anal4_2) 

        
# Analysis 4.3: The average Rent by city

anal4_3 = RentalDataset
anal4_3<-RentalDataset%>%
  group_by(City)%>%
  summarise(Avg_price = mean(Price))
ggplot(anal4_3,aes(x=City, y=Avg_price )) +
  geom_hline(aes (yintercept = mean (Avg_price)),colour = "gray70",
             size = 0.9)+
  geom_segment(aes(x = City, xend = City,
                   y = 1, yend = Avg_price), size = 1,
               colour = "sky blue")+
  geom_point(color="blue",size=4, alpha=0.6) +
  theme_light() +
  theme (legend.position = "none") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "The average Rent by city", y = "Average Price")
view(anal4_3)  
m =mean(anal4_3$Avg_price)  

# Analysis 4.4: The Average Rent by city in most rented location 
anal4_4 = RentalDataset
anal4_4<-RentalDataset%>%
  group_by(City, Location)%>%
  summarise(Avg_price = mean(Price),Count = length(City))%>%
  top_n(1)%>%
  subset(select = c("City","Location","Avg_price" ))

ggplot(anal4_4,aes(x=Location, y=Avg_price, colour = City)) +
  geom_segment( aes(x=Location, xend=Location, y=1, yend=Avg_price)) +
  geom_point(  size=4, alpha=0.6) +
  stat_summary(fun = mean, geom = "point", size = 5)+
  geom_hline(aes (yintercept = mean (Avg_price)),colour = "gray70",
             size = 0.9)+
  geom_segment(aes(x = Location, xend = Location,
                   y = 1, yend = Avg_price), size = 2)+
  theme_light() +
  coord_flip() +
  theme_dark()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  labs (title = "Average Rent by city in most rented location ",
        y = "Average Rent")
m =mean(anal4_4$Avg_price)  

view(anal4_4) 


# Analysis 4.5: The contact type preferred by city
anal4_5 = RentalDataset
anal4_5<-RentalDataset%>%
  group_by(City, Contact_Type)%>%
  subset( Contact_Type != "Contact Builder")%>%
  summarise(Count = length(City))
ggplot(anal4_5,aes(x=Contact_Type, y=Count, colour = Contact_Type)) +
  geom_segment( aes(x=Contact_Type, xend=Contact_Type, y=1, yend=Count)) +
  geom_point(  size=3, alpha=0.6) +
  stat_summary(fun = mean, geom = "point", size = 4)+
  theme_light() +
  facet_wrap(~City)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "The contact type preferred by city",
        x = "Contact Type")
view(anal4_5)  

# Analysis 4.6 :The contact type preferred by the city in most rented locations

anal4_6<-RentalDataset
anal4_6 %>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                         'Gachibowli', 'Salt Lake City Sector 2'))%>%
  group_by(City,Location , Contact_Type)%>%
  subset(Contact_Type != "Contact Builder")%>%
  summarise(Count = length(City))%>%
  view()%>%
  ggplot(aes(x=City, y=Count, color = Location)) +
  geom_segment( aes(x=City, xend=City, y=1, yend=Count,
                    color = City)) +
  geom_point( size=2, alpha=0.6) +
  theme_light() +
  facet_wrap(~Contact_Type)
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "Contact type preferred by city in most rented locations ",
        x = "Location",
        y = "Count")

    
# Analysis 4.7 : The Area Type preferred by tenants in most rented locations
anal4_7<-RentalDataset
anal4_7 %>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                          'Gachibowli', 'Salt Lake City Sector 2'))%>%
  group_by(Area_Class,Location , Renter_Preferred)%>%
  subset(Contact_Type != "Contact Builder")%>%
  summarise(Count = length(Location))%>%
  view()%>%
  ggplot(aes(x=Area_Class, y=Count,
             color = Renter_Preferred)) +
  geom_segment( aes(x=Area_Class, xend=Area_Class, y=1, yend=Count)) +
  geom_point( size=2, alpha=0.6) +
  theme_light() +
  facet_wrap(~Location)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  coord_flip()+
    labs (title = "Area Type preferred by tenants in most rented locations ",
          x = "Area Class",
          y = "Count")

# Analysis 4.8 : When renters usually rent in the most rented location bases on the furnishing type. 
anal4_8<-RentalDataset
anal4_8 %>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                         'Gachibowli', 'Salt Lake City Sector 2'))%>%
  group_by(Published_date = format(Published_date, "%B"),Furnishing_Types, Location)%>%
  summarise(Count = length(Location))%>%
  view()%>%
  ggplot(aes(x=Location, y=Count,
             color = Furnishing_Types)) +
  geom_segment( aes(x=Location, xend=Location, y=1, yend=Count)) +
  geom_point( size=2, alpha=0.6) +
  theme_light() +
  facet_wrap(~factor(Published_date, levels = c("April","May", "June","July")))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  coord_flip()+
  labs (title = "When renters rent in most rented location based on furnishing type ",
        x = "Location",
        y = "Count")


# Analysis 4.9 : What is the average of BHKs in most rented locations?
anal4_9<-RentalDataset
anal4_9 %>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                         'Gachibowli', 'Salt Lake City Sector 2'))%>%
  group_by(BHK, Location)%>%
  summarise(Avg_BHK = mean(BHK))%>%
  view()%>%
  ggplot(aes(x=BHK, y=Avg_BHK)) +
  geom_segment( aes(x=BHK, xend=BHK, y=1, yend=Avg_BHK)) +
  geom_point( size=2, alpha=0.6, color = "white") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  facet_wrap(~Location)+
  theme_dark()+
  labs (title = "Average of BHKs in most rented locations ",
        x = "BHK",
        y = "Count")

# Analysis 4.9 Conclusion

anal4_10<-RentalDataset
anal4_10 %>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                         'Gachibowli', 'Salt Lake City Sector 2'))%>%
  ggplot(aes(x=Square_Feet, y=Price,color = Location)) +
  geom_point(size = 4) +
  theme_light() +
  facet_wrap(~City)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "City displaied by most rented location, size and price ",
        x = "Square Feet",
        y = "Price")


# Question 5.0: Distribution of how the number of floors will affect tenants from renting houses.
# Analysis 5.1: Number of houses by the total number of floors

anal5_1 = RentalDataset
anal5_1%>%
  group_by(Total_Floor)%>%
  summarise(Count = length(Total_Floor))%>%
  top_n(8)%>%
  view()%>%
  ggplot(aes(Total_Floor, Count,fill = Count)) +
  geom_bar(stat='Identity') +
  labs(title='Houses by the number of floors') +
  theme_bw()

# Analysis 5.2: What floors do renters prefer of houses
anal5_2 = RentalDataset
anal5_2%>%
  group_by(Renter_Preferred,Total_Floor,Floor_Number )%>%
  summarise(Count = length(Total_Floor))%>%
  top_n(1)%>%
  view()%>%
  ggplot( aes(Floor_Number, Total_Floor, color =Renter_Preferred))+
  geom_point(size = 3, alpha = 0.8)+
  geom_smooth(method = 'glm', color = "black")+
  theme_bw()+
  facet_wrap(~Renter_Preferred)+
  labs(title = "What floors do renters prefer",
       x = "Floor_Number",
       y = "Count")
anal5_2


# Analysis 5.3: : Average price for renters of houses by total number of floors
anal5_3 = RentalDataset
anal5_3%>%
  filter(Total_Floor <= 30)%>%
  group_by(Total_Floor, Renter_Preferred)%>%
  summarise(Avg_rent = mean(Price))%>%
  top_n(1)%>%
  view()%>%
  ggplot(aes(Total_Floor, Avg_rent,fill = Renter_Preferred)) +
  geom_bar(stat='Identity') +
  labs(title='Average price for renters of houses by total number of floors',
       x = "Total Floor",
       y = 'Avarage rent price'
       ) +
  xlim(1:30,main='')+
  theme_bw()





# Analysis 5.4: Does the average size of the house related to which floor is the house on?
anal5_4<-RentalDataset%>%
  group_by(Floor_Number)%>%
  summarise(Avg_size = mean(Square_Feet))%>%
  view()%>%
  ggplot( aes(Floor_Number, Avg_size))+
  geom_point(size = 3, alpha = 0.8)+
  geom_smooth(method = 'glm', color = "Red")+
  theme_bw()+
  labs(title = "Size of the house by which floor is the house on",
       x = "Floor Number",
       y = "Avarege size")



# Analysis 5.5: Does the average size of the house related to which floor is the house on?

anal5_5<-RentalDataset
anal5_5 %>%
  ggplot( aes(x=Total_Floor, y=City, fill=City)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")+
  labs(title = "Floors of the houses by cities",
       x = "Total floor of the house",
       y = "City")

# Analysis 5.6: From the most rented location, which location has the most total floors by city
anal5_6 = RentalDataset
anal5_6%>%
  filter(Location %in% c('Velachery','Laxmi Nagar',
                         'Gachibowli', 'Salt Lake City Sector 2'))%>%
  view()%>%
  ggplot( aes(x=Floor_Number, y=City, fill=Location)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="h") +
  theme_ipsum() +
  coord_flip()+
  theme(
    plot.title = element_text(size=11)
  ) +
  xlab("")+
  labs(title = "ocation has the most total floors by city",
       x = "Total floor of the house",
       y = "City")


# Analysis 5.7: Bachelors differ from families in choosing the floor number

anal5_7<-RentalDataset
anal5_7 %>%
  ggplot(aes(x=Floor_Number, y=Total_Floor, color = Area_Class)) +
  geom_segment( aes(x=Floor_Number, xend=Floor_Number, y=1, yend=Total_Floor)) +
  geom_point(  size=2, alpha=0.6) +
  theme_light() +
  theme (legend.position = "none") +
  facet_wrap(~Renter_Preferred)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "Bachelors and families in choosing the floor number",
        x = "Floor number",
        y = "Total floor")


# Analysis 5.8:Area class differences in choosing the floor number
anal5_8<-RentalDataset
anal5_8 %>%
  subset( Area_Class != "Built Area")%>%
  ggplot(aes(x=Floor_Number, y=Total_Floor)) +
  geom_segment( aes(x=Floor_Number, xend=Floor_Number, y=1, yend=Total_Floor)) +
  geom_point(  size=2, alpha=0.6, color = "white") +
  theme_light() +
  facet_wrap(~Area_Class)+
  theme_dark()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "Area class differences in choosing the floor number",
        x = "Floor number",
        y = "Total floor")


# Analysis 5.9 :Contact type differences in choosing the floor number 
anal5_9<-RentalDataset
anal5_9 %>%
  subset( Contact_Type != "Contact Builder")%>%
  ggplot(aes(x=Floor_Number, y=Total_Floor)) +
  geom_segment( aes(x=Floor_Number, xend=Floor_Number, y=1, yend=Total_Floor)) +
  geom_point(shape = 15, size=2, alpha=0.6, color = "#800000") +
  theme_light() +
  facet_wrap(~Contact_Type)+
  coord_flip()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs (title = "Contact type differences in choosing the floor number ",
        x = "Floor number",
        y = "Total floor")


# Analysis 5.10 : conclusion
anal5_10 <- RentalDataset
ggplot(anal5_10,aes(x=Floor_Number, y=Total_Floor, size= Square_Feet,
                    fill = Renter_Preferred)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 10)) +
  scale_fill_viridis(discrete=TRUE, option="A") +
  facet_wrap(~City)+
  theme_ipsum() +
  guides(size = FALSE)+
  labs (title = "Population of houses according to the floor number and type of renters ",
        x = "Floor number",
        y = "Total floor")