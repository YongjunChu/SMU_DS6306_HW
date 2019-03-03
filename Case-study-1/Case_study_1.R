library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

##question 1 : how many breweries are present in each state?

#upload the two data files: "Beers.csv"" and "Breweries.csv"
beer <- read.csv("C:/Users/chu001/Documents/Yongjun-Chu files/SMU-data-science-application/Doing-Data_Science/Case_study_1/Beers.csv", sep = ',', header = T)
str(beer)
head(beer)
#convert all empty or space entries to NA
beer[beer=="" | beer == " "] <- NA
#apply(is.na(beer), 2, which)
sapply(beer, function(x) sum(is.na(x)))

brewery <- read.csv("C:/Users/chu001/Documents/Yongjun-Chu files/SMU-data-science-application/Doing-Data_Science/Case_study_1/Breweries.csv", sep = ',', header = T)
str(brewery)
head(brewery)
#convert all empty or space entries to NA
brewery[brewery == "" | brewery == " "] <- NA
sapply(brewery, function(x) sum(is.na(x)))

#remove the extra empty space before the state name in column State
brewery$State <- as.factor(gsub("^\\s+", "", brewery$State))
str(brewery)
#or using str_trim to remove the empty space at the beginning/end of a string
library(stringr)
brewery$State <- as.factor(str_trim(brewery$State))

library(plyr)
require("gridExtra")
number = count(brewery, 'State')
number
unique(number$freq)

#another way to figure out this question
brewery %>% group_by(State) %>% dplyr::summarise(brew_per_state =length(Brew_ID))

#ggplot the freq of brewery for each state in decreasing order
p = ggplot(number, aes(x=reorder(State, freq), y=freq, fill=State)) + geom_bar(stat="identity") + labs(title="The number of brewers by state", 
         x="State", y = "Count") + theme_minimal() + coord_flip() + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5, size = 18, face = "bold"), axis.title = element_text(size = 16, face = "bold"))
p


###question 2: maerge two data frames

#Merge beer data with the breweries data. Print the first 6 observations and the #last six observations to check the merged file
colnames(beer)[1] <- "Name_beer"
colnames(beer)[5] <- "Brew_ID"
colnames(brewery)[2] <- "Name_brew"
total = merge(beer, brewery, by.x="Brew_ID", by.y="Brew_ID", all = T)
nrow(total)
head(total)
tail(total)
str(total)

#to produce less columns for presentation city of St Lois and MO
head(total %>% select(Name_beer, Beer_ID, ABV, IBU, Style, Name_brew, City, State))
total %>% filter(grepl("Saint Louis", total$City))
total %>% filter(grepl("Budweiser", total$Name_beer))
total %>% filter(grepl("DC", total$State))

total %>% filter(grepl(0.128, total$ABV))
total %>% filter(grepl(138, total$IBU))

###Question 3

#Report the exact rows having NA in each column
#apply(is.na(total), 2, which)
#head(total, n=20)

#report the total of rows having 'NA' in each column
number_NA <- sapply(total, function(x) sum(is.na(x)))
number_NA


###Question 4

#Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare

str(total)
#get the histogram of ABV and IBU for TX: 
qplot(total$ABV[total$State == "TX"],
      geom="histogram",
      binwidth = 0.002,  
      main = "Histogram for ABV", 
      xlab = "ABV",  
      fill=I("blue"))

qplot(total$IBU[total$State == "TX"],
      geom="histogram",
      binwidth = 2,  
      main = "Histogram for IBU", 
      xlab = "IBU",  
      fill=I("red"))

#get the histogram of ABV and IBU for all states: 
qplot(total$ABV,
      geom="histogram",
      binwidth = 0.002,  
      main = "Histogram for ABV", 
      xlab = "ABV",  
      ylab="Frequency",
      fill=I("blue")) + theme_grey(base_size = 16)

qplot(total$IBU,
      geom="histogram",
      binwidth = 2,  
      main = "Histogram for IBU", 
      xlab = "IBU", 
      ylab="Frequency",
      fill=I("red"))+ theme_grey(base_size = 16)

#ggplot(data=total, aes(total$ABV)) + geom_histogram()

#get median ABV and IBU for each state
median <-  total %>% group_by(State) %>% dplyr::summarise(median_ABV=median(ABV, na.rm = T),median_IBU = median(IBU,na.rm=T) )
median
filter(median, State=="DC")
filter(median, State == "ME")

#plot a bar graph to compare ABV
ggplot(median, aes(x=reorder(State, -median_ABV),y=median_ABV, fill=State)) + geom_bar(stat = 'identity') + labs(title="The median ABV by state", 
         x="State", y = "ABV") +  theme_minimal() + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5, size=18,face = "bold"), axis.title = element_text(size = 16, face = "bold"), axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold"))

#plot a bar graph to compare IBU
ggplot(median, aes(x=reorder(State, -median_IBU),y=median_IBU, fill=State)) + geom_bar(stat = 'identity') + labs(title="The median IBU by state", 
         x="State", y = "IBU") + theme_minimal()  + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5, size=18,face = "bold"), axis.title = element_text(size = 16, face = "bold")) 



##the following bi-axis plotting didn't work in ggplot

#plot a bar graph to compare ABV and IBU using bi-axis
median$median_ABV <- as.numeric(median$median_ABV)
ggplot(median, aes(x= State,y=median_ABV, group=1)) + geom_line()

#plot a bar graph to compare ABV and IBU using bi-axis
devtools::install_github('trinker/plotflow')
library(plotflow)

plotflow::ggdual_axis(
    ggplot(data = median, aes(x= State,y =median_ABV)) + geom_line(),
    ggplot(data = median, aes(x= State,y =median_IBU)) + geom_line()
)
  
#plot a bar graph to compare ABV and IBU using bi-axis
 library(ggplot2)
p <- ggplot(median, aes(x = State))
  p <- p + geom_line(aes(y = median_ABV, colour = "median_ABV")) 
  
  p <- p + geom_line(aes(y = median_IBU, colour = "median_IBU"))
   
  # adding the relative humidity data, transformed to match roughly the range of the temperature
  p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "Air temperature [°C]",
                x = "Date and time",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.9))
p




###question 5


#Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter #(IBU) beer?
#
max <- total %>% group_by(State) %>% dplyr::summarise(max_ABV=max(ABV, na.rm = T),max_IBU = max(IBU,na.rm=T) )
max

top_5_ABV <- arrange(max, -max_ABV)[1:5,]
print(top_5_ABV)
top_5_IBU <- arrange(max, -max_IBU)[1:5,]
print(top_5_IBU)

#sub_max_ABV <- filter(max, State == " CO"|State ==" KY"|State ==" IN"|State ==" NY"|State ==" CA")
#sub_max_IBU <- filter(max, State == " OR"|State ==" VA"|State ==" MA"|State ==" OH"|State ==" MN")

#plot a bar graph to compare max ABV for top 5 states
ggplot(top_5_ABV, aes(x=reorder(State, -max_ABV),y=max_ABV, fill=State)) + geom_bar(stat = 'identity') + labs(title="The top 5 states having the largest max_ABV", 
          x="State", y = "max_ABV") + theme_minimal() + coord_flip()+  theme_bw(base_size=16) + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5))  

#plot a bar graph to compare max IBU for top 5 states
ggplot(top_5_IBU, aes(x=reorder(State, -max_IBU),y=max_IBU, fill=State)) + geom_bar(stat = 'identity') + labs(title="The top 5 states having the largest max_IBU", 
          x="State", y = "max_IBU") + theme_minimal() + coord_flip()+  theme_bw(base_size=16) + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5))  


max_ABV_state <- filter(max, max_ABV == max(max_ABV))
max_ABV_state

max_IBU_state <- filter(max, max_IBU == max(max_IBU))
max_IBU_state



###Question 6

#Summary statistics for the ABV variable.
summary(total$ABV, na.rm=T)
summary(total$IBU, na.rm=T)


###question 7

#Is there an apparent relationship between the bitterness of the beer and its #alcoholic content? Draw a scatter plot

#a scatter plot should solve this problem
ggplot(total, aes(x=ABV, y=IBU)) + geom_point(color="blue", size=1.5, alpha=0.4, pch=16)+ theme_bw(base_size=16) + geom_smooth(method = "lm", se = T, color="red") + labs(title="The scatter plot between ABV and IBU") + theme(legend.position="none") + theme(plot.title=element_text(hjust=0.5))
total$IBU <- as.numeric(total$IBU)

#the cor function is not working becaue of the NA in the data
corr_coef <- round(cor(total$ABV, total$IBU), digits = 2)
corr_coef

library("ggpubr")
ggscatter(total, x = "ABV", y = "IBU", color = "black",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", add.params = list(color = "blue", fill = "lightgray"),
          xlab = "ABV", ylab = "IBU", title="The scatter plot between ABV and IBU")


