setwd("G:/My Drive/STAT 3105")

library(jsonlite)
library(dplyr)

#process data
data <- fromJSON("unemployment_cpi_unempl_2000_2020.json")
df <- as.data.frame(bind_rows(data$Results))
a <- data.frame(df$data[1])
b <- data.frame(df$data[2])

#remove unnecessary columns
a <- subset(a, select = -footnotes)
b <- subset(b, select = -footnotes)

#reassign 1st/2nd half status to each month
a$period[a$period=="M12" | a$period == "M11" | a$period == "M10" | a$period == "M09" | a$period == "M08"| a$period == "M07"] <- "2nd Half"

a$period[a$period=="M06" | a$period == "M05" | a$period == "M04" | a$period == "M03" | a$period == "M02"| a$period == "M01"] <- "1st Half"

#change type of column from char to double
a$value = as.double(a$value)

#obtain average unemployment per period for first part
first_part <- a %>%
  group_by(year, period) %>%
  summarise(mean(value))

names(first_part) <- c("year", "period", "unemployment")

#change type of column from char to double
b$value = as.double(b$value)

#obtain average CPI per period for 2nd part
second_part <- b %>% 
  group_by(year, periodName) %>%
  summarise(mean(value))
names(second_part) <- c("year", "period", "cpi")

#add CPI to df

first_part$cpi <- second_part$cpi
df <- first_part

#export to csv
write.csv("CPI.csv")

#calculate inflation
df$inflation <- NA

counter = 1
for(index in df$cpi){
   if(counter == 1) {
    df$inflation[counter] <- NA
    counter = counter + 1
  }
  df$inflation[counter] <- (df$cpi[counter] - df$cpi[counter - 1])/df$cpi[counter -1] * 100
  counter = counter + 1
}

#scatter plot inflation vs unemployment
plot(df$unemployment, df$inflation,
     main = "Inflation vs Unemployment", xlab = "Unemployment Rate (Percentage)",
     ylab = "Inflation Rate (Percentage)")
