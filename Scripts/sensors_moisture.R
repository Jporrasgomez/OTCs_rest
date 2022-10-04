library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
source('Scripts/basicFun.R')

#Opening and checking data----------------------------------------------------------
##!!!soil moisture no tiene distribucion normal
c1 <- c1 <- read_delim("Data/control1_20072022_modificado.csv", 
                       ";", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
colnames(c1) <- c("n", "date_time","time_zone", "T_ground_c1","T_bottom_c1","T_top_c1",
                  "soil_moisture_c1")
c1 <- c1  %>% select(date_time, T_ground_c1, T_bottom_c1, T_top_c1, soil_moisture_c1)
c1[c('date', 'time')] <- str_split_fixed(c1$date_time, ' ', 2)
c1$datenew <- ymd(c1$date)
c1 <- subset(c1, datenew >= "2022-06-22")
c1$month <- month(c1$datenew, label = T)
c1$day <- day(c1$datenew)
c1$timenew <- hm(c1$time)
c1$hour <- hour(c1$timenew)
c1$min <- minute(c1$timenew)

hist(c1$T_bottom_c1)
hist(c1$T_top_c1)
hist(c1$T_ground_c1)
hist(c1$soil_moisture_c1)
summary(c1)

c2 <- read_delim("Data/control2_20072022_modificado.csv", 
                 ";", escape_double = FALSE, col_names = FALSE, 
                 trim_ws = TRUE)
colnames(c2) <- c("n", "date_time","time_zone", "T_ground_c2","T_bottom_c2","T_top_c2",
                  "soil_moisture_c2")
c2 <- c2  %>% select(date_time, T_ground_c2, T_bottom_c2, T_top_c2, soil_moisture_c2)
c2[c('date', 'time')] <- str_split_fixed(c2$date_time, ' ', 2)
c2$datenew <- ymd(c2$date)
c2 <- subset(c2, datenew >= "2022-06-22")
c2$month <- month(c2$datenew, label = T)
c2$day <- day(c2$datenew)
c2$timenew <- hm(c2$time)
c2$hour <- hour(c2$timenew)
c2$min <- minute(c2$timenew)
hist(c2$T_bottom_c2)
hist(c2$T_top_c2)
hist(c2$T_ground_c2)
hist(c2$soil_moisture_c2)
summary(c2)

otc1 <- read_delim("Data/otc1_20072022_modificado.csv", 
                   ";", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)
colnames(otc1) <- c("n", "date_time","time_zone", "T_ground_otc1","T_bottom_otc1","T_top_otc1",
                    "soil_moisture_otc1")
otc1 <- otc1  %>% select(date_time, T_ground_otc1, T_bottom_otc1, T_top_otc1, soil_moisture_otc1)
otc1[c('date', 'time')] <- str_split_fixed(otc1$date_time, ' ', 2)
otc1$datenew <- ymd(otc1$date)
otc1 <- subset(otc1, datenew >= "2022-06-22")
otc1$month <- month(otc1$datenew, label = T)
otc1$day <- day(otc1$datenew)
otc1$timenew <- hm(otc1$time)
otc1$hour <- hour(otc1$timenew)
otc1$min <- minute(otc1$timenew)
hist(otc1$T_bottom_otc1)
hist(otc1$T_top_otc1)
hist(otc1$T_ground_otc1)
hist(otc1$soil_moisture_otc1)
summary(otc1)

otc2 <-  read_delim("Data/otc2_20072022_modificado.csv", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
colnames(otc2) <- c("n", "date_time","time_zone", "T_ground_otc2","T_bottom_otc2","T_top_otc2",
                    "soil_moisture_otc2")
otc2 <- otc2  %>% select(date_time, T_ground_otc2, T_bottom_otc2, T_top_otc2, soil_moisture_otc2)
otc2[c('date', 'time')] <- str_split_fixed(otc2$date_time, ' ', 2)
otc2$datenew <- ymd(otc2$date)
otc2 <- subset(otc2, datenew >= "2022-06-22")
otc2$month <- month(otc2$datenew, label = T)
otc2$day <- day(otc2$datenew)
otc2$timenew <- hm(otc2$time)
otc2$hour <- hour(otc2$timenew)
otc2$min <- minute(otc2$timenew)
hist(otc2$T_bottom_otc2)
hist(otc2$T_top_otc2)
hist(otc2$T_ground_otc2)
hist(otc2$soil_moisture_otc2)
summary(otc2)
str(otc2)

##Hay anomalías en las mediciones. Debería decir a R que me busque datos en los que haya una diferencia superior a 3 grados entre el dato y la medida anterior
#Para detectarlos. En 15 minutos no debería haber diferencias superiores a ni si quiera 1 grado. A NO SER que haya nubosidad dispersa y la incidencia solar
#cambie
#Anomalías detectadas: c1 (2022.06.21 12:45, t:ground) de 48ºC  // c2 (2022.06.21 12:45, t_ground) de 60ºC, SON LAS DOS A LA MISMA HORA Y DIA!
#De momento, utilizo los datos a partir del día 22 para evitar esas anomalías. 


plot1 <- merge(c1, otc1, all = TRUE)
plot2 <- merge(c2, otc2, all = TRUE)
all_data <- merge(plot1, plot2, all = TRUE)

otcs <- merge(otc1, otc2, all = TRUE)
controls <- merge(c1, c2, all = TRUE)

#Plot 1 -----------------------------------------------
#Cambiar cosas de temp por moisture

plot1 <- plot1 %>% select("soil_moisture_c1","soil_moisture_otc1", "datenew",
                          "month", "day", "timenew", "hour", "min")

plot1_dif <- summarise(group_by(plot1, hour, min),
                            t_top_dif = soil_moisture_otc1 - soil_moisture_c1)
                            

plot1_temp_day <- summarise(group_by(plot1_temp, datenew),
                            t_top_mean_c1 = mean(T_top_c1, na.rm = T),
                            t_top_serr_c1 = s.err(T_top_c1),
                            t_bottom_mean_c1 = mean(T_bottom_c1, na.rm = T),
                            t_bottom_serr_c1 = s.err(T_bottom_c1),
                            t_ground_mean_c1 = mean(T_ground_c1, na.rm = T),
                            t_ground_serr_c1 = s.err(T_ground_c1),
                            t_top_mean_otc1 = mean(T_top_otc1, na.rm = T),
                            t_top_serr_otc1 = s.err(T_top_otc1),
                            t_bottom_mean_otc1 = mean(T_bottom_otc1, na.rm = T),
                            t_bottom_serr_otc1 = s.err(T_bottom_otc1),
                            t_ground_mean_otc1 = mean(T_ground_otc1, na.rm = T),
                            t_ground_serr_otc1 = s.err(T_ground_otc1))

plot1_temp_24h <- summarise(group_by(plot1_temp_dif, hour, min),
                            t_top_dif_mean = mean(t_top_dif, na.rm = T),
                            t_top_dif_serr = s.err(t_top_dif),
                            t_bottom_dif_mean = mean(t_bottom_dif, na.rm = T),
                            t_bottom_dif_serr = s.err(t_bottom_dif),
                            t_ground_dif_mean = mean(t_ground_dif, na.rm = T), 
                            t_ground_dif_serr = s.err(t_ground_dif))

plot1_temp_24h$time <- paste(plot1_temp_24h$hour,":",plot1_temp_24h$min)
