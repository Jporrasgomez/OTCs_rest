
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
source('Scripts/basicFun.R')

#Opening data
c1 <- read.csv("Data/control1_piloto_modificado.csv")
colnames(c1) <- c("n", "date_time","time_zone", "T_ground_c1","T_bottom_c1","T_top_c1",
                  "soil_moisture_c1")
c1 <- c1  %>% select(date_time, T_ground_c1, T_bottom_c1, T_top_c1, soil_moisture_c1)
hist(c1$T_bottom_c1)

c2 <- read.csv("Data/control2_piloto_modificado.csv")
colnames(c2) <- c("n", "date_time","time_zone", "T_ground_c2","T_bottom_c2","T_top_c2",
                  "soil_moisture_c2")
c2 <- c2  %>% select(date_time, T_ground_c2, T_bottom_c2, T_top_c2, soil_moisture_c2)
hist(c2$T_bottom_c2)

otc1 <- read.csv("Data/otc1_piloto_modificado.csv")
colnames(otc1) <- c("n", "date_time","time_zone", "T_ground_otc1","T_bottom_otc1","T_top_otc1",
                    "soil_moisture_otc1")
otc1 <- otc1  %>% select(date_time, T_ground_otc1, T_bottom_otc1, T_top_otc1, soil_moisture_otc1)
hist(otc1$T_bottom_otc1)

otc2 <-  read.csv("Data/otc2_piloto_modificado.csv")
colnames(otc2) <- c("n", "date_time","time_zone", "T_ground_otc2","T_bottom_otc2","T_top_otc2",
                    "soil_moisture_otc2")
otc2 <- otc2  %>% select(date_time, T_ground_otc2, T_bottom_otc2, T_top_otc2, soil_moisture_otc2)
hist(otc2$T_bottom_otc2)

data1 <- merge(c1, otc1, all = TRUE)
data2 <- merge(c2, otc2, all = TRUE)
all_data <- merge(data1, data2, all = TRUE)

#Preguntar a los datos: ¿cuál es la diferencia de temperatura media para cada tipo de dato (t1, t2, t3) durante la noche y durante el día?
#Hay que buscar un paquete que sea como lubridate a las horas
#Consejo de Teresa: En cuanto a tu pregunta de R, creo que los más práctico es usar las funciones "dmy_hms" y sus hermanas 
#("dmy_hm" etc) que también están dentro de lubridate. He probado con:
#lubridate::dmy_hms("20-06-2022 01:10:10")



### DATOS EN CONJUNTO

c1 <- melt(c1, id.var = c('date_time'), variable.name = 'Data')
c1$plot <- paste("control1")
c2 <- melt(c2, id.var = c('date_time'), variable.name = 'Data')
c2$plot <- paste("control2")
otc1 <- melt(otc1, id.var = c('date_time'), variable.name = 'Data')
otc1$plot <- paste("OTC1")
otc2 <- melt(otc2, id.var = c('date_time'), variable.name = 'Data')
otc2$plot <- paste("OTC2")

plot1 <- merge(c1, otc1, all = TRUE)
plot2 <- merge(c2, otc2, all = TRUE)
all_plots <- merge(plot1, plot2, all = TRUE)

#PLOT1

plot1[c('date', 'time')] <- str_split_fixed(plot1$date_time, ' ', 2)
plot1$datenew <- ymd(plot1$date)
plot1$month <- month(plot1$datenew, label = T)
plot1$day <- day(plot1$datenew)
plot1 <- subset(plot1, day >= 21)


plot1_temp <- plot1 %>% 
  filter(Data %in% c("T_ground_c1","T_bottom_c1","T_top_c1","T_ground_otc1","T_bottom_otc1","T_top_otc1"))

ggplot(plot1_temp, aes(date_time,value, color = plot)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth() + theme (text = element_text(size=10))+
  labs(x = "time", y = "Temperature")


plot1_moist <- plot1 %>% 
  filter(Data %in% c("soil_moisture_c1", "soil_moisture_otc1"))
ggplot(plot1_moist, aes(date_time,value, color = plot)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth() + theme (text = element_text(size=10))+
  labs(x = "time", y = "Moisture")


#PLOT2

plot2[c('date', 'time')] <- str_split_fixed(plot2$date_time, ' ', 2)
plot2$datenew <- ymd(plot2$date)
plot2$month <- month(plot2$datenew, label = T)
plot2$day <- day(plot2$datenew)
plot2 <- subset(plot2, day >= 21)


plot2_temp <- plot2 %>% 
  filter(Data %in% c("T_ground_c2","T_bottom_c2","T_top_c2","T_ground_otc2","T_bottom_otc2","T_top_otc2"))

ggplot(plot2_temp, aes(date_time,value, color = plot)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth() + theme (text = element_text(size=10))+
  labs(x = "time", y = "Temperature")


plot2_moist <- plot2 %>% 
  filter(Data %in% c("soil_moisture_c2", "soil_moisture_otc2"))
ggplot(plot2_moist, aes(date_time,value, color = plot)) +
  geom_point(alpha=0.4) + geom_line(alpha=0.4) + geom_smooth() + theme (text = element_text(size=10))+
  labs(x = "time", y = "Moisture")




