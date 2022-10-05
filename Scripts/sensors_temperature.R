#Comments----------------
##Hay anomalías en las mediciones. Debería decir a R que me busque datos en los que haya una diferencia superior a 8 grados entre el dato y la medida anterior

#cambie
#Anomalías detectadas: c1 (2022.06.21 12:45, t:ground) de 48ºC  // c2 (2022.06.21 12:45, t_ground) de 60ºC, SON LAS DOS A LA MISMA HORA Y DIA!
#De momento, utilizo los datos a partir del día 22 para evitar esas anomalías. 

#Hay anomalias en soil_moisture los días 6 de julio y 11 de agosto (precipitación). Hay que probar a enterrar mejor el sensor. Y SIno, 
#eliminar los datos en los que haya una fierencia mayor a 500 unidades (o algo así) entre mediciones consecutivas. 

#En los datos, no sé si está bien comparar los datos de las 24h de los plots (medias de las medias) con los datos de los puntos
#Incluir en los datos, las medias para all otcs y all controls

#Packages-----------------
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
source('Scripts/basicFun.R')

#Opening and checking data---------------------------
##!!!soil moistureno tiene distribucion normal por las anomalias

c1 <- c1 <- read_delim("Data/data_94238921_2022_10_04_1.csv", 
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

c2 <- read_delim("Data/data_94238924_2022_10_04_1.csv", 
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

otc1 <- read_delim("Data/data_94238923_2022_10_04_1.csv", 
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

otc2 <-  read_delim("Data/data_94238922_2022_10_04_1.csv", 
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



#Building main dataframes ---------------------
plot1 <- merge(c1, otc1, all = TRUE)
plot2 <- merge(c2, otc2, all = TRUE)
all_data <- merge(plot1, plot2, all = TRUE)

otcs <- merge(otc1, otc2, all = TRUE)
controls <- merge(c1, c2, all = TRUE)

#VISUALIZACIÓN PUNTOS -------------------------------

#C1
c1graph <- subset(c1, "2022-07-01" <= datenew)
c1graph$datetimenew <- ymd_hm(c1graph$date_time)

ggc1 <- ggplot(c1graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top_c1), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom_c1), group = 1, color = "blue")+
  geom_line(aes( y = T_ground_c1), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture_c1/40)), group = 1, color = "black")+
  scale_y_continuous(name = "Temperature (Cº)",
                     sec.axis = sec_axis(~.*40, name=" ")) + 
  theme_bw()+
  labs(x = " ", y =  "Temperature (ºC")+ 
  ggtitle("CONTROL 1") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
  )
    
#OTC1
otc1graph <- subset(otc1, "2022-07-01" <= datenew)
otc1graph$datetimenew <- ymd_hm(otc1graph$date_time)

ggotc1 <- ggplot(otc1graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top_otc1), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom_otc1), group = 1, color = "blue")+
  geom_line(aes( y = T_ground_otc1), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture_otc1/40)), group = 1, color = "black")+
  scale_y_continuous(name = " ",
                     sec.axis = sec_axis(~.*40, name="Soil moisture (raw data TMS-4)")) + 
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("OTC 1") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
    )

#C2
c2graph <- subset(c2, "2022-07-01" <= datenew)
c2graph$datetimenew <- ymd_hm(c2graph$date_time)

ggc2 <- ggplot(c2graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top_c2), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom_c2), group = 1, color = "blue")+
  geom_line(aes( y = T_ground_c2), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture_c2/40)), group = 1, color = "black")+
  scale_y_continuous(name = "Temperature (Cº)",
                     sec.axis = sec_axis(~.*40, name=" ")) + 
  theme_bw()+
  labs(x = " ", y =  "Temperature (ºC")+ 
  ggtitle("CONTROL 2") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
  )

#OTC2
otc2graph <- subset(otc2, "2022-07-01" <= datenew)
otc2graph$datetimenew <- ymd_hm(otc2graph$date_time)

ggotc2 <- ggplot(otc2graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top_otc2), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom_otc2), group = 1, color = "blue")+
  geom_line(aes( y = T_ground_otc2), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture_otc2/40)), group = 1, color = "black")+
  scale_y_continuous(name = " ",
                     sec.axis = sec_axis(~.*40, name="Soil moisture (raw data TMS-4)")) + 
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("OTC 2") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
  )

#ALL

ggarrange(ggc1, ggotc1, ggc2, ggotc2, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#PLOT1------------------------------------------


plot1 <- plot1 %>% select("T_ground_c1","T_bottom_c1","T_top_c1", "soil_moisture_c1","T_ground_otc1","T_bottom_otc1","T_top_otc1",
                          "soil_moisture_otc1", "datenew", "month", "day", "timenew", "hour", "min")

plot1_temp <- plot1 %>% select(1:3,5:7,9:14)

plot1_temp_dif <- summarise(group_by(plot1_temp, hour, min),
                        t_top_dif = T_top_otc1 - T_top_c1,
                        t_bottom_dif = T_bottom_otc1 - T_bottom_c1, 
                        t_ground_dif = T_ground_otc1 - T_ground_c1)

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


#intentar plotear las 3 temperaturas en el mismo gráfico con el s.err

ggplot1_temp <- ggplot(plot1_temp_24h, aes(x = hour)) + 
  geom_line(aes(y = t_top_dif_mean), color = "darkred") +
  geom_line(aes(y = t_bottom_dif_mean), color="blue")+
  geom_line(aes(y = t_ground_dif_mean), color="#4CAF50")+
  theme_bw()+
  labs(x = " ", y = "Temp. dif. OTC - control (ºC)")+ 
  ggtitle("Plot 1") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"))+
  geom_label(label="Top", x=23, y= -0.3,
      label.padding = unit(0.55, "lines"),
      label.size = 0.35,
      color = "darkred",
      fill="white") + 
  geom_label(label="Ground", x=22.5, y= 0.8,
                 label.padding = unit(0.55, "lines"),
                 label.size = 0.35,
                 color = "#4CAF50",
                 fill="white") + geom_label(label="Bottom", x=22.5, y= 3.3,
                 label.padding = unit(0.55, "lines"),
                 label.size = 0.35,
                 color = "blue",
                 fill="white")

ggplot1_temp


#PLOT2--------------------------------------------------------------

plot2 <- plot2 %>% select("T_ground_c2","T_bottom_c2","T_top_c2", "soil_moisture_c2","T_ground_otc2","T_bottom_otc2","T_top_otc2",
                          "soil_moisture_otc2", "datenew", "month", "day", "timenew", "hour", "min")

plot2_temp <- plot2 %>% select(1:3,5:7,9:14)

plot2_temp_dif <- summarise(group_by(plot2_temp, hour, min),
                            t_top_dif = T_top_otc2 - T_top_c2,
                            t_bottom_dif = T_bottom_otc2 - T_bottom_c2, 
                            t_ground_dif = T_ground_otc2 - T_ground_c2)

plot2_temp_day <- summarise(group_by(plot2_temp, datenew),
                            t_top_mean_c2 = mean(T_top_c2, na.rm = T),
                            t_top_serr_c2 = s.err(T_top_c2),
                            t_bottom_mean_c2 = mean(T_bottom_c2, na.rm = T),
                            t_bottom_serr_c2 = s.err(T_bottom_c2),
                            t_ground_mean_c2 = mean(T_ground_c2, na.rm = T),
                            t_ground_serr_c2 = s.err(T_ground_c2),
                            t_top_mean_otc2 = mean(T_top_otc2, na.rm = T),
                            t_top_serr_otc2 = s.err(T_top_otc2),
                            t_bottom_mean_otc2 = mean(T_bottom_otc2, na.rm = T),
                            t_bottom_serr_otc2 = s.err(T_bottom_otc2),
                            t_ground_mean_otc2 = mean(T_ground_otc2, na.rm = T),
                            t_ground_serr_otc2 = s.err(T_ground_otc2))

plot2_temp_24h <- summarise(group_by(plot2_temp_dif, hour, min),
                            t_top_dif_mean = mean(t_top_dif, na.rm = T),
                            t_top_dif_serr = s.err(t_top_dif),
                            t_bottom_dif_mean = mean(t_bottom_dif, na.rm = T),
                            t_bottom_dif_serr = s.err(t_bottom_dif),
                            t_ground_dif_mean = mean(t_ground_dif, na.rm = T), 
                            t_ground_dif_serr = s.err(t_ground_dif))

plot2_temp_24h$time <- paste(plot2_temp_24h$hour,":",plot2_temp_24h$min)

ggplot2_temp <- ggplot(plot2_temp_24h, aes(x = hour)) + 
  geom_line(aes(y = t_top_dif_mean), color = "darkred") + 
  geom_line(aes(y = t_bottom_dif_mean), color="blue")+
  geom_line(aes(y = t_ground_dif_mean), color="#4CAF50")+
  theme_bw()+
  labs(x = "24 h", y =  " ")+ 
  ggtitle("Plot 2") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  ) +
  geom_label(label="Top", x=23, y= 0.3,
             label.padding = unit(0.55, "lines"),
             label.size = 0.35,
             color = "darkred",
             fill="white") + 
  geom_label(label="Ground", x=22.5, y= 2.5,
             label.padding = unit(0.55, "lines"),
             label.size = 0.35,
             color = "#4CAF50",
             fill="white") + geom_label(label="Bottom", x=22.5, y= 1.5,
                                        label.padding = unit(0.55, "lines"),
                                        label.size = 0.35,
                                        color = "blue",
                                        fill="white")

ggplot2_temp

##ALL PLOTS---------------------------------------

allplots <- all_data %>% select("T_ground_c1","T_bottom_c1","T_top_c1", "soil_moisture_c1","T_ground_otc1","T_bottom_otc1","T_top_otc1",
                                "soil_moisture_otc1","T_ground_c2","T_bottom_c2","T_top_c2", "soil_moisture_c2","T_ground_otc2","T_bottom_otc2","T_top_otc2",
                                "soil_moisture_otc2", "datenew", "month", "day", "timenew", "hour", "min")

allplots_temp <- allplots %>% select(1:3,5:7,9:11,13:15,17:22)

allplots_temp_dif <- summarise(group_by(allplots_temp, hour, min),
                               t_top_dif_1 = T_top_otc1 - T_top_c1,
                               t_bottom_dif_1 = T_bottom_otc1 - T_bottom_c1, 
                               t_ground_dif_1 = T_ground_otc1 - T_ground_c1,
                               t_top_dif_2 = T_top_otc2 - T_top_c2,
                               t_bottom_dif_2 = T_bottom_otc2 - T_bottom_c2, 
                               t_ground_dif_2 = T_ground_otc2 - T_ground_c2)

allplots_temp_means <- summarise(group_by(allplots_temp_dif, hour, min),
                               t_top_dif_mean = (t_top_dif_1 + t_top_dif_2)/2,
                               t_bottom_dif_mean = (t_bottom_dif_1 + t_bottom_dif_2)/2,
                               t_ground_dif_mean = (t_ground_dif_1 + t_ground_dif_2)/2)

#No salen los errores standar
allplots_temp_24h <- summarise(group_by(allplots_temp_means, hour, min),
                               t_top_dif_mean = mean(t_top_dif_mean, na.rm = T),
                               t_top_dif_serr = s.err(t_top_dif_mean),
                               t_bottom_dif_mean = mean(t_bottom_dif_mean, na.rm = T),
                               t_bottom_dif_serr = s.err(t_bottom_dif_mean),
                               t_ground_dif_mean = mean(t_ground_dif_mean, na.rm = T), 
                               t_ground_dif_serr = s.err(t_ground_dif_mean))

ggallplots_temp <- ggplot(allplots_temp_24h, aes(x = hour)) + 
  geom_line(aes(y = t_top_dif_mean), color = "darkred") + 
  geom_line(aes(y = t_bottom_dif_mean), color="blue")+
  geom_line(aes(y = t_ground_dif_mean), color="#4CAF50")+
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("All plots") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )+
  geom_label(label="Top", x=23, y= 0,
             label.padding = unit(0.55, "lines"),
             label.size = 0.35,
             color = "darkred",
             fill="white") + 
  geom_label(label="Ground", x=22.5, y= 1.7,
             label.padding = unit(0.55, "lines"),
             label.size = 0.35,
             color = "#4CAF50",
             fill="white") + geom_label(label="Bottom", x=22.5, y= 2.4,
                                        label.padding = unit(0.55, "lines"),
                                        label.size = 0.35,
                                        color = "blue",
                                        fill="white")

ggallplots_temp
                            

#All plots day

allplots_temp_day <- merge(plot1_temp_day, plot2_temp_day)


ggttopday <- ggplot(allplots_temp_day, aes(x = datenew)) + 
  geom_line(aes(y = t_top_mean_otc1), color = "darkred") +
  geom_line(aes(y = t_top_mean_otc2), color = "darkred", linetype = "twodash") + 
  geom_line(aes(y = t_top_mean_c1), color="steelblue")+
  geom_line(aes(y = t_top_mean_c2), color="steelblue", linetype = "twodash") +
  theme_bw()+
  labs(x = " ", y =  "Mean temperature (ºC)")+ 
  ggtitle("Mean top temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )
ggttopday

ggtbottomday <- ggplot(allplots_temp_day, aes(x = datenew)) + 
  geom_line(aes(y = t_bottom_mean_otc1), color = "darkred") +
  geom_line(aes(y = t_bottom_mean_otc2), color = "darkred", linetype = "twodash") + 
  geom_line(aes(y = t_bottom_mean_c1), color="steelblue")+
  geom_line(aes(y = t_bottom_mean_c2), color="steelblue", linetype = "twodash") +
  theme_bw()+
  labs(x = "Days", y =  " ")+ 
  ggtitle("Mean bottom temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )
ggtbottomday

ggtgrounday <- ggplot(allplots_temp_day, aes(x = datenew)) + 
  geom_line(aes(y = t_ground_mean_otc1), color = "darkred") +
  geom_line(aes(y = t_ground_mean_otc2), color = "darkred", linetype = "twodash") + 
  geom_line(aes(y = t_ground_mean_c1), color="steelblue")+
  geom_line(aes(y = t_ground_mean_c2), color="steelblue", linetype = "twodash") +
  theme_bw()+
  labs(x = " ", y =  " ")+ 
  ggtitle("Mean ground temperature") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )
ggtgrounday


######## BOXPLOTS ########



#Visualización conjunta-------------------------------------------

ggarrange(ggc1, ggotc1, ggc2, ggotc2, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

ggarrange(ggttopday, ggtbottomday, ggtgrounday, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

ggarrange(ggplot1_temp, ggplot2_temp, ggallplots_temp, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

ggarrange(ggplot1_temp, ggplot2_temp, ggallplots_temp, ggttopday, ggtbottomday, ggtgrounday,
          labels = c("A" , "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)


# Datos ------------------------------
c1t <- paste(round(mean(c1$T_top_c1), digits = 2),  "±", round(s.err(c1$T_top_c1), digits = 2))
c1b <- paste(round(mean(c1$T_bottom_c1), digits = 2),  "±", round(s.err(c1$T_bottom_c1), digits = 2))
c1g <- paste(round(mean(c1$T_ground_c1), digits = 2),  "±", round(s.err(c1$T_ground_c1), digits = 2))
c1sm <- paste(round(mean(c1$soil_moisture_c1), digits = 2),  "±", round(s.err(c1$soil_moisture_c1), digits = 2))

otc1t <- paste(round(mean(otc1$T_top_otc1), digits = 2),  "±", round(s.err(otc1$T_top_otc1), digits = 2))
otc1b <- paste(round(mean(otc1$T_bottom_otc1), digits = 2),  "±", round(s.err(otc1$T_bottom_otc1), digits = 2))
otc1g <- paste(round(mean(otc1$T_ground_otc1), digits = 2),  "±", round(s.err(otc1$T_ground_otc1), digits = 2))
otc1sm <- paste(round(mean(otc1$soil_moisture_otc1), digits = 2),  "±", round(s.err(otc1$soil_moisture_otc1), digits = 2))

c2t <- paste(round(mean(c2$T_top_c2), digits = 2),  "±", round(s.err(c2$T_top_c2), digits = 2))
c2b <- paste(round(mean(c2$T_bottom_c2), digits = 2),  "±", round(s.err(c2$T_bottom_c2), digits = 2))
c2g <- paste(round(mean(c2$T_ground_c2), digits = 2),  "±", round(s.err(c2$T_ground_c2), digits = 2))
c2sm <- paste(round(mean(c2$soil_moisture_c2), digits = 2),  "±", round(s.err(c2$soil_moisture_c2), digits = 2))

otc2t <- paste(round(mean(otc2$T_top_otc2), digits = 2),  "±", round(s.err(otc2$T_top_otc2), digits = 2))
otc2b <- paste(round(mean(otc2$T_bottom_otc2), digits = 2),  "±", round(s.err(otc2$T_bottom_otc2), digits = 2))
otc2g <- paste(round(mean(otc2$T_ground_otc2), digits = 2),  "±", round(s.err(otc2$T_ground_otc2), digits = 2))
otc2sm <- paste(round(mean(otc2$soil_moisture_otc2), digits = 2),  "±", round(s.err(otc2$soil_moisture_otc2), digits = 2))

apt <- paste(round(mean(allplots_temp_24h$t_top_dif_mean), digits = 2),  "±", round(s.err(allplots_temp_24h$t_top_dif_mean), digits = 2))
apb <- paste(round(mean(allplots_temp_24h$t_bottom_dif_mean), digits = 2),  "±", round(s.err(allplots_temp_24h$t_bottom_dif_mean), digits = 2))
apg <- paste(round(mean(allplots_temp_24h$t_ground_dif_mean), digits = 2),  "±", round(s.err(allplots_temp_24h$t_ground_dif_mean), digits = 2))

p1t <- paste(round(mean(plot1_temp_24h$t_top_dif_mean), digits = 2),  "±", round(s.err(plot1_temp_24h$t_top_dif_mean), digits = 2))
p1b <- paste(round(mean(plot1_temp_24h$t_bottom_dif_mean), digits = 2),  "±", round(s.err(plot1_temp_24h$t_bottom_dif_mean), digits = 2))
p1g <- paste(round(mean(plot1_temp_24h$t_ground_dif_mean), digits = 2),  "±", round(s.err(plot1_temp_24h$t_ground_dif_mean), digits = 2))

p2t <- paste(round(mean(plot2_temp_24h$t_top_dif_mean), digits = 2),  "±", round(s.err(plot2_temp_24h$t_top_dif_mean), digits = 2))
p2b <- paste(round(mean(plot2_temp_24h$t_bottom_dif_mean), digits = 2),  "±", round(s.err(plot2_temp_24h$t_bottom_dif_mean), digits = 2))
p2g <- paste(round(mean(plot2_temp_24h$t_ground_dif_mean), digits = 2),  "±", round(s.err(plot2_temp_24h$t_ground_dif_mean), digits = 2))

TTOP <- c(c1t, otc1t, p1t, c2t, otc2t, p2t, apt)
TBOTTOM <- c(c1b, otc1b, p1b, c2b, otc2b, p2b, apb)
TGROUND <- c(c1g, otc1g, p1g, c2g, otc2g, p2g, apg)
SOILMOIST <- c(c1sm, otc1sm, "NA", c2sm, otc2sm, "NA","NA")

df <- data.frame(TTOP, TBOTTOM, TGROUND, SOILMOIST)
rownames(df) <- c("Control 1", "OTC1", "PLOT 1 diff", "Control 2", "OTC2", "PLOT 2 diff", "All plots diff")
colnames(df) <- c("Top temperature", "Bottom temperature", "Ground temperature", "Soil moisture")

df %>% write.csv("Results/data_20221004.csv")
