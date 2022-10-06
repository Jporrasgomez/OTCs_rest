


c2 <- read_delim("Data/data_94238924_2022_10_04_1.csv", 
                 ";", escape_double = FALSE, col_names = FALSE, 
                 trim_ws = TRUE)
colnames(c2) <- c("n", "date_time","time_zone", "T_ground","T_bottom","T_top",
                  "soil_moisture")
c2$treat <- "Control"
c2$plot <- "two"
c2 <- c2  %>% select(date_time, T_ground, T_bottom, T_top, soil_moisture, treat, plot)

c2[c('date', 'time')] <- str_split_fixed(c2$date_time, ' ', 2)
c2$datenew <- ymd(c2$date)
c2 <- subset(c2, datenew >= "2022-06-22")
c2$month <- month(c2$datenew, label = T)
c2$day <- day(c2$datenew)
c2$timenew <- hm(c2$time)
c2$hour <- hour(c2$timenew)
c2$min <- minute(c2$timenew)
hist(c2$T_bottom)
hist(c2$T_top)
hist(c2$T_ground)
hist(c2$soil_moisture)
summary(c2)


otc2 <-  read_delim("Data/data_94238922_2022_10_04_1.csv", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
colnames(otc2) <- c("n", "date_time","time_zone", "T_ground","T_bottom","T_top",
                    "soil_moisture")
otc2$treat <- "OTC"
otc2$plot <- "two"

otc2 <- otc2  %>% select(date_time, T_ground, T_bottom, T_top, soil_moisture, treat, plot)
otc2[c('date', 'time')] <- str_split_fixed(otc2$date_time, ' ', 2)
otc2$datenew <- ymd(otc2$date)
otc2 <- subset(otc2, datenew >= "2022-06-22")
otc2$month <- month(otc2$datenew, label = T)
otc2$day <- day(otc2$datenew)
otc2$timenew <- hm(otc2$time)
otc2$hour <- hour(otc2$timenew)
otc2$min <- minute(otc2$timenew)
hist(otc2$T_bottom)
hist(otc2$T_top)
hist(otc2$T_ground)
hist(otc2$soil_moisture)
summary(otc2)
str(otc2)




plot2 <- merge(c2, otc2, all = TRUE)


#Boxplot

plot2bp <- plot2 %>%
  select(date_time, T_ground, T_bottom, T_top, soil_moisture, treat, plot) %>%
  pivot_longer(cols = starts_with("T_"), names_to = "T_sensor", values_to = "Temperature")

treat_color <- c("OTC" = "red", "Control" = "blue")

ggboxtemp <- ggplot(plot2bp,
       aes(x = T_sensor,
           y = Temperature,
           color = treat)) +
  geom_boxplot() + 
  scale_color_manual(values = treat_color)+
  theme_minimal()



ggboxsoilm <- ggplot(plot2bp,
       aes(x = treat,
           y = soil_moisture,
           color = treat)) +
  geom_boxplot() + 
  scale_color_manual(values = treat_color)+
  theme_minimal()

ggboxtemp
ggboxsoilm

#All time series

c2graph <- subset(c2, "2022-07-01" <= datenew)
c2graph$datetimenew <- ymd_hm(c2graph$date_time)

ggc2 <- ggplot(c2graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom), group = 1, color = "blue")+
  geom_line(aes( y = T_ground), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture/40)), group = 1, color = "black")+
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
ggc2

otc2graph <- subset(otc2, "2022-07-01" <= datenew)
otc2graph$datetimenew <- ymd_hm(otc2graph$date_time)

ggotc2 <- ggplot(otc2graph, aes(x = datetimenew)) + 
  geom_line(aes( y = T_top), group = 1, color = "darkred")+
  geom_line(aes( y = T_bottom), group = 1, color = "blue")+
  geom_line(aes( y = T_ground), group = 1, color = "green")+
  geom_line((aes (y = soil_moisture/40)), group = 1, color = "black")+
  scale_y_continuous(name = "Temperature (Cº)",
                     sec.axis = sec_axis(~.*40, name=" ")) + 
  theme_bw()+
  labs(x = " ", y =  "Temperature (ºC")+ 
  ggtitle("OTC 2") +
  theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.title.y.right = element_text(color = "black", size = 12, face = "bold")
  )
ggotc2



