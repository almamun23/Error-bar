getwd()
setwd("E:/Kuakata/Temperature")
x2=read.csv("Temperature.csv")
attach(x2)
str(x2)
df.summary <- x2 %>%
  group_by(Station) %>%
  summarise(
    sd = sd(Temperature, na.rm = TRUE),
    Temperature = mean(Temperature)
  )
df.summary
#######################
ggplot(df.summary, aes(Station, Temperature)) +
  geom_line(aes(group = 1)) +
   
  geom_errorbar( aes(ymin = Temperature-sd, ymax = Temperature+sd),width = 0.2) +
  geom_point(size = 2)
##################################
ggplot(x2, aes(Station, Temperature)) +
  geom_jitter( position = position_jitter(0.2), color = "darkblue") + 
  geom_line(aes(group = 1), data = df.summary,color="darkblue") +
  geom_errorbar(
    aes(ymin = Temperature-sd, ymax = Temperature+sd),
    data = df.summary, width = 0.2,colour="darkblue") +
  geom_point(data = df.summary, size = 2)+
  labs(y="Temperature (\u00B0C)", x="Station")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5,size = 14))+ 
  theme(axis.text.x = element_text(face="bold", size=14 ),axis.text.y = element_text(face="bold", size=14))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(breaks = seq(22,32,by=1))+
  ##ggtitle("Monthly Sea Surface Temperature(°C)\n in 2020")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()



ggline(x2,x="Station",y="Temperature",point.size = 1.5,add = c("mean_se","jitter"))+
  theme_bw()
####################################################################density plot
ggplot( x3,aes(x= Salinity,y=StationA))+
  geom_joy(scale=2,aes(fill=StationA),alpha=2,lwd=0.6)+
  ####geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  ###scale_fill_viridis_c(name = "SST", option = "C")+
  #labs(x="Temperature (\u00B0C)", y="Station")+
  labs(x="Salinity(PSU)",y="Station")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5,size = 14))+ 
  theme(axis.text.x = element_text(face="bold", size=14 ),axis.text.y = element_text(face="bold", size=14))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  #scale_x_continuous(breaks = seq(22,32,by=1))+
  ####ggtitle("Monthly Sea Surface Temperature(°C)\n in 2020")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none") 
 
  theme_bw()+
 
  
getwd()
setwd("E:/Kuakata/Temperature")
xy=read.csv("Temperature.csv")
attach(xy)
str(xy)
ggplot(xy,aes(x=Salinity.PSU.,y=Temperature))+geom_point() 
geom_smooth(method ="loess")

ggline(x1,x="Station",y="Temperature", point.size = 1.5,point.color = "black",add = "mean_se")+
  theme_bw()

getwd()
setwd("E:/Kuakata/Salinity")
x3=read.csv("Salinity.csv")
attach(x3)
str(x3)
df.summary1 <- x3 %>%
  group_by(Station) %>%
  summarise(
    sd = sd(Salinity, na.rm = TRUE),
    Salinity = mean(Salinity)
  )
df.summary1
ggplot(x3, aes(Station,Salinity)) +
  geom_col(data = df.summary1, fill =NA, color = "darkgreen") +
  geom_jitter( position = position_jitter(0.2), color = "darkgreen") + 
  geom_errorbar( aes(ymin = Salinity-sd, ymax = Salinity+sd), 
                 data = df.summary1, width = 0.5,colour="darkgreen")+
  labs(y=" Salinity(PSU)", x="Station")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5,size = 14))+ 
  theme(axis.text.x = element_text(face="bold", size=14 ),axis.text.y = element_text(face="bold", size=14))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(breaks = seq(0,7,by=0.7))+
  ####ggtitle("Monthly Sea Surface Temperature(°C)\n in 2020")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
  theme_minimal()
##########################
  getwd()
  setwd("E:/Kuakata/Temperature")
  x4=read.csv("mutual.csv")
  attach(x4)
  str(x4)
  ggplot(x4,aes(x=Temperature,y=Salinity))+geom_point()+ 
    geom_smooth(method = "loess",se=T)+
    theme_bw()+
    annotate("text",x=27.5,y=6,label="R-squared:  0.5423\n p-value: 0.00004077",size=5)+
    scale_y_continuous(breaks = seq(3,7,by=0.5))+
    scale_x_continuous(breaks = seq(23.5,31,by=0.8))+
    labs(y=" Salinity(PSU)", x="Temperature(°C)")
  cor(Temperature,Salinity)
  lmod=lm(Salinity~Temperature,x4)
  summary(lmod)
  ###########################
  getwd()
  setwd("E:/Kuakata/Temperature")
  x8=read.csv("kuakata5.csv")
  attach(x8)
  str(x8)
  ggplot(x8,aes(x=Temperature,y=Salinity))+geom_point()+ 
    geom_smooth(method = "lm",se=T)+
    theme_bw()+
    annotate("text",x=24.877,y=6.5,label=" R-squared:  0.03198\n p-value: 0.4639",size=5)+
    scale_y_continuous(breaks = seq(3,7,by=0.8))+
    scale_x_continuous(breaks = seq(23,26,by=0.5))+
    labs(y=" Salinity(PSU)", x="Temperature(°C)")
  lmod=lm(Salinity~Temperature,x8)
  summary(lmod)
  cor(Salinity,Temperature)
  
  getwd()
  setwd("E:/Kuakata/Salinity")
  x9=read.csv("DO.csv")
  attach(x9)
  str(x9) 
  class(x9)
 tem=data.matrix(x9)
 rownames(tem)=x9$Station
 tem=tem[,-1]
 pheatmap(tem)
 ##########
 getwd()
 setwd("E:/Kuakata/Salinity")
 x10=read.csv("ok.csv")
 attach(x10)
 str(x10) 
 class(x10)
  ################# radar chart
 getwd()
 setwd("E:/Kuakata/Salinity")
 x10=read.csv("ok.csv")
 attach(x10)
 str(x10) 
 class(x10)
 ggradar(x10,
         values.radar = c(24,47,80),
         grid.min = 10, grid.mid = 47, grid.max = 80,
         group.line.width = 1, 
         group.point.size = 3,
         gridline.max.linetype =5,
         group.colours = c("red",'darkblue'),
         # Background and grid lines
         background.circle.colour = "white",
         gridline.mid.colour = "green",
         legend.position = "bottom"
 ) 
 
 