library(easypackages)
libraries("tidyverse","ggpmisc","xlsx")
f<-function(x){return(x/60)}
SleepHours<-c(8+f(34),10,10+f(20),9,8,8,8+f(20),7+f(30),6+f(43),5+f(20),7+f(50),7+f(12),7+f(50),6+f(10),8+f(20),8+f(42),8+f(50),8+f(08))
PCHours<-c(8+f(32),4+f(58),6+f(19),6+f(30),8+f(57),2+f(42),9+f(25),11+f(33),4+f(5),10+f(35),9+f(16),13+f(6),8+f(53),8+f(30),4+f(6),3+f(20),12+f(45),6+f(52))
Date<-c("04-01-2024","04-02-2024","04-03-2024","04-04-2024","04-05-2024","04-06-2024","04-07-2024","04-08-2024","04-09-2024","04-10-2024","04-11-2024","04-12-2024","04-13-2024","04-14-2024","04-15-2024","04-16-2024","04-17-2024","04-18-2024")
HW<-c(F,T,T,T,F,F,F,F,T,T,T,F,F,F,F,T,T,T)
Data2<-data.frame(Day=1:length(SleepHours),Sleep=c(SleepHours),PC=c(PCHours),HW=c(HW),Date=c(Date))
Data5<-data.frame(Day=1:length(SleepHours),Date=c(Date),Sleep=c(SleepHours),PC=c(PCHours),HW=c(HW))
Data<-data.frame(Day=1:length(SleepHours),Sleep=c(SleepHours),PC=c(PCHours))
Data_Long<-Data%>%
  pivot_longer(cols = c(2,3),names_to = "Activity",values_to = "Hours")
Data_Long%>%
  ggplot(aes(x=Day,y=Hours,color = Activity))+geom_col(aes(fill=Activity))+labs(title="Amount of Sleep vs Amount of Screen Time (Hours)")
t.test(Hours~Activity,data=Data_Long)
t.test(SleepHours,PCHours,data=Data,)
cor.test(SleepHours,PCHours,data=Data,method = "pearson")

write.xlsx(Data5,"C:/Users/katie/Downloads/Sleep_Data.xlsx",sheetName = "Data",col.names = T,row.names = T,append = F)

#Scaled
Data3<-scale(Data2%>%select(Sleep,PC,HW))
Data3<-cbind(Data3,Data2$Day)
colnames(Data3)[4]<-"Day"
Data_Long_Scaled<-as.data.frame(Data3)%>%
  pivot_longer(cols = c(1:3),names_to = "Activity",values_to = "Value")
Data_Long_Scaled%>%
  ggplot(aes(x=Day,y=Value,fill = Activity))+scale_fill_manual(values = c("limegreen","indianred1",  "darkturquoise"))+geom_col(aes(fill=Activity))+labs(title="Activity Z-score by Day")

write.xlsx(Data3,"C:/Users/katie/Downloads/Sleep_Data.xlsx",sheetName = "Std Data",col.names = T,row.names = T,append = T)

Data4<-scale(Data2%>%select(Sleep,PC))
Data4<-cbind(Data4,Data2$Day)
colnames(Data4)[3]<-"Day"
Data_Long_Scaled<-as.data.frame(Data3)%>%
  pivot_longer(cols = c(1:2),names_to = "Activity",values_to = "Value")
Data_Long_Scaled%>%
  ggplot(aes(x=Day,y=Value,color = Activity))+geom_col(aes(fill=Activity))+labs(title="Activity Z-score by Day")

Data4<-as.data.frame(Data4)
Data4%>%
  ggplot(aes(x=PC,y=Sleep))+geom_line()+labs(title="Amount of Sleep vs Amount of Screen Time (Z-score)")+xlab("PC Time Score")+ylab("Sleep Score")+stat_poly_eq(method = "lm",formula = y~poly(x,3),use_label(c("eq", "R2")),coef.digits = 4)+stat_poly_line(formula = y~poly(x,3),method = "lm",se=F)

write.xlsx(Data4,"C:/Users/katie/Downloads/Sleep_Data.xlsx",sheetName = "Std Data 2",col.names = T,row.names = T,append = T)

cor.test(Data4$Sleep,Data4$PC,data=Data,method = "pearson")
