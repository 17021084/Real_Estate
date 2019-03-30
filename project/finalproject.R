setwd("C:/Users/DoQuangTrung/Desktop/project")



# đọc dữ liệu
preData_train <- read.csv("train.csv")
preDatedat_test <- read.csv("test.csv")

#
# md <- lm(SalePrice~. , data= preData_train)

#

# xem qua cấu trúc của predata 
str(preData_train) 


# xử lí , lọc các biến numeric ------------------------------------------
# xử dụng mô hình hồi quy tuyến tính nên chỉ dùng biến numeric

library(dplyr)
library(tidyverse)

train <- select_if( preData_train , is.numeric)  # có thêm feature saleProduc
test <- select_if(preDatedat_test , is.numeric) 

#xóa cột id  vì ko cần thiết
train$Id <-NULL
test$Id<-NULL

# ép lại kiểu factor cho các biến
train$MSSubClass  <- factor(train$MSSubClass)
train$OverallQual <- factor(train$OverallQual)
train$OverallCond <- factor(train$OverallCond) 
#------
test$MSSubClass  <- factor(test$MSSubClass)
test$OverallQual <- factor(test$OverallQual)
test$OverallCond <- factor(test$OverallCond) 


# kiểm tra lại data set 
str(train)





#-----------------------------------------------------------------------
# THống kê mô tả các features của bộ train 
library(grid)
library(ggplot2)

#  max min, median , mean , 1st n 3rd percentile (phân vị)
summary(train)

######################################Vẽ đồ thị###################################

attach(train) # đính kèm để ko cần phải tên dataset $ tên cột mà ghi tên cột luôn

#--------------------------------------------------------------------
#MSSubclass : biến ordinal : ko dùng cho mô hình

# ggplot(train, aes(MSSubClass))+ geom_p() + theme_bw()
# table(MSSubClass)

ggplot(train, aes(x = "", y="", fill = MSSubClass)) + 
  geom_bar(width = 1, stat = "identity") +
  #theme(axis.line = element_blank(), 
  #    plot.title = element_text(hjust=0.5)) + 
  
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of MSSubClass",
       caption="Identifies the type of dwelling involved in the sale."
  )+ coord_polar(theta = "y", start=0)




#LotFrontage


boxplot(LotFrontage , main =" biểu đồ box plot của LotFrontAge", col="maroon" )


# 
#---------------------------------------------------------

#LotArea

ggplot(train,aes(LotArea)) + geom_histogram(fill='maroon')+ theme_bw()




#---------------------------------------------
# overall qual 
#overCond



plot1<-ggplot(train,aes(OverallQual)) + geom_bar(fill ='maroon') + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Rates the overall material and finish of the house ",
    title=" Đồ thị phân bố của OverAll Quaity",
    y= "Frequency",
    x="value"
    )

plot2<-ggplot(train,aes(OverallCond)) + geom_bar(fill ='maroon') + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Rates the overall condition of the house ",
    title=" Đồ thị phân bố của OverAll OverallCond ",
    y= "Frequency",
    x="value"
  )

pushViewport(viewport(layout = grid.layout(1, 2)))

print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))



#-----------------------------------------------------------------

# YearBuilt: Original construction date
#
# YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)



ybplot<-ggplot(train,aes(YearBuilt))+ geom_histogram(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Original construction year ",
    title=" Đồ thị của  YearBuid  ",
    y= "Frequency",
    x="Year"
  )

yraplot<-ggplot(train,aes(YearRemodAdd))+ geom_histogram(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Remodel date \n(same as construction date if no  remodeling or additions) ",
    title=" Đồ thị của  YearRemodAdd  ",
    y= "Frequency",
    x="Year"
  )
# sẽ để song song
pushViewport(viewport(layout = grid.layout(1, 2)))

print(ybplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(yraplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))



#--------------------------------------------------------------------------------------------
# YrSold : năm bán
# MoSold : tháng bán
# đồ thị group của tháng và năm


ggplot(train,aes(YrSold))+ geom_bar(aes(fill=factor(MoSold)),position="dodge" )+
  
  theme_bw() + theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title=" Đồ thị của Date Sold  ",
    subtitle = " month group Year Sold ",
    fill="Month",
    y= "Frequency",
    x="Year"
  )

#---------------------------------------------------------------------------------------------
#GarageYrBlt  Year garage was built : năm của gara đc xây

ggplot(train,aes(GarageYrBlt))+ geom_histogram(fill ='maroon' )+
  
  theme_bw() + theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title=" Đồ thị của GarageYrBlt  ",
    subtitle = " Year garage was built  ",
    y= "Frequency",
    x="Year"
  )

# 
# GarageCars: Size of garage in car capacity : số xe có thể dựng trung 1 nhà ga
# 
# GarageArea: Size of garage in square feet

# gar <- filter( train,GarageCars>0)


ggplot(gar,aes(x='', y=GarageArea))+ geom_boxplot(aes(fill=factor(GarageCars)),position="dodge" )+

  theme_bw() + theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Garage Area and GarageCars ",
    subtitle = "    ",
    fill="",
    y= "Diện tích garage",
    x=""
  )





#---------------------------------------------------------------------------------------------

# WoodDeckSF: Wood deck area in square feet
# 
# OpenPorchSF: Open porch area in square feet
# 
# EnclosedPorch: Enclosed porch area in square feet
# 
# ThreeSsnPorch: Three season porch area in square feet
# 
# ScreenPorch: Screen porch area in square feet
# 
# PoolArea: Pool area in square feet



wd<-ggplot(train,aes(WoodDeckSF))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = "  Wood deck area in square feet ",
    title="   ",
    y= "density",
    x="WoodDeckSF"
  )
opsf <-ggplot(train,aes(OpenPorchSF))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Open porch area in square feet ",
    title="   ",
    y= "density",
    x="OpenPorchSF"
  )


ep <-ggplot(train,aes(EnclosedPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Enclosed porch area in square feet ",
    title="   ",
    y= "density",
    x="EnclosedPorch"
  )

tsp <-ggplot(train,aes(ThreeSsnPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = "  Three season porch area in square feet ",
    title="   ",
    y= "density",
    x="ThreeSsnPorch"
  )

sp <-ggplot(train,aes(ScreenPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Screen porch area in square feet ",
    title="   ",
    y= "density",
    x="ScreenPorch"
  )
pa<-ggplot(train,aes(PoolArea))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Pool area in square feet ",
    title="   ",
    y= "density",
    x="PoolArea"
  )

pushViewport(viewport(layout = grid.layout(3, 2)))

print(wd, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(opsf, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ep, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(tsp, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(sp, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(pa, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))





#---------------------------------------------------------------------------------------------

#MasVnrArea: Masonry veneer area in square feet


#BsmtFinSF1: Type 1 finished square feet
# BsmtFinSF2: Type 2 finished square feet
# 
# BsmtUnfSF: Unfinished square feet of basement area
# 
# TotalBsmtSF: Total square feet of basement area


####
# 1stFlrSF: First Floor square feet
# 
# 2ndFlrSF: Second floor square feet
# 
# LowQualFinSF: Low quality finished square feet (all floors)
# 
# GrLivArea: Above grade (ground) living area square feet
# 
# BsmtFullBath: Basement full bathrooms
# 
# BsmtHalfBath: Basement half bathrooms
# 
# FullBath: Full bathrooms above grade
# 
# HalfBath: Half baths above grade
# 
# Bedroom: Bedrooms above grade (does NOT include basement bedrooms)
# 
# Kitchen: Kitchens above grade
# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
# Fireplaces: Number of fireplaces
# 









