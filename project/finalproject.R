setwd("C:/Users/DoQuangTrung/Desktop/project")



# đọc dữ liệu
preData_train <- read.csv("train.csv")
preDatedat_test <- read.csv("test.csv")



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
write.csv(train , "build_mode.csv")
write.csv(test , 'apply_model.csv')


# ép lại kiểu factor cho các biến
train$MSSubClass  <- factor(train$MSSubClass)
train$OverallQual <- factor(train$OverallQual)
train$OverallCond <- factor(train$OverallCond) 
train$FullBath <-factor(train$FullBath)
train$BsmtFullBath <- factor(train$BsmtFullBath)
train$FullBath <-factor(train$FullBath)
train$HalfBath <- factor(train$HalfBath)
train$BedroomAbvGr <- factor(train$BedroomAbvGr)
train$KitchenAbvGr <- factor(train$KitchenAbvGr)
train$TotRmsAbvGrd <- factor(train$TotRmsAbvGrd)
train$Fireplaces <- factor(train$Fireplaces)
#------
# test$MSSubClass  <- factor(test$MSSubClass)
# test$OverallQual <- factor(test$OverallQual)
# test$OverallCond <- factor(test$OverallCond) 


# kiểm tra lại data set 
# str(train)
# 
# 

md <- lm(SalePrice~. , data= train)
summary(md)
Model2<-step(md, direction="backward")
summary(md)

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

ggplot(train,aes(LotArea)) + geom_density(fill='maroon')+ theme_bw()




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



ggplot(train,aes(x='', y=GarageArea))+ geom_boxplot(aes(fill=factor(GarageCars)),position="dodge" )+

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
    y= "",
    x="WoodDeckSF"
  )
opsf <-ggplot(train,aes(OpenPorchSF))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Open porch area in square feet ",
    title="   ",
    y= "",
    x="OpenPorchSF"
  )


ep <-ggplot(train,aes(EnclosedPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Enclosed porch area in square feet ",
    title="   ",
    y= "",
    x="EnclosedPorch"
  )

tsp <- ggplot(train,aes(ThreeSsnPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = "  Three season \n porch area in square feet ",
    title="   ",
    y= "",
    x="ThreeSsnPorch"
  )

sp <-ggplot(train,aes(ScreenPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Screen porch \n area in square feet ",
    title="   ",
    y= "",
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


#MasVnrArea: Masonry veneer area in square feet  : độ dày 

mva<- ggplot(train,aes(MasVnrArea)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Masonry veneer area   ",
    subtitle = " Diên tích phần lát \n tường tính theo feet^2 ",
     y= "",
    x="MasVnrArea"
  )


####
# 1stFlrSF: First Floor square feet
# 
# 2ndFlrSF: Second floor square feet


stfloor<-ggplot(train,aes(X1stFlrSF)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title=" First Floor square feet   ",
    subtitle = " Diên tích sàn tầng1 (dv:feet^2) ",
    y= "",
    x="1st Floor area"
  )


ndfloor<-ggplot(train,aes(X2ndFlrSF)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Second floor square feet  ",
    subtitle = " Diên tích sàn tầng 2 tường (dv:feet^2) ",
    y= "",
    x="Floor area"
  )

pushViewport(viewport(layout = grid.layout(3, 3)))

print(wd, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(opsf, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ep, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(tsp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(sp, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(pa, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
print(stfloor, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(ndfloor, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
print(mva, vp = viewport(layout.pos.row = 3, layout.pos.col = 3))


#BsmtFinSF1: Type 1 finished square feet  (bỏ)

# BsmtFinSF2: Type 2 finished square feet (bỏ)
# 
# 

#--------------------------------------------------------------------------------

# LowQualFinSF: Low quality finished square feet (all floors)

ggplot(train,aes( FullBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Second floor square feet  ",
    subtitle = " Diên tích sàn tầng 2 tường (dv:feet^2) ",
    y= "",
    x="Quanlity"
  )




# 
# GrLivArea: Above grade (ground) living area square feet
# 

ggplot(train,aes( GrLivArea ) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtFullBath: Basement full bathrooms 
#
ggplot(train,aes( BsmtFullBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtHalfBath: Basement half bathrooms
# 
ggplot(train,aes( BsmtHalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# FullBath: Full bathrooms above grade
#
ggplot(train,aes( BsmtHalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# HalfBath: Half baths above grade
# 

ggplot(train,aes( HalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# BedroomAbvGr: Bedrooms above grade (does NOT include basement bedrooms)
# 
ggplot(train,aes( BedroomAbvGr) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# Kitchen: Kitchens above grade


ggplot(train,aes( KitchenAbvGr) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)

ggplot(train,aes( TotRmsAbvGrd) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Fireplaces: Number of fireplaces
# 
ggplot(train,aes( Fireplaces) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtUnfSF: Unfinished square feet of basement area
# 

ggplot(train,aes( BsmtUnfSF) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# TotalBsmtSF: Total square feet of basement area
ggplot(train,aes( TotalBsmtSF) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())







