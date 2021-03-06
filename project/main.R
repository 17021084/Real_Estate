setwd("C:/Users/DoQuangTrung/Desktop/project")


# đọc dữ liệu
data <- read.csv("build_mode.csv")

# xem qua cấu trúc của data 
str(data) 


# xử lí , lọc các biến numeric ------------------------------------------
# xử dụng mô hình hồi quy tuyến tính nên chỉ dùng biến numeric

library(dplyr)
library(tidyverse)
library(psych)

# ép lại kiểu factor cho các features
data$MSSubClass  <- factor(data$MSSubClass)
data$OverallQual <- factor(data$OverallQual)
data$OverallCond <- factor(data$OverallCond) 
data$FullBath <-factor(data$FullBath)
data$BsmtFullBath <- factor(data$BsmtFullBath)
data$FullBath <-factor(data$FullBath)
data$HalfBath <- factor(data$HalfBath)
data$BedroomAbvGr <- factor(data$BedroomAbvGr)
data$KitchenAbvGr <- factor(data$KitchenAbvGr)
data$TotRmsAbvGrd <- factor(data$TotRmsAbvGrd)
data$Fireplaces <- factor(data$Fireplaces)
data$MoSold<-factor(data$MoSold)
# data$YearBuilt<-factor(data$YearBuilt)
# data$YearRemodAdd<-factor(data$YearRemodAdd)
# data$YrSold <-factor(data$YrSold)

# kiểm tra lại data set 
str(data)


# 
# 


#-----------------------------------------------------------------------
# THống kê mô tả các features của bộ data 
library(grid)
library(ggplot2)

#  max min, median , mean , 1st n 3rd percentile (phân vị)
summary(data)

######################################Vẽ đồ thị###################################

attach(data) # đính kèm để ko cần phải tên dataset $ tên cột mà ghi tên cột luôn

#--------------------------------------------------------------------
#MSSubclass : biến nominal ( phân loại)

ggplot(data, aes(x = "", y="", fill = MSSubClass)) + 
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

ggplot(data,aes(LotArea)) + geom_density(fill='maroon')+ theme_bw()




#---------------------------------------------
# overall qual 
#overCond

plot1<-ggplot(data,aes(OverallQual)) + geom_bar(fill ='maroon') + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Rates the overall material and finish of the house ",
    title=" Đồ thị phân bố\n của OverAll Quaity",
    y= "",
    x="rate"
  )

plot2<-ggplot(data,aes(OverallCond)) + geom_bar(fill ='maroon') + 
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Rates the overall condition of the house ",
    title=" Đồ thị phân bố của \n OverAll OverallCond ",
    y= "",
    x="rate"
  )

pushViewport(viewport(layout = grid.layout(1, 2)))

print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))



#-----------------------------------------------------------------

# YearBuilt: Original construction date
#
# YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)



ybplot<-ggplot(data,aes(YearBuilt))+ geom_bar(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Original construction year ",
    title=" Đồ thị của  YearBuid  ",
    y= "",
    x="Year"
  )

yraplot<-ggplot(data,aes(YearRemodAdd))+ geom_bar(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Remodel date \n(same as construction date if no  remodeling or additions) ",
    title=" Đồ thị của  YearRemodAdd  ",
    y= "",
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


ggplot(data,aes(YrSold))+ geom_bar(aes(fill=factor(MoSold)),position="dodge" )+
  
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

ggplot(data,aes(GarageYrBlt))+ geom_histogram(fill ='maroon' )+
  
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



ggplot(data,aes(x='', y=GarageArea))+ geom_boxplot(aes(fill=factor(GarageCars)),position="dodge" )+
  
  theme_bw() + theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Garage Area and GarageCars ",
    subtitle = " diện tích gara \n với số xe chứa đc",
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



wd<-ggplot(data,aes(WoodDeckSF))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = "  Wood deck area \n in square feet ",
    title="WoodDeckSF",
    y= "",
    x="WoodDeckSF"
  )
opsf <-ggplot(data,aes(OpenPorchSF))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Open porch area \n in square feet ",
    title="OpenPorchSF",
    y= "",
    x="OpenPorchSF"
  )


ep <-ggplot(data,aes(EnclosedPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Enclosed porch \n area in square feet ",
    title="EnclosedPorch",
    y= "",
    x="EnclosedPorch"
  )

tsp <- ggplot(data,aes(ThreeSsnPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = "  Three season \n porch area in square feet ",
    title=" ThreeSsnPorch",
    y= "",
    x="ThreeSsnPorch"
  )

sp <-ggplot(data,aes(ScreenPorch))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Screen porch \n area in square feet ",
    title="ScreenPorch",
    y= "",
    x="ScreenPorch"
  )
pa<-ggplot(data,aes(PoolArea))+ geom_density(fill ='maroon')+theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    subtitle = " Pool area in\n square feet ",
    title=" PoolArea ",
    y= "density",
    x="PoolArea"
  )


#MasVnrArea: Masonry veneer area in square feet  : độ dày 

mva<- ggplot(data,aes(MasVnrArea)) + geom_density(fill="maroon")+
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


stfloor<-ggplot(data,aes(X1stFlrSF)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title=" First Floor \nsquare feet   ",
    subtitle = " Diên tích sàn tầng1 (dv:feet^2) ",
    y= "",
    x="1st Floor area"
  )


ndfloor<-ggplot(data,aes(X2ndFlrSF)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Second floor \n square feet  ",
    subtitle = " Diên tích sàn tầng 2 tường (dv:feet^2) ",
    y= "",
    x="Floor area"
  )

pushViewport(viewport(layout = grid.layout(2, 2)))

print(wd, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(opsf, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ep, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(tsp, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(sp, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pa, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(stfloor, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(ndfloor, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(mva, vp = viewport(layout.pos.row = 3, layout.pos.col = 3))


vars=cbind(WoodDeckSF,OpenPorchSF,EnclosedPorch,ThreeSsnPorch,ScreenPorch,PoolArea,X1stFlrSF,X2ndFlrSF,SalePrice)
pairs.panels(vars)# tạo ma trận tương quan
corr.test(vars)

#-----------------------------------------------------------------------------------

#BsmtFinSF1: Type 1 finished square feet  
# BsmtFinType1: Rating of basement finished area 

bfsf1<- ggplot(data,aes(BsmtFinSF1)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="Type 1 finished\n  square feet",
    subtitle = " Diện tích hoàn thành\n tầng hầm loại 1 ",
    y= "",
    x="diện tích"
  )


bft1<-ggplot(data,aes(BsmtFinType1)) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="Rating",
    subtitle = "Đánh giá hoàn thành \n tầng hầm loại 1 ",
    y= "",
    x="Đánh giá"
  )

# BsmtFinSF2: Type 2 finished square feet ##
# BsmtFinType2: Rating of basement finished area (if multiple types)


bfsf2<- ggplot(data,aes(BsmtFinSF2)) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="Type 2 finished \n square feet",
    subtitle = " Diện tích hoàn thành \ntầng hầm loại 2 ",
    y= "",
    x="diện tích"
  )

bft2<-ggplot(data,aes(BsmtFinType2)) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="rating",
    subtitle = "đánh giá hoàn thành \n tầng hầm loại 2 ",
    y= "",
    x="Đánh giá"
  )

pushViewport(viewport(layout = grid.layout(2, 2)))

print(bfsf1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(bft1, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(bfsf2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(bft2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

vars=cbind(BsmtFinSF1,BsmtFinSF2,SalePrice)
pairs.panels(vars)# tạo ma trận tương quan
corr.test(vars)


#--------------------------------------------------------------------------------

# LowQualFinSF: Low quality finished square feet (all floors)

ggplot(data,aes( LowQualFinSF ) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title="  Second floor square feet  ",
    subtitle = " Diên tích sàn tầng 2 tường (dv:feet^2) ",
    y= "",
    x="Quanlity"
  )
summary(LowQualFinSF)

# 
# GrLivArea: Above grade (ground) living area square feet
# 

ggplot(data,aes( GrLivArea ) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtFullBath: Basement full bathrooms 
#
ggplot(data,aes( BsmtFullBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtHalfBath: Basement half bathrooms
# 
ggplot(data,aes( BsmtHalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# FullBath: Full bathrooms above grade
#
ggplot(data,aes( BsmtHalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# HalfBath: Half baths above grade
# 

ggplot(data,aes( HalfBath ) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# BedroomAbvGr: Bedrooms above grade (does NOT include basement bedrooms)
# 
ggplot(data,aes( BedroomAbvGr) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# Kitchen: Kitchens above grade


ggplot(data,aes( KitchenAbvGr) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)

ggplot(data,aes( TotRmsAbvGrd) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Fireplaces: Number of fireplaces
# 
ggplot(data,aes( Fireplaces) ) + geom_bar(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# BsmtUnfSF: Unfinished square feet of basement area
# 

ggplot(data,aes( BsmtUnfSF) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# TotalBsmtSF: Total square feet of basement area
ggplot(data,aes( TotalBsmtSF) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# ----------------------------------------------------------------------------
ggplot(data,aes( SalePrice) ) + geom_density(fill="maroon")+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(data,aes(x='', y=SalePrice) ) + geom_boxplot(fill="maroon" , notch = T, notchwidth = 0.1)+
  theme_bw() +
  theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#------------------------------------------------------------------------------

summary(data)



# Chạy mô hình 
# 
set.seed(14) # chia radom dể test
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train.data <- data [ind == 1, ]
test.data <- data[ind == 2, ]

dim(train.data)
dim(test.data)


md <- lm(SalePrice~. , data= train.data)
summary(md)
plot(md)

# md$fitted.values[1:37]



#Đánh giá mô hình trên tập dữ liệu test
table( test.data$OverallQual )
table( train.data$OverallQual )

pre <- predict(md,  newdata=test.data)


plot(test.data$SalePrice, pre, xlab = "Observed", ylab = "Prediction")

abline(a = 0, b = 1)
plot(md) #dia




# giảm chiều dữ liệu :
# chọn mô hình mới md2
md2<-step(md, direction="backward")

# SalePrice ~PoolArea+ThreeSsnPorch+BsmtFinSF1+YearRemodAdd +HalfBath+BsmtFinType1+MoSold+
#   MasVnrArea+ LowQualFinSF +KitchenAbvGr +LotFrontage+ TotRmsAbvGrd BsmtFullBath + ScreenPorch +GarageCars+
#   YearBuilt +LotArea+OverallCond+BedroomAbvGr+X1stFlrSF + MSSubClass+FullBath+X2ndFlrSF+Fireplaces+OverallQual

# summary(md2)
# plot (md2)



#tương quan đa biến

library(psych)
vars=cbind(data)
pairs.panels(vars)# tạo ma trận tương quan
corr.test(vars)


