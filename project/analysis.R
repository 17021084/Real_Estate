setwd("C:/Users/DoQuangTrung/Desktop/project")
data_set <- read.csv("real_estate.csv")

dat <- read.csv("train.csv")
str(dat)
dat$MSSubClass <- factor(dat$MSSubClass)
dat$OverallQual <- factor(dat$OverallQual)
dat$OverallCond <-factor(dat$OverallCond)

# chia tập
train <- data_set[1:600,]
test <- data_set[601:1460 ,]



# công thưc 
# biến đầu vào có thể là định tính định lượng, nhưng phải để về factor
train$OverallCond<-factor(train$OverallCond)

myfomula <- SalePrice~.

  # SalePrice ~(LotArea	+OverallQual+	OverallCond+	YearBuilt	+HalfBath+BedroomAbvGr+	KitchenAbvGr+	GarageCars+	WoodDeckSF	+OpenPorchSF	+EnclosedPorch	+SsnPorch	+ScreenPorch	+ PoolArea+	MiscVal	+MoSold)



model <- lm(myfomula , data = train)
print(model)
summary(model)


md1 <- lm(SalePrice~ScreenPorch+YearBuilt+LotArea+OverallQual , data =train)
summary(md1)
anova(md1)
plot(md1)


#Hế số tương quan , đo sự tương quan  giữa các biến liên tục

# hệ số tương quan.r   (hệ số xác định Rsquar =r^2)
attach(data_set)
cor(ScreenPorch,SalePrice , use="complete.obs")
cor.test(ScreenPorch,SalePrice )
# kết quả  :p value hơi cao, ko có nhiều ý nghĩ thống kê,
# cor tính ra 0.0315 rất thấp , 2 thằng ko liên quan lắm
# do đó hệ số xác định Rsquare =  0.0315^2 =0.0009945899  rất thấp

#tương quan đa biến

library(psych)
vars=cbind(ScreenPorch,YearBuilt,LotArea,OverallQual,SalePrice)
pairs.panels(vars)# tạo ma trận tương quan

corr.test(vars) # kiểm tra ý nghãi thống kê
# kết quả:
# 1 ma trận tương quan 
# 2 số mẫu
# 3 xác xuất

plot(model)
model2 <- step(model ,direction="backward")
