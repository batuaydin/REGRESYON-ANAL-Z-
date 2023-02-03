x1 <- data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`# x değerlerini atar
y1 <-data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)`  # y değerlerini atar.



#Normallik testi

attach(data_ex_2_8_Shelf_Stocking_)
install.packages("dplyr")
install.packages("ggpubr")
library(dplyr)
library(ggpubr)
ggdensity(y1, main= "Time",xlab = "Time")
ggqqplot(y1)
boxplot((data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)`))
boxplot(data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`)
hist(data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`)
hist(data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)`)
shapiro.test(y1) # Eğer veri seti 50 den küçük olduğu shapiro wilk testi
shapiro.test(data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)`)
shapiro.test(data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`)
#Verimiz 50’den küçük olduğu için normallik testini shapiro testiyle yapıyoruz.
#p-value değeri 0,05 değerinden büyük olduğu için dağılımın normal olduğunu söyleriz.

#Regresyon modelinin tahmini

model=lm(y1~x1) # Basit doğrusal regresyon modelini sağlar.
#Modelimizde 1 tane bağımsız değişken olduğu için basit doğrusal regresyondur.
summary(model) #Modelin özet sonuçlarını gösterir.
confint(model, level=0.95) #Model katsayılarının güven aralıklarını verir. Yüzde %95 güvenle test etmek istiyoruz.
plot(data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`, data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)` ) #saçılım grafiği


#Saçılım grafiği normale yakın olduğu x ve y arasında doğrusal bir ilişki olduğunu söyleyebiliriz.
cor(data_ex_2_8_Shelf_Stocking_$`Cases Stocked, x`, data_ex_2_8_Shelf_Stocking_$`Time, y (minutes)``) #korelasyon katsayısı

abline(model) #regresyon doğrusu

names(model)
model$fitted.values # regresyon doğrusu üzerinde yer alan nokta ve değerleri gösterir.
fitted(model)
prediction=predict(model, interval = "prediction") #kestirim aralıkları uyum değerlerinin alt ve üst sınırlarını verir.
prediction
confidence=predict(model, interval = "confidence") # uyum değerleri ve güven aralıklarını verir.
confidence
plot(data_ex_2_8_Shelf_Stocking_ ,model$fitted) #fit (uyum) değerlerin saçılım grafiği
coef(model) # regresyon katsayıları

coef(model) [1] #modelin 1. katsayısını verir (bo)
model$coef[1]+model$coef[2]*50 # x 25 iken y nin alacağı değer

#Daha sonra Regresyon modelinin tahminini yapıyoruz. Modelimiz Basit Doğrusal Regresyon olduğu için “model=lm(y1~x1)” yazıyoruz. Residual, Coefficients ve sig. değerlerimiz, F değeri ve belirtme kat sayısı değerleriyle model tahmini yapabiliriz.
#R square değeri 0,99 olduğu için zaman %99 oranında raf stoğuyla bağlantılı diyebiliriz. Modelin anlamlılığını test etmek istersek de F hesap değerine bakabiliriz. F değeri 2452 olduğu için F tablo değeri bu değerden daha küçük olduğu için model anlamlıdır.
#Değişkenler arasında doğrusal ya da eğrisel bir ilişki olduğunu anlamak için saçılım grafiğine bakarız. Bu grafikte doğrusal bir ilişki gözlenir.  

predict(model, list(x1=25)) # x 25 iken y nin al

predict(model, data.frame(x1=25)) # x 25 iken y nin alacağı değer
predict(model, data.frame(x1=c(25, 6, 8))) # birden fazla x değeri için y nin alacağı değerler
predict(model, data.frame(x1=c(25, 6, 8)), interval = "confidence", level=0.95) # x in değerleri için y nin değerleri ve  %95 lik güven aralıkları
predict(model, data.frame(x1=c(25, 6, 8)), interval = "prediction", level=0.95) # x in değerleri ve y nin uyum değerleri ve alt ve üst sınırları


confidence = predict(model, interval = "confidence") # güven aralıkları 

confidence




# Korelasyonlar
cor.test(x1,y1)  # korelasyon testi
cor.test(x1,y1,alternative = c("two.sided", "less","greater"), method = c("pearson", "kendall", "spearman"), exact = NULL, conf.level = 0.95)
cor.test(x1,y1,alternative = c("two.sided", "less","greater"), method = c("spearman"), exact = NULL, conf.level = 0.95)

#Kestirim aralıkları grafikleri
plot(y1~x1)
X1.sort=sort(unique(x1))
prediction=predict(model, newdata=data.frame(x1=X1.sort), interval = "prediction")
prediction
lines(X1.sort,prediction[,2],lty=2)
lines(X1.sort,prediction[,3],lty=2)
plot(y1~x1)
# Güven aralıkları grafikleri
X1.sort=sort(unique(x1))
confidence=predict(model, newdata=data.frame(x1=X1.sort), interval = "confidence")
confidence
lines(X1.sort,confidence[,2],lty=2)
lines(X1.sort,confidence[,3],lty=2)
# Standart sapmalar
standatdevx1=sd(x1,na.rm=FALSE) 
standatdevx1 # x e ait standart sapma
standatdevy1=sd(y1,na.rm=FALSE)
standatdevy1 # y e ait standart sapma
# x ile y arasında kovaryans
cov(x1,y1)
#varyans analizi tablosu
anova(model)

uyum =fitted(model)
uyum
res=resid(model)
res
newd <- data.frame(x1=c(50,75,100,125,150,175,200,225,250,275))
k = predict(lm(y1~x1), newd, se.fit = TRUE)
k
