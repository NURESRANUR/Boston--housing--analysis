##Çalışma dizini kontrol etme
getwd()
##Csv dosyasının bulunduğu klasörü çalışma dizini olarak sentezleme
setwd("/Users/esraermis18icloud.com/Downloads")
## Veri setini CSV dosyasını içeri aktarma
data<-read.csv('HousingData.csv')
## Veri setinin gözlemlerini görüntüleme
head(data)

##Veri setinin yapısını inceleme
str(data)
##Veri setindeki toplam gözlem sayısını görme
nrow(data)
##Değişkenlere ait özet istatistilere incelemek
summary(data)
##Tablo halinde veri setlerini görüntülemek
View(data)

##Eksik gözlem içeren veri sayısının hesaplanması
colSums(is.na(data))
##Eksik gözlem içeren satırtları veri setinden çıkarmak
df_clean<-na.omit(data)
##temizlenmiş veri seti 
nrow(df_clean)
View(df_clean)

##Doğrusal regresyon modelioluşturma
##Bağımlı değişken:MEDV (ev fiyatı)
##Bağımsız değişken:CRIM(suç oranı)
model <- lm(MEDV ~ CRIM,data=df_clean)
##kurulan regresyon özet sonuçları
summary(model)
##regresyon katsayalrını görüntüleme
coef(model)


##Keşifsel analiz 
## medv için normal dağılım
hist(df_clean$MEDV,
     main='MEDV DAĞILIMI',
     xlab = 'medv',
     col='lightblue',
     border='black')

##crım için normal dağılım kontrolü 
hist(df_clean$CRIM,
    main='CRIME DAĞILIMI',
    xlab = 'CRIM',
    col='lightgreen',
    border='black')
##boxplotlar
boxplot(df_clean$CRIM,
        main='crım boxlot',
        col='lightblue')

boxplot(df_clean$MEDV,
        main='MDV boxlot',
        col='green')

qqline(df_clean$CRIM,main='CRIM Q-Q Plot')
qqnorm(df_clean$CRIM, col='red')

qqline(df_clean$MEDV,main='MEDV Q-Q Plot')
qqnorm(df_clean$MEDV, col='red')


##suç oranı ve ev fiyatları arasındaki ilişkiyi saçılım grafiği ile gözlemek
library(ggplot2)
ggplot(df_clean,aes(x=CRIM, y=MEDV))+
       geom_point()+
       geom_smooth(method='lm',color='RED')
##KOLERASYON ANALİZİ
numeric_data<-df_clean[sapply(df_clean,is.numeric)]
cor_matrix<-cor(numeric_data)
print(cor_matrix)

##regresyon analizi

model<-lm(MEDV~CRIM, data=df_clean)
summary(model)
