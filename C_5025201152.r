##1
#a
#sebelum
sebelum <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
#sesudah
sesudah <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

#selisih
sd(sebelum - sesudah)

#b
t.test(sebelum, sesudah, alternative = "greater", var.equal = FALSE)

#c
var.test(sebelum, sesudah)
t.test(sebelum, sesudah, alternative = "two.sided", var.equal = TRUE, mu = 0)

##2
library(BSDA)
#a setuju
#b
tsum.test(mean.x = 23500, sd(3900), n.x = 100)
#c 9.437e-15 < 0.05
# H0 dapat ditolak

##3
#a
# H0 : tidak ada perbedaan rata-rata pemegang saham dari bandung dengan pemegang saham dari bali
# H1 : ada perbedaan rata-rata pemegang saham dari bandung dengan pemegang saham dari bali 
#b
tsum.test(mean.x = 3.64, sd(1.67), n.x = 19, mean.y = 2.79, sd(1.32), n.y = 27, alternative = "greater", var.equal = TRUE)
#c
#d
#e
#pernyataan H0 dapat ditolak
#f
# Jadi ada perbedaan rata-rata pemegang saham dari bandung dengan pemegang saham dari bali
##4
#a
kucheng <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
attach (kucheng)
names (kucheng)

kucheng$Group <- as.factor(kucheng$Group)
kucheng$Group = factor (kucheng$Group, labels = c("Kucing oren", "Kucing hitam", "Kucing putih"))
class(kucheng$Group)

Group1 <- subset (kucheng, Group == "Kucing oren")
Group2 <- subset (kucheng, Group == "Kucing hitam")
Group3 <- subset (kucheng, Group == "Kucing putih")

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group3$Length)
qqline(Group3$Length)

#b
bartlett.test(Length ~ Group, data = kucheng)
#c
model1 = lm (Length ~ Group, data = kucheng)
anova (model1)

#d
#0.0013 < 0.05
#H0 dapat ditolak
#e
TukeyHSD(aov(model1))
#f
install.packages("ggplot2")
library("ggplot2")

ggplot(kucheng, aes(x = Group, y = Length)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Length (cm)")



##5
install.packages("ggpubr")
install.packages("dplyr")
install.packages("multcompView")
install.packages("readr")

library("ggpubr")
library(dplyr)
library(multcompView)
library(readr)
kaca <- read.csv("https://drive.google.com/u/0/uc?id=1aLUOdw_LVJq6VQrQEkuQhZ8FW43FemTJ&export=download")
#a
ggboxplot(kaca, x = "Temp", y = "Light", color = "Glass",
palette = c("#00AFBB", "#E7B800", "#E70000"))
#b
kaca$Glass <- as.factor(kaca$Glass)
kaca$Temp_Factor <- as.factor(kaca$Temp)
str(kaca)

anova2 <- aov(Light ~ Glass*Temp_Factor, data = kaca)
summary(anova2)
#c
data_summary <- group_by(kaca, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
#d
tukey <- TukeyHSD(anova2)
print(tukey)
#e
tukey.cld <- multcompLetters4(anova2, tukey)
print(tukey.cld)
