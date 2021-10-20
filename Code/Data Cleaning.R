########################################################################
############################# Data Cleaning ############################
########################################################################


rm(list = ls())

Bodyfat <- read.csv("BodyFat.csv")
# View(Bodyfat)

summary(Bodyfat)
Bodyfat[which(Bodyfat$BODYFAT==0),] # #182 del
Bodyfat[172,] # 172 bodyfat=1.9 del
Bodyfat[which(Bodyfat$BODYFAT==45.10),] # #216 del
Bodyfat[which(Bodyfat$WEIGHT==363.15),] # #39 del
Bodyfat[which(Bodyfat$AGE==81),] # #79
Bodyfat[which(Bodyfat$HEIGHT==29.5),] # #42 rep


w_kg <- Bodyfat$WEIGHT*0.4536
h_m <- Bodyfat$HEIGHT*0.0254
bmi_check <- w_kg/h_m/h_m
bmi_check[42]
h_42 <- (sqrt(w_kg[42]/Bodyfat$ADIPOSITY[42]))/0.0254 # 1.76351m 69.43 inches

df <- Bodyfat
df$HEIGHT[42] <- h_42

plot(df$DENSITY,df$BODYFAT,type = "n",ylim = c(-5,50))
text(df$DENSITY,df$BODYFAT,df$IDNO)
Bodyfat[96,] # #96 del
Bodyfat[76,] # #76 sim 28 del
Bodyfat[48,] # #48 sim 146 149 del


plot(df$ADIPOSITY,bmi_check,type = "n",ylim = c(10,50))
text(df$ADIPOSITY,bmi_check,df$IDNO)
Bodyfat[163,] # #163
Bodyfat[221,] # #221

Myage <- df$AGE
Myage[Myage<45] = 0
Myage[Myage>=60] = 2
Myage[Myage>2] = 1
table(Myage)

Myage <- factor(Myage)

df <- cbind(df,Myage)

ddf <- df[c(-182, -216, -96, -76, -48, -39,-172, -86),]
boxplot(ddf$BODYFAT~ddf$Myage)

library(car)
leveneTest(ddf$BODYFAT~ddf$Myage)

write.csv(ddf,"Bodyfat_cleaned.csv")
