########################################################################
####################### Modeling and Analysis ##########################
########################################################################


############################## age factor ##############################

simp_m <- lm(BODYFAT ~ ADIPOSITY+ABDOMEN+Myage,data = ddf)
summary(simp_m)
a <- sum(simp_m$residuals^2)


step(lm(BODYFAT~.,data = ddf[,c(-1,-3,-4,-7)]))
full_m <-lm(BODYFAT ~ HEIGHT + NECK + CHEST + ABDOMEN + FOREARM + WRIST + Myage,data = ddf) 
summary(full_m)
b <- sqrt(sum(full_m$residuals^2)/nrow(ddf))


############################## Myage=0 #################################


sub0 <- subset(ddf,ddf$Myage==0)
simp_m0 <- lm(BODYFAT~WEIGHT+HEIGHT+ABDOMEN,data = sub0)
summary(simp_m0)
a1 <- sqrt(sum(simp_m0$residuals^2)/nrwo(sub0))

step(lm(BODYFAT~.,data = sub0[,c(-1,-3,-4,-7,-18)]))
full_m0 <-lm(BODYFAT ~ HEIGHT + NECK + ABDOMEN + BICEPS + WRIST,data = sub0) 
summary(full_m0)
b1 <- sqrt(sum(full_m0$residuals^2)/nrwo(sub0))


############################# Myage=1 ##################################

sub1 <- subset(ddf,ddf$Myage==1)
simp_m1 <- lm(BODYFAT~WEIGHT+HEIGHT+ABDOMEN,data = sub1)
summary(simp_m1)
a2 <- sqrt(sum(simp_m1$residuals^2)/nrow(sub1))


step(lm(BODYFAT~.,data = sub1[,c(-1,-3,-4,-7,-18)]))
full_m1 <-lm(BODYFAT ~ ABDOMEN + THIGH + ANKLE + FOREARM + WRIST, data = sub1)
summary(full_m1)
b2 <- sqrt(sum(full_m1$residuals^2)/nrow(sub1))


############################## Myage=2 #################################

sub2 <- subset(ddf,ddf$Myage==2)
simp_m2 <- lm(BODYFAT~WEIGHT+HEIGHT+ABDOMEN,data = sub2)
summary(simp_m2)
a3 <- sqrt(sum(simp_m2$residuals^2)/nrow(sub2))

step(lm(BODYFAT~.,data = sub2[,c(-1,-3,-4,-7,-18)]))
full_m2 <-lm(BODYFAT ~ WEIGHT + NECK + ABDOMEN + ANKLE+BICEPS+FOREARM, data = sub2)
summary(full_m2)
b3 <- sqrt(sum(full_m2$residuals^2)/nrow(sub2))
plot(full_m2)

