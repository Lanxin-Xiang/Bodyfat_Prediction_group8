res10 <- simp_m0$residuals

ggplot(data = NULL, aes(sample = res10)) +
  stat_qq() +
  stat_qq_line(col = "Red",lwd = 0.8)+
  labs(title = "QQ-plot of \"Daily\" model young adults group")

diagplot <- function(md,title){
  p1 <- ggplot(data = NULL, aes(md$fitted.values,md$residuals)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 0,col = "red",lty = 2)+
    theme(plot.title = element_text(size = 11))+
    labs(title = paste0("Residuals vs. Fitted of ",title),
         x = "Fitted values",y = "Residuals")
  
  p2 <- ggplot(md, aes(.hat, .stdresid))+
    geom_point(aes(size=.cooksd), na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)+
    xlab("Leverage")+ylab("Standardized Residuals")+
    theme(plot.title = element_text(size = 11))+
    labs(title = paste0("Residual vs Leverage of ",title))+
    scale_size_continuous("Cook's Distance", range=c(1,5))+
    #theme_bw()+
    theme(legend.position="bottom")
  return(list(rvfPlot=p1, rvlevPlot=p2))
}

p10 <- diagplot(simp_m0,"\"Daily\" model young adults group")



datanew=read.csv("Bodyfat_cleaned.csv")

#plot

library(ggplot2)
library(ggpubr)
library(ggpmisc)
head(datanew)

b <- ggplot(datanew, aes(x = WEIGHT, y = BODYFAT))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm", color = "black", fill = "lightgray") 

# Change color and shape by groups (cyl)
b + geom_point(aes(color = AGE, shape = AGE))+
  geom_smooth(aes(color = AGE, fill = AGE), method = "lm") +
  geom_rug(aes(color =AGE)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
# Remove confidence region (se = FALSE)
# Extend the regression lines: fullrange = TRUE
b + geom_point(aes(color = AGE, shape = cyl)) +
  geom_rug(aes(color =cyl)) +
  geom_smooth(aes(color = cyl), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggpubr::stat_cor(aes(color = cyl), label.x = 3)