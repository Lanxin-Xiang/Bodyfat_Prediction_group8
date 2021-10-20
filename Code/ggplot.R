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
    theme(legend.position="bottom")
  
  p3 <- ggplot(data = NULL, aes(sample = md$residuals)) +
    stat_qq() +
    stat_qq_line(col = "Red",lwd = 0.8)+
    labs(title = cbind("QQ-plot of ",title))
  
  return(list(rvfPlot=p1, rvlevPlot=p2,qqplot=p3))
}

p10 <- diagplot(simp_m0,"\"Daily\" model young adults group")
p11 <- diagplot(simp_m1,"\"Daily\" model middle age group")
p12 <- diagplot(simp_m2,"\"Daily\" model old group")

p20 <- diagplot(full_m0,"\"Advanced\" model young adults group")
p21 <- diagplot(full_m1,"\"Advanced\" model middle age group")
p22 <- diagplot(full_m2,"\"Advanced\" model old group")