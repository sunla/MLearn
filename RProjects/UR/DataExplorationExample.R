library(ggplot2)
data (diamonds)
str(diamonds)
summary(diamonds$color)
?diamonds
names(diamonds)
summary(diamonds$price,diamonds$cut)
by(diamonds$price,diamonds$cut,max)
  qplot(x=price, data = diamonds, binwidth = 100, color = I('black'), fill = I('#099DD9')) + 
    scale_x_continuous(limits = c(0,5000), breaks = seq(0,5000,100)) +
    facet_wrap(~cut)
nrow(subset(diamonds, price >= 15000))
qplot(x=cut, y=price, data=diamonds) +
  scale_y_continuous(limits = c(18500, 19000) )
