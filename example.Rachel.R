###Graphing with R
head(airquality, n=20)
plot(airquality$Day, airquality$Temp)

###Exercise
9*9
9^2
27%%2
FALSE/TRUE

###Visualisation
plot(airquality$Day, airquality$Temp, xlab = "Day", ylab = "Temp", pch = 19, las = 1, col = "deeppink")

###Tidyverse
install.packages("tidyverse")
library(tidyverse)


ggplot(data = airquality, aes(x = Day, y = Temp)) + 
  geom_boxplot()

ggplot(data = airquality, aes(x = Day, y = Temp, color = "ozone")) + 
  geom_point() + geom_smooth()

ggplot(data = airquality, aes(x = Day, y = Temp, color = "ozone")) + 
  geom_point() + geom_line()

ggplot(data = airquality, aes(x = Day, y = Temp, color = "ozone")) + geom_point(aes(shape = "ozone"), size = 2) + scale_shape_manual(values = (24)) + 
  scale_color_manual(values = ("purple")) 

ggplot(data = airquality, aes(x = Day, y = Temp, color = "ozone")) + 
geom_smooth(aes(linetype = "ozone"), linewidth = 2, se = FALSE) + scale_color_manual(values = c("blue")) + scale_linetype_manual(values = c("solid"))

ggplot(data = airquality, aes(x = Day, y = Temp, color = "ozone")) + 
geom_smooth(aes(linetype = "ozone"), linewidth = 2, se = FALSE) + geom_point(aes(shape = "ozone"), size = 2) + scale_color_manual(values = c("blue")) + scale_linetype_manual(values = c("solid"))


airquality2  <- airquality
airquality2$Rain <- rep(c("Yes","No","Yes"), 51)
head(airquality2)

install.packages("cowplot")
library(cowplot)


head(diamonds)
hist(diamonds$price)

ggsave() ###saving your graphics

####Exercise
ggplot(iris, aes(x = "species", y = Sepal.Length)) + geom_boxplot()
plot(iris$Sepal.Length, iris$Sepal.Width)

ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = clarity)) + geom_jitter(alpha = 0.05)

head(mpg)
?mpg
plot(mpg$displ, mpg$hwy)



library(tidyverse)
