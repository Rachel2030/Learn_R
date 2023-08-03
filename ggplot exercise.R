#####exercise
library(ggplot2)
data(iris)
head(iris)

ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot(aes(fill = Species, color = Species)) + geom_point() + geom_jitter()

ggplot(iris, aes(Sepal.Length, Sepal.Width, size = Sepal.Width)) + 
  geom_point()