#Data Manipulation
head(cars)

far_cars=cars[cars$dist>50,]

fast_cars=cars[cars$speed > quantile(cars$speed, probs = (.7)),]
fast_cars[fast_cars$speed>20,'speed']=100

#Stats Analysis
hist(cars$speed)
t.test(cars$speed,cars$dist)
lm(dist ~ speed, data = cars)

#visualization
install.packages("ggplot2")
library(ggplot2)

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

p <- ggplot(mtcars, aes(x=mpg)) + 
  geom_density()
p
p+ geom_vline(aes(xintercept=mean(mpg)),
              color="blue", linetype="dashed", size=1)

p <- ggplot(mtcars,aes(x=cyl,y=mpg, group=cyl)) + geom_boxplot()
p
p + coord_flip()

heatmap(as.matrix(mtcars), scale="column")
