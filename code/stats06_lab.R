pacman::p_load(tidyverse,
               patchwork,
               here)


# exercise 1 --------------------------------------------------------------

#use iris data set to create 3 regressions (one for each species) comparing
#x = petal width y= sepal width

df_setosa<-iris%>%
  filter(Species == "setosa")

df_versicolor<-iris%>%
  filter(Species == "versicolor")

df_virginica<-iris%>%
  filter(Species == "virginica")

lm(Sepal.Width ~ Petal.Width,
      data=df_setosa)

lm(Sepal.Width ~ Petal.Width,
   data=df_versicolor)

lm(Sepal.Width ~ Petal.Width,
   data=df_virginica)


# exercise 2 --------------------------------------------------------------

#select one species and compare petal.width only and petal width+length as x
#compare regression estimate of petalwidth and r^2 values 

m_set2<-lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data=df_setosa)

m_set<-lm(Sepal.Width ~ Petal.Width,
   data=df_setosa)

summary(m_set) #r2=.05
summary(m_set2) #r2=.06

# exercise 3 --------------------------------------------------------------

#include x in one model from exercise 1, does x impact r^2

x<-rnorm(nrow(iris),mean=0,sd=1)
iris<-iris%>%
  mutate(x=x)


