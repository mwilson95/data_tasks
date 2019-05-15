#Purpose: Complete Riskified Data Task
#Date: 5/15/19
#Author: Megan Wilson

#Import libraries
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(openxlsx)

#Import Data
data<-read.csv("personal_projects/Operations & Research Analyst - Test.csv")

#1. Draw a boxplot showing the area size distribution for each shape.
area_boxplot <- ggplot(data, aes(x=shape, y=area)) +
  geom_boxplot()+
  labs(x = 'Shape', y='Area (square inches)',
          title = "Distribution of Area by Shape")

#2. Calculate the mean, max, and standard deviation of the area size of each color.
summary<-data %>%
  group_by(color)%>%
  summarize(
    mean=mean(area),
    max=max(area),
    st_dev=sd(area)
  )

#3. What is the average area size of a yellow square?
yellow_sq<-data %>%
  filter(color=="yellow",shape=="square")%>%
  summarize(average=mean(area))

#4. Which shape is most likely to be green?

#Of green shapes which shape is the highest percent?
prob1<-data %>%
  filter(color=="green")%>%
  group_by(shape)%>%
  summarize(
    count=n()
  ) %>%
  mutate(prob=count/nrow(data %>% filter(color=="green")))


#Within shapes, for which does green have the highest percent?
prob2<-data %>%
  group_by(shape)%>%
  summarize(
    count=n(),
    count_green=sum(color=="green"),
    prob=sum(color=="green")/n()
  )


#5. Given the fact the the object is red, with an area size larger than 3,000 - what are the chances the object is a square? a triangle? a circle?
large_red <-data %>%
  filter(color=="red",area>3000) %>%
  group_by(shape)%>%
  summarize(
    count=n()
  )%>%
  mutate(prob=count/nrow(data %>% filter(color=="red",area>3000)))


#6. Write a function that calculates the side or radios of an object, depending on the shape and area of the object
#[for an Equilateral triangle - area = (side ^ 2) * sqrt(3) / 4].

calculate_side<-function(obj_shape,obj_area) {
  if(obj_shape=="circle"){
    output<-sqrt(obj_area/pi)

  }else if(obj_shape=="square"){
    output<-sqrt(obj_area)

  }else if(obj_shape=="triangle"){
    output<-sqrt((4*obj_area)/sqrt(3))
  }else{
    print("shape not calculated!")
  }
  return(output)
}

#7. Add a column to the dataset called "side" that shows the size matching the area in each row,
#   round that number to the closest integer (shape side or radios).
data<-data %>%
  mutate(side=mapply(function(x,y) calculate_side(x,y),shape,area) %>%round(0))

#8. Draw a boxplot showing the side size distribution for each shape - what can you infer from this plot?
side_boxplot <- ggplot(data, aes(x=shape, y=side)) +
  geom_boxplot() +
  labs(x = 'Shape', y='Side/Radius Size (inches)',
     title = "Distribution of Side Size by Shape")

#9. Make a scatter plot with "side" on the x axis, "area" on the y axis with a different color for each shape.
scatter_graph<-ggplot(data, aes(x=side, y=area,color=shape)) +
  geom_point()+
  labs(x = 'Side/Radius Size (inches)', y='Area (square inches)',
       title = "Shape Area vs. Side Length")


#10. Create a dataframe, table or list that show for each shape:
#a. The proportion of red objects within the shape
red_obj <-data %>%
  group_by(shape)%>%
  summarize(
    prop_red=sum(color=="red")/n()
  )

#b. The proportion of blue area out of the shape's total area (sum of square inch blue area of the shape over sum of all shape size).
blue_area<-data %>%
  group_by(shape)%>%
  summarize(
    prop_blue=sum(if_else(color=="blue",area,0))/sum(area)
  )


#11. Create a function that calculates 10. b. for a given shape and color.
calculate_area_prop<-function(df,obj_shape,obj_color){
  color_area<-df %>%
    filter(shape==obj_shape) %>%
    summarize(
      prop=sum(if_else(color==obj_color,area,0))/sum(area)
    )
  return(color_area$prop)
}
#Example function calls
calculate_area_prop(data,"triangle","blue")
calculate_area_prop(data,"triangle","red")


#Export Items for Written Summary.
jpeg("personal_projects/area_boxplot.jpg")
area_boxplot
dev.off()

jpeg("personal_projects/side_boxplot.jpg")
side_boxplot
dev.off()

jpeg("personal_projects/scatterplot.jpg")
scatter_graph
dev.off()

export<-list(summary,red_obj,blue_area)
write.xlsx(export,"personal_projects/summary_tables.xlsx")
