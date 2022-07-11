## GAN GEAD KEE
## TP057261

# install.packages("viridis")
# install.packages("hrbrthemes")
# install.packages("gridExtra")
# install.packages("plotly")
# install.packages("GGally")

library(viridis)  # colour scales
library(hrbrthemes)
library(gridExtra)
library(crayon)
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)

###############################################################################

options(max.print = 10000)

# Question 1: How the student achieves better score in Level 3. 
# Analysis 1-1: Find the relationship between extra education support with L1 and L2 score.
# Analysis 1-2: Find the relationship between study time and ..
# Analysis 1-3: Find the relationship between .

studentOriData = read.csv("C:\\Users\\Gead Kee\\Desktop\\RStudio\\Gan_Gead_Kee-TP057261\\student.csv")

viewD = function(f1){
  View(f1)
  glimpse(f1)
  summary(f1)
}

## SET COLUMN NAMES & ENSURE THE DATA TYPES
studentData = data.frame(
  ids = factor(as.numeric(studentOriData$index)),
  school = factor(studentOriData$school),
  gender = factor(studentOriData$sex),
  age = as.numeric(studentOriData$age),
  area = factor(studentOriData$address),
  familySize = factor(studentOriData$famsize),
  parentsStatus = factor(studentOriData$Pstatus),
  momEdu = factor(as.numeric(studentOriData$Medu)),
  dadEdu = factor(as.numeric(studentOriData$Fedu)),
  momJob = factor(studentOriData$Mjob),
  dadJob = factor(studentOriData$Fjob),
  courseReason = factor(studentOriData$reason),
  guardian = factor(studentOriData$guardian),
  travelTime = factor(as.numeric(studentOriData$traveltime)),
  studyTime = factor(as.numeric(studentOriData$studytime)),
  failedClass = factor(as.numeric(studentOriData$failures)),
  schoolSupport = factor(studentOriData$schoolsup),
  familySupport = factor(studentOriData$famsup),
  paidExtra = factor(studentOriData$paid),
  extraActivity = factor(studentOriData$activities),
  nursery = factor(studentOriData$nursery),
  tertiary = factor(studentOriData$higher),
  homeInternet = factor(studentOriData$internet),
  romRelation = factor(studentOriData$romantic),
  famRelation = factor(as.numeric(studentOriData$famrel)),
  freeTime = factor(as.numeric(studentOriData$freetime)),
  outFriends = factor(as.numeric(studentOriData$goout)),
  weekdayAlc = factor(as.numeric(studentOriData$Dalc)),
  weekendAlc = factor(as.numeric(studentOriData$Walc)),
  health = factor(as.numeric(studentOriData$health)),
  schAbsences = as.numeric(studentOriData$absences),
  g1 = as.numeric(studentOriData$G1),
  g2 = as.numeric(studentOriData$G2),
  g3 = as.numeric(studentOriData$G3),
  meanGrade = (studentOriData$G1 + studentOriData$G2 + studentOriData$G3)/3
)
viewD(studentData)

#### QUESTION 1
### Analysis 1-1

## SUMMARISE - MEAN
summarise(studentData, Grade1AVG = mean(g1), Grade2AVG = mean(g2), Grade3AVG = mean(g3))

## Find number of students with the conditions..
g1LTmean = nrow(studentData[studentData$g1 < mean(studentData$g1), ])
g1GTmean = nrow(studentData[studentData$g1 >= mean(studentData$g1), ])

g2LTmean = nrow(studentData[studentData$g2 < mean(studentData$g2), ])
g2GTmean = nrow(studentData[studentData$g2 >= mean(studentData$g2), ])

g3LTmean = nrow(studentData[studentData$g3 < mean(studentData$g3), ])
g3GTmean = nrow(studentData[studentData$g3 >= mean(studentData$g3), ])

g1LTmean
g1GTmean
g2LTmean
g2GTmean
g3LTmean
g3GTmean


## Compare the improvement of the students from Grade1 to Grade3

grades = c(rep("Grade1", 2), rep("Grade2", 2), rep("Grade3", 2))
comparison = c("below Grade1 Average", "above Grade1 Average", "below Grade2 Average", "above Grade2 Average", "below Grade3 Average", "above Grade3 Average")
noOfStudents = c(g1LTmean, g1GTmean, g2LTmean, g2GTmean, g3LTmean, g3GTmean)
comparisonGrades = data.frame(grades, comparison, noOfStudents)

a12 = ggplot(data = comparisonGrades, 
             aes(x = grades, y = noOfStudents, group = comparison)) +
  geom_col(aes(fill = comparison), position = "dodge") +
  scale_fill_viridis(discrete = T) + 
  ggtitle("The Regression of Students from G1 to G3") + 
  geom_text(aes(label = noOfStudents, y = noOfStudents + 2), 
            position = position_dodge(0.9), 
            vjust = 0)
# https://ggplot2.tidyverse.org/reference/geom_text.html - groupedBar-label
ggplotly(a12)


### Analysis 1-3
## plot line graph for a Sample of 10 student's grades from g1-g3
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x1 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x2 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x3 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x4 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x5 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x6 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x7 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x8 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x9 = c(x$g1, x$g2, x$g3)
x = sample_n(studentData, 1) %>% select(g1, g2, g3)
x10 = c(x$g1, x$g2, x$g3)

plot(x1, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="red", cex=0.75, ylim=c(0,20))
lines(x2, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="blue", cex=0.75)
lines(x3, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="green", cex=0.75)
lines(x4, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="pink", cex=0.75)
lines(x5, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="navy", cex=0.75)
lines(x6, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="orange", cex=0.75)
lines(x7, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="#013220", cex=0.75)
lines(x8, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="#A9A9A9", cex=0.75)
lines(x9, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="#FF00FF", cex=0.75)
lines(x10, type = "o", xlab = "Tests", ylab = "Grade Scores", main="Sample of Students' Scores from G1 to G3", col="#aa6c39", cex=0.75)


v = ggparcoord(studentData,
               columns = 32:34, groupColumn = 1,
               showPoints = TRUE, 
               title = "The Fluctuation of Students' Math Grade throughout G1-G3",
               alphaLines = 0.3,
               scale = "globalminmax") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=20)
  )
ggplotly(v)






