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


### Analysis 1-4
## find the relationship between tertiary edu and studyTime with "meanGrade"
a14 = studentData  %>% 
  ggplot(aes(x = studyTime, y = meanGrade)) +
  geom_count(alpha = 0.5) + 
  ylim(0,20) + 
  ggtitle("The Relationship of TertiaryEdu and StudyTime with Students' MeanGrade") +
  facet_grid(~tertiary)

ggplotly(a14)


### Analysis 1-5
## find relationship between meanGrade and schAbsences
a15 = ggplot(studentData, aes(meanGrade, schAbsences)) + 
  geom_count(alpha = 0.5, aes(size = ..n..)) +
  ggtitle("The Relationship between School Absences and Students' MeanGrade")

ggplotly(a15)


### Analysis 1-6
## find relationship between schAbsences and health
a16 = ggplot(studentData, aes(schAbsences, health)) +
  geom_count(alpha = 0.5, aes(color = ..n.., size = ..n..)) + 
  ggtitle("The Relationship between School Absences and Students' Health")

ggplotly(a16) 


### Analysis 1-7
## find relationship between schAbsences and free time
a17 = ggplot(studentData, aes(freeTime, schAbsences)) +
  geom_count(alpha = 0.5, aes(color = ..n..)) + 
  ggtitle("Relationship between School Absences and Free Time")+
  theme(legend.position = "right")

ggplotly(a17)



###########################################################################
#### QUESTION 2

### Analysis 2-1
## find the quartiles of "meanGrade"
quantile(studentData$meanGrade, probs = c(0.25, 0.75))


## find students who are above the Upper-Quartile & in Lower-Quartile
meanGradeMale75 = studentData %>% 
  filter(meanGrade >= 13 & gender == "M") %>%
  arrange(age)
meanGradeFemale75 = studentData %>% 
  filter(meanGrade >= 13 & gender == "F") %>%
  group_by(age) 

meanGradeMale25 = studentData %>% 
  filter(meanGrade < 8 & gender == "M") %>%
  arrange(age)
meanGradeFemale25 = studentData %>% 
  filter(meanGrade < 8 & gender == "F") %>%
  group_by(age) 

studentD75 = data.frame(gender = c("M", "F"), meanGradeGender75 = c(nrow(meanGradeMale75), nrow(meanGradeFemale75)))
studentD25 = data.frame(gender = c("M", "F"), meanGradeGender25 = c(nrow(meanGradeMale25), nrow(meanGradeFemale25)))


## plot bar chart based on the data found
gg75 = ggplot(studentD75, aes(x = factor(gender), y = meanGradeGender75)) + 
  geom_col(aes(fill = gender), position = "dodge") + 
  geom_text(aes(label = meanGradeGender75, y = meanGradeGender75 + 0.05),
            position = position_dodge(0.9), 
            vjust = 0) + 
  scale_x_discrete("Gender Distribution of Students Above the Upper-Quartile of Mean Grades") + 
  scale_y_continuous("Mean Grade") +
  theme(legend.position="bottom")


gg25 = ggplot(studentD25, aes(x = factor(gender), y = meanGradeGender25)) + 
  geom_col(aes(fill = gender), position = "dodge") + 
  geom_text(aes(label = meanGradeGender25, y = meanGradeGender25 + 0.05),
            position = position_dodge(0.9), 
            vjust = 0) + 
  scale_x_discrete("Gender Distribution of Students in the Lower-Quartile of Mean Grades") + 
  scale_y_continuous("Mean Grade") +
  theme(legend.position="bottom")

grid.arrange(gg25, gg75, ncol = 2)


### Analysis 2-2
## merge the data in Analysis 2-1 into only Upper & Lower Quartile students
meanGrade25 = rbind.data.frame(meanGradeMale25, meanGradeFemale25)
meanGrade75 = rbind.data.frame(meanGradeMale75, meanGradeFemale75)

w = ggparcoord(meanGrade25,
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
ggplotly(w)

fg = ggparcoord(meanGrade75,
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
ggplotly(fg)



## Analysis 2-2-1
## differences in schAbsences and freeTime in Upper & Lower Quartile meanGrades

a221.25 = meanGrade25 %>% 
  count(schAbsences, meanGrade, freeTime) %>%
  ggplot(aes(x = meanGrade, y = schAbsences)) +
  geom_tile(mapping = aes(fill = freeTime)) + 
  ggtitle("FreeTime & Absences of Students in Lower-Quartile of MeanGrades") +
  scale_fill_viridis(discrete = T)

a221.75 = meanGrade75 %>% 
  count(schAbsences, meanGrade, freeTime) %>%
  ggplot(aes(x = meanGrade, y = schAbsences)) +
  geom_tile(mapping = aes(fill = freeTime)) + 
  ggtitle("FreeTime & Absences of Students in Upper-Quartile of MeanGrades") +
  scale_fill_viridis(discrete = T)

ggplotly(a221.25)
ggplotly(a221.75)


## Analysis 2-2-2
## differences in schAbsences and freeTime and studyTime in Upper & Lower Quartile meanGrades
a222.25 = meanGrade25 %>% 
  count(freeTime, meanGrade, schAbsences, studyTime) %>%
  ggplot(aes(x = meanGrade, y = schAbsences)) +
  geom_tile(mapping = aes(fill = freeTime)) + 
  ggtitle("StudyTime & FreeTime & Absences of Students in Lower-Quartile of MeanGrades") +
  scale_fill_viridis(discrete = T) + 
  facet_grid(~studyTime)

a222.75 = meanGrade75 %>% 
  count(freeTime, meanGrade, schAbsences, studyTime) %>%
  ggplot(aes(x = meanGrade, y = schAbsences)) +
  geom_tile(mapping = aes(fill = freeTime)) + 
  ggtitle("StudyTime & FreeTime & Absences of Students above Upper-Quartile of MeanGrades") +
  scale_fill_viridis(discrete = T) + 
  facet_grid(~studyTime)

ggplotly(a222.25)
ggplotly(a222.75)


### Analysis 2-3
## differences in studyTime and tertiary of students in Upper & Lower Quartile meanGrades
a23.25 = meanGrade25 %>% 
  ggplot(aes(x = studyTime, y = meanGrade)) +
  geom_jitter(alpha = 0.5) + 
  ggtitle("StudyTime & Tertiary of Students in Lower-Quartile of MeanGrades") + 
  ylim(0,20) + 
  facet_grid(~tertiary)

a23.75 = meanGrade75 %>% 
  ggplot(aes(x = studyTime, y = meanGrade)) +
  geom_jitter(alpha = 0.5) + 
  ggtitle("StudyTime & Tertiary of Students above Higher-Quartile of MeanGrades") + 
  ylim(0,20) + 
  facet_grid(~tertiary)

grid.arrange(a23.25, a23.75, ncol = 2)

ggplotly(a23.25)
ggplotly(a23.75)


### Analysis 2-4
## differences in studyTime and homeInternet of students in Upper & Lower Quartile meanGrades

a24.25 = meanGrade25 %>% 
  ggplot(aes(x = studyTime, y = meanGrade, fill = studyTime)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3) +
  coord_flip() +
  xlab("studyTime") + 
  ylab("meanGrade") +
  ggtitle("StudyTime & homeInternet of Students in Lower-Quartile") + 
  facet_grid(~homeInternet)

a24.75 = meanGrade75 %>% 
  ggplot(aes(x = studyTime, y = meanGrade, fill = studyTime)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3) +
  coord_flip() +
  xlab("studyTime") + 
  ylab("meanGrade") +
  ggtitle("StudyTime & homeInternet of Students above Upper-Quartile") +
  facet_grid(~homeInternet)

grid.arrange(a24.25, a24.75, ncol = 2)


### Analysis 2-5
## 
n = meanGrade25 %>% 
  ggplot(aes(x = studyTime, y = meanGrade, fill = studyTime)) +
  geom_violin(stat = "ydensity",
              position = "dodge",
              scale = "area") +
  coord_flip() +
  xlab("studyTime") + 
  ylab("meanGrade") +
  facet_grid(~homeInternet)


## Analysis 2-5-1
## relationship with family size  
a251.25 = meanGrade25 %>% 
  ggplot(aes(x = familySize, y = meanGrade)) +
  geom_boxplot() + 
  ggtitle("Affects of FamilySize on Students in Lower-Quartile of MeanGrades") + 
  xlab("Family Size") + 
  ylab("meanGrade")

ggplotly(a251.25)

a251.75 = meanGrade75 %>% 
  ggplot(aes(x = familySize, y = meanGrade)) +
  geom_boxplot() + 
  ggtitle("Affects of FamilySize on Students above Upper-Quartile of MeanGrades") + 
  xlab("Family Size") + 
  ylab("meanGrade")

ggplotly(a251.75)


## Analysis 2-5-2
## relationship with parents current status

a252.25 = meanGrade25 %>% 
  ggplot(aes(x = parentsStatus , y = meanGrade, fill = parentsStatus)) +
  geom_count() + 
  ggtitle("Affects of Parents Status on Students in Lower-Quartile of MeanGrades") + 
  xlab("Parents' Status") + 
  ylab("meanGrade")

ggplotly(a252.25)

a252.75 = meanGrade75 %>% 
  ggplot(aes(x = parentsStatus , y = meanGrade, fill = parentsStatus)) +
  geom_count() + 
  ggtitle("Affects of Parents Status on Students above Upper-Quartile of MeanGrades") + 
  xlab("Parents' Status") + 
  ylab("meanGrade")

ggplotly(a252.75)


## Analysis 2-5-3
## relationship with guardian

a253.25 = meanGrade25 %>% 
  ggplot(aes(x = guardian, y = meanGrade, fill = guardian)) +
  geom_count() + 
  ggtitle("Affects of Guardians on Students in Lower-Quartile of MeanGrades") + 
  xlab("Guardians of Students") + 
  ylab("meanGrade") +
  scale_fill_viridis(discrete=TRUE, direction = -1)

ggplotly(a253.25)

a253.75 = meanGrade75 %>% 
  ggplot(aes(x = guardian, y = meanGrade, fill = guardian)) +
  geom_count() + 
  ggtitle("Affects of Guardians on Students above Upper-Quartile of MeanGrades") + 
  xlab("Guardians of Students") + 
  ylab("meanGrade") +
  scale_fill_viridis(discrete=TRUE, direction = -1)

ggplotly(a253.75)

## Analysis 2-5-3-1
## merge with parentStatus
a2531.25 = meanGrade25 %>% 
  ggplot(aes(x = guardian, y = meanGrade, fill = guardian)) +
  geom_count() + 
  ggtitle("Affects of Guardians and parentStatus on Students in Lower-Quartile") + 
  xlab("Guardians of Students") + 
  ylab("meanGrade") +
  scale_fill_viridis(discrete=TRUE, direction = -1) +
  facet_grid(~parentsStatus)

ggplotly(a2531.25)

a2531.75 = meanGrade75 %>% 
  ggplot(aes(x = guardian, y = meanGrade, fill = guardian)) +
  geom_count() + 
  ggtitle("Affects of Guardians and parentStatus on Students above Upper-Quartile of MeanGrades") + 
  xlab("Guardians") + 
  ylab("meanGrade") +
  scale_fill_viridis(discrete=TRUE, direction = -1) +
  facet_grid(~parentsStatus)

ggplotly(a2531.75)


## Analysis 2-5-4
## relationship with family relation

a254.25 = meanGrade25 %>% 
  ggplot(aes(x = famRelation, y = meanGrade, fill = famRelation)) +
  geom_count() + 
  ggtitle("Affects of familyRelations on Students in Lower-Quartile of MeanGrades") + 
  xlab("Relationship with Family") + 
  ylab("meanGrade") +
  scale_fill_discrete(direction = -1)

ggplotly(a254.25)

a254.75 = meanGrade75 %>% 
  ggplot(aes(x = famRelation, y = meanGrade, fill = famRelation)) +
  geom_count() + 
  ggtitle("Affects of familyRelations on Students above Upper-Quartile of MeanGrades") + 
  xlab("Relationship with Family") + 
  ylab("meanGrade") +
  scale_fill_discrete(direction = -1)

ggplotly(a254.75)


## Analysis 2-5-4-1
## merge with family size

a2541.25 = meanGrade25 %>% 
  ggplot(aes(x = famRelation, y = meanGrade, fill = famRelation)) +
  geom_count() + 
  ggtitle("Affects of familyRelations on Students in Lower-Quartile of MeanGrades") + 
  xlab("Relationship with Family") + 
  ylab("meanGrade") +
  scale_fill_discrete(direction = -1) + 
  facet_grid(~familySize)

ggplotly(a2541.25)

a2541.75 = meanGrade75 %>% 
  ggplot(aes(x = famRelation, y = meanGrade, fill = famRelation)) +
  geom_count() + 
  ggtitle("Affects of familyRelations on Students above Upper-Quartile of MeanGrades") + 
  xlab("Relationship with Family") + 
  ylab("meanGrade") +
  scale_fill_discrete(direction = -1) + 
  facet_grid(~familySize)

ggplotly(a2541.75)




