# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

library(dplyr)


age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")



checkHeight3 = function(students.input = students){

  result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
  colnames(result.frame) = c("name", "difference")

  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))

  for (i in 1:nrow(students.input)) {
    # calculate sex-specific deviations from the mean
    if (students.input[i, "sex"] == "F") {
      height.diff = 100*(students.input[i,]$height - female.mean$mean)
    }
    else {
      height.diff = 100*(students.input[i, ]$height - male.mean$mean)
    }
    result.frame[i, "name"] = as.character(students.input[i, "name"])
    result.frame[i, "difference"] = height.diff
  }
  return(result.frame)
}

checkHeight3(students.input = students)





#version 
check_height_2 <- function(row){
  #get correct gender name
  if(row[['sex']] == 'M'){
    gender = 'man'
  }else{
    gender = 'woman'
  }
  # Get gender mean height
  m_height = df %>%
    filter(sex == row[['sex']]) %>%
    summarise(mean(height))
  # get difference from mean
  x = round((row[['height']]-m_height)*100,digits = 2)[1,1]
  if (x >= 0) {
    print(paste0(row[['names']],' is ',x,' cm taller than the average ',gender))
  } else {
    print(paste0(row[['names']],' is ',abs(x),' cm smaller than the average ', gender))
  }
  
}
check_height_2(df[2,])

#
check_height_3 <- function(df){
  l <- c()
  for(i in 1:nrow(df)){
    l[i] = check_height_2(df[i,])
  }
  df2 <- data.frame(df['names'],diff=l)
  
}
check_height_3(df)

