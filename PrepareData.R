


#######################################################################################################

#Read the school data from the csv data file into R:
d.school.r <- read.csv(
  file=
  "./Data/School_data.csv",
  header=TRUE)

#Check the variable names and dimensions in the data frame
names(d.school.r)
dim(d.school.r)




quest_names.sch <- c("1.wife not hunts/works", "2.daughter babysits", "3.not wear dead hat",
               "4.hit students", "5.no questions", "6.post flu", "7.not post chest",
               "8.pot to needy", "9.good nonbaptized not heaven", "10.bad baptized heaven",
               "11.stop work to visit", "12.cheap mean store", "13.xcousin marriage",
               "14.arranged marriage", "15.laborer party")



