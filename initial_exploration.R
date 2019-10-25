library(data.table)

student_dt = fread("SM_2012_13_20141103_01.csv")
staff_dt = fread("STT_2012_13_20141103.csv")
teaching_dt = fread("STA_2012_13_20141103")

staff_dt[rowSums(is.na(staff_dt[ , 1:34])) == 0, ]
