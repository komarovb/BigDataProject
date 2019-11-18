library(data.table)

# student_dt = fread("data/SM_2012_13_20141103_01.csv")
# staff_dt = fread("STT_2012_13_20141103.csv")
# teaching_dt = fread("STA_2012_13_20141103")

# staff_dt[rowSums(is.na(staff_dt[ , 1:34])) == 0, ]

# dt[order(-dt[, STUDENT_AGE_VALUE])[1],]

# Useful functions: 
# write.csv(carSpeeds, file = 'data/car-speeds-cleaned.csv', row.names = FALSE)
# carSpeeds$Color = ifelse(carSpeeds$Color == 'Blue', 'Green', carSpeeds$Color)
# str(df)
# print(factor_data)
# colnames(df)
# names(df)
# rownames(df)
# data[data$Code %in% selected,]
# df[is.na(df$Fare),]
# tmp = rbind(tmp, tmp2)

# String to number!
# as.numeric(s_dt[1, STUDY_GRANT_AMT])

# 'o' %in% missing_values

# WORKING ON MISSING VALUES ------------------------------

# Remove selected columns
remove_columns <- function(dt, columns_to_drop) {
  col_names = colnames(dt)
  col_names = col_names[! col_names %in% columns_to_drop]
  dt = dt[, ..col_names]
  return(dt)
}

dt = fread("data/SM_2012_13_20141103_01.csv") # Reading the whole data

# Filter unnecessary attributes based on the data structure for each type of placement
# Remove for all:
# ID_MOBILITY_CDE, CONSORTIUM_AGREEMENT_NUMBER, SPECIAL_NEEDS_SUPPLEMENT_VALUE, 
# ????? SHORT_DURATION_CDE ????? QUALIFICATION_AT_HOST_CDE ?????
drop_for_all = c('ID_MOBILITY_CDE', 
                 'CONSORTIUM_AGREEMENT_NUMBER', 'SPECIAL_NEEDS_SUPPLEMENT_VALUE', 
                 'SHORT_DURATION_CDE', 'QUALIFICATION_AT_HOST_CDE')

dt = remove_columns(dt, drop_for_all)

# Split data based on what was the type of placement for further processing
s_dt = dt[ dt$MOBILITY_TYPE_CDE == 'S',]
s_p_dt = dt[ dt$MOBILITY_TYPE_CDE == 'C',]
p_dt = dt[ dt$MOBILITY_TYPE_CDE == 'P',]

# Remove for 'S' placement
# MOBILITY_TYPE_CDE, PLACEMENT_ENTERPRISE_VALUE, PLACEMENT_ENTERPRISE_CTRY_CDE, 
# PLACEMENT_ENTERPRISE_SIZE_CDE, TYPE_PLACEMENT_SECTOR_VALUE", LENGTH_PLACEMENT_VALUE, 
# PLACEMENT_START_DATE, ECTS_CREDITS_PLACEMENT_AMT, PLACEMENT_GRANT_AMT
# ????? TOTAL_ECTS_CREDITS_AMT ?????
drop_for_s = c('MOBILITY_TYPE_CDE', 'PLACEMENT_ENTERPRISE_VALUE', 'PLACEMENT_ENTERPRISE_CTRY_CDE', 
               'PLACEMENT_ENTERPRISE_SIZE_CDE', 'TYPE_PLACEMENT_SECTOR_VALUE', 'LENGTH_PLACEMENT_VALUE', 
               'PLACEMENT_START_DATE', 'ECTS_CREDITS_PLACEMENT_AMT', 'PLACEMENT_GRANT_AMT')
s_dt = remove_columns(s_dt, drop_for_s)

# Candidates for descriptive analytics:
# TAUGHT_HOST_LANGUAGE_CDE, LANGUAGE_TAUGHT_CDE
# STUDY_GRANT_AMT, PLACEMENT_GRANT_AMT, PREVIOUS_PARTICIPATION_CDE, SPECIAL_NEEDS_SUPPLEMENT_VALUE
# ....

# Check for missing values & Feature selection
missing_values = c('? Unknown ?', '???', '?')

# Character fields: 
# HOME_INSTITUTION_CDE, HOME_INSTITUTION_CTRY_CDE, STUDENT_GENDER_CDE, STUDENT_NATIONALITY_CDE, 
# STUDENT_SUBJECT_AREA_VALUE, STUDENT_STUDY_LEVEL_CDE, HOST_INSTITUTION_CDE, HOST_INSTITUTION_COUNTRY_CDE,
# TAUGHT_HOST_LANGUAGE_CDE, LANGUAGE_TAUGHT_CDE, LINGUISTIC_PREPARATION_CDE, PREVIOUS_PARTICIPATION_CDE,
# STUDY_GRANT_AMT

# DATE: STUDY_START_DATE

s_dt[ s_dt$STUDENT_GENDER_CDE %in% missing_values, ]
s_dt[ s_dt$LINGUISTIC_PREPARATION_CDE %in% missing_values, ]

colums_to_check = c('HOME_INSTITUTION_CDE', 'HOME_INSTITUTION_CTRY_CDE',  'STUDENT_GENDER_CDE', 
                    'STUDENT_NATIONALITY_CDE', 'STUDENT_SUBJECT_AREA_VALUE', 'STUDENT_STUDY_LEVEL_CDE',
                    'HOST_INSTITUTION_CDE', 'HOST_INSTITUTION_COUNTRY_CDE', 'TAUGHT_HOST_LANGUAGE_CDE',
                    'LANGUAGE_TAUGHT_CDE', 'LINGUISTIC_PREPARATION_CDE', 'PREVIOUS_PARTICIPATION_CDE', 'STUDY_GRANT_AMT')
missing_values_per_feature = c()
for (i in 1:length(colums_to_check))
{
  col = colums_to_check[i]
  number_of_missing_values = nrow(s_dt[s_dt[[col]] %in% missing_values, ..col])
  missing_values_per_feature[i] = number_of_missing_values
}

# For 'S' placement
# number_of_missing_values = nrow(s_dt[s_dt[[col]] %in% missing_values, ..col])

# We focus on student placements

# Save data without missing values and unnecessary attributes to a new .csv file





# ------------------------------

# TODO before presentation:
# Do basic feature selection! 17.11.2019
# Deal with missing values 11.11.2019 - 18.11.2019

# Implement multiple scenarious of clustering 18.11.2019
# Implement some kind of classification / regression 18.11.2019

# Add some descriptive analytics and visualisation to the initial results 19.11.2019
# Make presentaiton 19.11.2019




















