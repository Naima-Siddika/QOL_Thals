#load packages
library(tidyverse)
library(readxl)

# Load data
data <- read_excel("raw_data/QOL_Raw.xlsx")

#check missing values
is.na(data)
sum(is.na(data))

#remove missing values
data <- na.omit(data)



sf36 <- data |> 
  select(20:55)

# Renaming columns
colnames(sf36) <- paste0("Q", 1:36)

# Physical Functioning
physical_functioning <- sf36 |> 
  select(Q3:Q12) |> 
  mutate(Q3 = case_when(
    Q3 ==  "Yes, Limited a Little" ~ 0,
    Q3 ==  "No, Not Limited at all" ~ 50,
    Q3 ==  "Yes, Limited a lot" ~ 100
  ))


physical_functioning <- sf36 |> 
  select(Q3:Q12) |> 
  mutate(across(Q3:Q12, ~ case_when(
    .== "Yes, Limited a Little" ~ 0,
    .== "No, Not Limited at all" ~ 50,
    .== "Yes, Limited a lot" ~ 100,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(Physical_functioning = mean(c_across(Q3:Q12), na.rm = TRUE))

  
glimpse(physical_functioning)


unique(physical_functioning$Q3)


# Physical Role
physical_role <- sf36 |> 
  select(Q13:Q16) |> 
  mutate(across(Q13:Q16, ~ case_when(
    .== "Yes" ~ 0,
    .== "No" ~ 100,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(Physical_role = mean(c_across(Q13:Q16), na.rm = TRUE))


# Bodily pain
bodily_pain <- sf36 |> 
  select(Q17:Q19) |> 
  mutate(across(Q17:Q19, ~ case_when(
    .== "Yes" ~ 0,
    .== "No" ~ 100,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(Bodily_pain = mean(c_across(Q17:Q19), na.rm = TRUE)
         )



# General Health (Q1 SAME scale e nai)

# Corrected code to process and calculate 'general_health'
general_health <- sf36 |> 
  select(Q1, Q33:Q36) |> 
  mutate(
    Q1 = case_when(
      Q1 == "Excellent" ~ 100,
      Q1 == "Very Good" ~ 75,
      Q1 == "Good" ~ 50,
      Q1 == "Fair" ~ 25,
      Q1 == "Poor" ~ 0,
      TRUE ~ NA_real_
    ),
    
    Q33 = case_when(
      Q33 == "Definitely true" ~ 0,
      Q33 == "Mostly true" ~ 25,
      Q33 == "Don't know" ~ 50,
      Q33 == "Mostly false" ~ 75,
      Q33 == "Definitely false" ~ 100,
      TRUE ~ NA_real_
    ),
    
    Q34 = case_when(
      Q34 == "Definitely true" ~ 100,
      Q34 == "Mostly true" ~ 75,
      Q34 == "Don't know" ~ 50,
      Q34 == "Mostly false" ~ 25,
      Q34 == "Definitely false" ~ 0,
      TRUE ~ NA_real_
    ),

    Q35 = case_when(
      Q35 == "Definitely true" ~ 0,
      Q35 == "Mostly true" ~ 25,
      Q35 == "Don't know" ~ 50,
      Q35 == "Mostly false" ~ 75,
      Q35 == "Definitely false" ~ 100,
      TRUE ~ NA_real_
    ),
    
    Q36 = case_when(
      Q36 == "Definitely true" ~ 100,
      Q36 == "Mostly true" ~ 75,
      Q36 == "Don't know" ~ 50,
      Q36 == "Mostly false" ~ 25,
      Q36 == "Definitely false" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Calculate the general health score as row means using cbind
    General_health = rowMeans(cbind(Q1, Q33, Q34, Q35, Q36), na.rm = TRUE)
  )


unique(general_health$Q35)

# range = c_across
# rowmeans(cbind())

# vitality
vitality <- sf36 |> 
  select(Q23, Q27, Q29, Q31)

unique(vitality$Q23)

vitality <- sf36 |> 
  select(Q23, Q27, Q29, Q31) |> 
  mutate(
    Q23 = case_when(
      Q23 == "All of the time" ~ 100,
      Q23 == "Most of the time" ~ 80,
      Q23 == "A good Bit of the time" ~ 60,
      Q23 == "Some of the time" ~ 40,
      Q23 == "A little bit of the time" ~ 20,
      Q23 == "None of the Time" ~ 0,
      TRUE ~ NA_real_
    ),
    
      Q27 = case_when(
        Q27 == "All of the time" ~ 100,
        Q27 == "Most of the time" ~ 80,
        Q27 == "A good Bit of the time" ~ 60,
        Q27 == "Some of the time" ~ 40,
        Q27 == "A little bit of the time" ~ 20,
        Q27 == "None of the Time" ~ 0,
        TRUE ~ NA_real_
      ),
    
      Q29 = case_when(
        Q29 == "All of the time" ~ 0,
        Q29 == "Most of the time" ~ 20,
        Q29 == "A good Bit of the time" ~ 40,
        Q29 == "Some of the time" ~ 60,
        Q29 == "A little bit of the time" ~ 80,
        Q29 == "None of the Time" ~ 100,
        TRUE ~ NA_real_
      ),
    
      Q31 = case_when(
        Q31 == "All of the time" ~ 0,
        Q31 == "Most of the time" ~ 20,
        Q31 == "A good Bit of the time" ~ 40,
        Q31 == "Some of the time" ~ 60,
        Q31 == "A little bit of the time" ~ 80,
        Q31 == "None of the Time" ~ 100,
        TRUE ~ NA_real_
      ),
    Vitality = rowMeans(cbind(Q23, Q27, Q29, Q31), na.rm = TRUE)
  )
  

 # Social functioning
social_functioning <- sf36 |> 
  select(Q20, Q32)

unique(social_functioning$Q32)

social_functioning <- sf36 |> 
  select(Q20, Q32) |> 
  mutate(
    Q20 = case_when(
      Q20 == "None" ~ 100,
      Q20 == "Very Mild" ~ 75,
      Q20 == "Mild" ~ 75,
      Q20 =="Moderate" ~ 50,
      Q20 == "Severe"~ 25,
      Q20 == "Very Severe" ~ 0,
      TRUE ~ NA_real_
    ), 
    Q32 = case_when(
      Q32 == "All of the time" ~ 0,
      Q32 == "Most of the time" ~ 25,
      Q32 == "A good Bit of the time" ~ 50,
      Q32 == "Some of the time" ~ 50,
      Q32 == "A little bit of the time" ~ 75,
      Q32 == "None of the Time" ~ 100,
      TRUE ~ NA_real_
    ),
    Social_functioning = rowMeans(cbind(Q20, Q32), na.rm = TRUE)
  )


# emotional role
emotional_role <- sf36 |> 
  select(Q24, Q25, Q26, Q28, Q30) |> 
  mutate(
    across(c(Q24, Q25), ~ case_when(
    .== "All of the time" ~ 0,
    .== "Most of the time" ~ 20,
    .== "A good Bit of the time" ~ 40,
    .== "Some of the time" ~ 60,
    .== "A little bit of the time" ~ 80,
    .== "None of the Time" ~ 100,
    TRUE ~ NA_real_
  )),

    Q30 = case_when(
    Q30 == "All of the time" ~ 100,
    Q30 =="Most of the time" ~ 80,
    Q30 == "A good Bit of the time" ~ 60,
    Q30 == "Some of the time" ~ 40,
    Q30 == "A little bit of the time" ~ 20,
    Q30 == "None of the Time" ~ 0,
    TRUE ~ NA_real_
  ),
  Q26 = case_when(
    Q26 == "All of the time" ~ 100,
    Q26 =="Most of the time" ~ 80,
    Q26 == "A good Bit of the time" ~ 60,
    Q26 == "Some of the time" ~ 40,
    Q26 == "A little bit of the time" ~ 20,
    Q26 == "None of the Time" ~ 0,
    TRUE ~ NA_real_
  ),
   Q28 = case_when(
    Q28 == "All of the time" ~ 0,
    Q28 == "Most of the time" ~ 20,
    Q28 == "A good Bit of the time" ~ 40,
    Q28 == "Some of the time" ~ 60,
    Q28 == "A little bit of the time" ~ 80,
    Q28 == "None of the Time" ~ 100,
    TRUE ~ NA_real_
  ),
Emotional_Role = rowMeans(cbind(Q24, Q25, Q26, Q28, Q30), na.rm = TRUE)
)
  
  unique(emotional_role$Q24)

# pain
pain <- sf36 |> 
  select(Q21, Q22)


pain <- sf36 |> 
  select(Q21, Q22) |> 
  mutate(
    Q21 = case_when(
      Q21 == "None" ~ 100,
      Q21 == "Very Mild" ~ 80,
      Q21 == "Mild" ~ 60,
      Q21 =="Moderate" ~ 40,
      Q21 == "Severe"~ 20,
      Q21 == "Very Severe" ~ 0,
      TRUE ~ NA_real_
    ),
    Q22 = case_when(
    Q22 == "Not at all" ~ 100,
    Q22 == "A little bit" ~ 75,
    Q22 == "Quite a bit" ~ 50,
    Q22 == "Moderately" ~ 25,
    Q22 == "Extremely"  ~ 0,
    TRUE ~ NA_real_
  ),
  Pain = rowMeans(cbind(Q21, Q22), na.rm = TRUE)
  )

unique(pain$Q22)

# 8 domain
# combined the data
demographics <- data |> 
  select(1:19)

sf_domains <- cbind(demographics, physical_functioning, physical_role, bodily_pain, general_health, vitality, social_functioning, emotional_role, pain)

sf_domains <- sf_domains |> 
  select(physical_functioning, physical_role, bodily_pain, general_health, vitality, Social_functioning, Emotional_Role, Pain)

sf_domains <- sf_domains |> 
  select(Physical_functioning, Physical_role, Bodily_pain, General_health, Vitality, 
         Social_functioning, Emotional_Role, Pain)
qol_data <- cbind(demographics, sf_domains)

# PHYSICAL HEALTH 
qol_data <- qol_data |> 
  select(Physical_functioning, Physical_role, Bodily_pain, General_health, 
         Vitality, Social_functioning, Emotional_Role, Pain ) |> 
  mutate(Physical_Health = rowMeans(cbind(Physical_functioning, Physical_role, Bodily_pain, General_health, Vitality), na.rm = TRUE)) |> 
  mutate(Mental_Health = rowMeans(cbind(Social_functioning, Emotional_Role, Pain), na.rm = TRUE )) |> 
  mutate(QOL_Score = rowMeans(Physical_Health, Mental_Health), na.rm = TRUE ))

sf_domains <-  sf_domains |> 
select(Physical_functioning, Physical_role, Bodily_pain, General_health, 
       Vitality, Social_functioning, Emotional_Role, Pain ) |> 
  mutate(Physical_Health = rowMeans(cbind(Physical_functioning, Physical_role, Bodily_pain, General_health, Vitality), na.rm = TRUE)) |> 
  mutate(Mental_Health = rowMeans(cbind(Social_functioning, Emotional_Role, Pain), na.rm = TRUE )) |> 
  mutate(QOL_Score = rowMeans(Physical_Health, Mental_Health), na.rm = TRUE )

sf_domains <- sf_domains |> 
  select(Physical_functioning, Physical_role, Bodily_pain, General_health, 
         Vitality, Social_functioning, Emotional_Role, Pain) |> 
  mutate(
    # Calculate Physical_Health as the row mean of physical domains
    Physical_Health = rowMeans(cbind(Physical_functioning, Physical_role, Bodily_pain, General_health, Vitality), na.rm = TRUE),
    
    # Calculate Mental_Health as the row mean of mental domains
    Mental_Health = rowMeans(cbind(Social_functioning, Emotional_Role, Pain), na.rm = TRUE),
    
    # Calculate QOL_Score as the row mean of Physical_Health and Mental_Health
    QOL_Score = rowMeans(cbind(Physical_Health, Mental_Health), na.rm = TRUE)
  )
