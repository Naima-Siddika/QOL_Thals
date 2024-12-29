# Load packages
library(tidyverse)
library(readxl)

# Load data
data <- read_excel("raw_data/QOL_Raw.xlsx")

# Check missing values
is.na(data)
sum(is.na(data))

# Remove missing values
data <- na.omit(data)


sf_36 <- data |> 
  select(20:55)

# Renaming columns
colnames(sf_36) <- paste0("Q", 1:36)


# 1. Physical functioning
Physical_functioning <- sf_36 |> 
  select(Q3:12) |> 
  mutate(
   across(Q3:Q12, ~ case_when(
   .== "Yes, Limited a Lot" ~ 0,
   .== "Yes, Limited a Little" ~ 50,
   .== "No, Not Limited at all" ~ 100,
   TRUE ~ NA_real_
   ) ),
   Physical_functioning = rowMeans(across(Q3:Q12), na.rm = TRUE)
   )


unique(Physical_functioning$Q3)

# 2. Physical role
Physical_role <- sf_36 |> 
  select(Q13:Q16) |> 
  mutate(
    across(Q13:Q16, ~ case_when(
     .== "Yes" ~ 0,
     .== "No" ~ 100,
     TRUE ~ NA_real_
    )),
    Physical_role = rowMeans(across(Q13:Q16), na.rm = TRUE)
    )


unique(Physical_role$Q13)


# 3. bodily pain
Bodily_pain <- sf_36 |>
  select(Q21, Q22) |> 
  mutate(
    Q21 = case_when(
    Q21 == "None" ~ 100,
    Q21 == "Very Mild" ~ 80,
    Q21 == "Mild" ~ 60,
    Q21 == "Moderate" ~ 40,
    Q21 == "Severe" ~ 20,
    Q21 == "Very Severe" ~ 0,
    TRUE ~ NA_real_
    ),
    Q22 = case_when(
    Q22 == "Not at all" ~ 100,
    Q22 == "A little bit" ~ 75,
    Q22 == "Moderately" ~ 50,
    Q22 == "Quite a bit" ~ 25,
    Q22 == "Extremely" ~ 0,
    TRUE ~ NA_real_
    ),
    Bodily_pain = rowMeans(cbind(Q21, Q22), na.rm = TRUE)
    )

unique(Bodily_pain$Q22)

# 4. General Health
General_health <- sf_36 |> 
  select(Q1, Q33:Q36) |> 
  mutate(
    Q1 = case_when(
      Q1 == "Excellent" ~ 100,
      Q1 == "Very Good" ~ 75,
      Q1 == "Good"~ 50,
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
  General_health = rowMeans(cbind(Q1, Q33, Q34, Q35, Q36), na.rm = TRUE)
  
  )


unique(General_health$Q1)

# 5. Vitality
Vitality <- sf_36 |> 
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


# 6. Emotinal role
Emotional_role <- sf_36 |> 
  select(Q17:Q19) |> 
  mutate(
    across(Q17:Q19, ~ case_when(
      .== "Yes" ~ 0,
      .== "No" ~ 100,
      TRUE ~ NA_real_
    )),
    Emotional_role = rowMeans(across(Q17:Q19), na.rm = TRUE)
    )

unique(Emotional_role$Q17)


# 7. Social functioning
Social_functioning <- sf_36 |> 
  select(Q20, Q32) |> 
  mutate(
    Q20 = case_when(
    Q20 == "None" ~ 100,
    Q20 == "Very Mild" ~ 75,
    Q20 == "Mild" ~ 75,
    Q20 == "Moderate" ~ 50,
    Q20 == "Severe" ~ 25,
    Q20 == "Very Severe" ~ 0,
    TRUE ~ NA_real_
    ),
     Q32 = case_when(
    Q32 == "All of the time" ~ 0,
    Q32 == "Most of the time" ~ 25,
    Q32 == "Some of the time" ~ 50,
    Q32 == "A little bit of the time" ~ 75,
    Q32 == "None of the Time" ~ 100,
    TRUE ~ NA_real_
     ),
    Social_functioning = rowMeans(cbind(Q20, Q32), na.rm = TRUE)
  )


unique(social_functioning$Q32)

# 8. Mental Health
Mental_health <- sf_36 |> 
  select(Q24, Q25, Q26, Q28, Q30) |>
  mutate(
    Q24 = case_when(
    Q24 == "All of the time" ~ 0,
    Q24 == "Most of the time" ~ 20,
    Q24 == "A good Bit of the time" ~ 40,
    Q24 == "Some of the time" ~ 60,
    Q24 == "A little bit of the time" ~ 80,
    Q24 == "None of the Time" ~ 100,
    TRUE ~ NA_real_
    ),
    Q25 = case_when(
      Q25 == "All of the time" ~ 0,
      Q25 == "Most of the time" ~ 20,
      Q25 == "A good Bit of the time" ~ 40,
      Q25 == "Some of the time" ~ 60,
      Q25 == "A little bit of the time" ~ 80,
      Q25 == "None of the Time" ~ 100,
      TRUE ~ NA_real_
    ),
    Q26 = case_when(
      Q26 == "All of the time" ~ 100,
      Q26 == "Most of the time" ~ 80,
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
    Q30 = case_when(
      Q30 == "All of the time" ~ 100,
      Q30 == "Most of the time" ~ 80,
      Q30 == "A good Bit of the time" ~ 60,
      Q30 == "Some of the time" ~ 40,
      Q30 == "A little bit of the time" ~ 20,
      Q30 == "None of the Time" ~ 0,
      TRUE ~ NA_real_
    ),
    Mental_health = rowMeans(cbind(Q24, Q25, Q26, Q28, Q30), na.rm = TRUE)
  )

unique(Mental_health$Q24)
# Combined the data

demographics <- data |> 
  select(1:19)

sf_domains <- cbind(demographics, Physical_functioning, Physical_role, Bodily_pain, General_health,Vitality,
                    Social_functioning, Emotional_role, Mental_health)
