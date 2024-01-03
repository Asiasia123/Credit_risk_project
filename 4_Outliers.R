# Analysis: Outliers
# Comment: Excluding outliers causes excluding almost 44% of data (too much to omit)


## ----Outliers  - 1st check----

boxplot(mig_data$ORIGINAL_EXPOSURE) # replace 3rd Qu.  and lower  1st Qu. 
boxplot(mig_data$EAD_AMOUNT)        # replace 3rd Qu. and lower  1st Qu. 
boxplot(mig_data$RWA)               # replace 3rd Qu. and lower  1st Qu. 
boxplot(mig_data$EXPECTED_LOSS)     # replace 3rd Qu. and lower Median 

# Replacing outliers with NaN
mig_data_outliers <- mig_data %>% 
  mutate(
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE > quantile(ORIGINAL_EXPOSURE, 0.75, na.rm = TRUE), NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT > quantile(EAD_AMOUNT, 0.75, na.rm = TRUE), NaN, EAD_AMOUNT),
    RWA = ifelse(RWA > quantile(RWA, 0.75, na.rm = TRUE), NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS > quantile(EXPECTED_LOSS, 0.75, na.rm = TRUE), NaN, EXPECTED_LOSS),
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE < quantile(ORIGINAL_EXPOSURE, 0.25, na.rm = TRUE), NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT < quantile(EAD_AMOUNT, 0.25, na.rm = TRUE), NaN, EAD_AMOUNT),
    RWA = ifelse(RWA < quantile(RWA, 0.25, na.rm = TRUE), NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS < quantile(EXPECTED_LOSS, 0.25, na.rm = TRUE), NaN, EXPECTED_LOSS)
  )

summary(mig_data_outliers)

boxplot(mig_data_outliers$ORIGINAL_EXPOSURE)  
boxplot(mig_data_outliers$EAD_AMOUNT)        
boxplot(mig_data_outliers$RWA)               
boxplot(mig_data_outliers$EXPECTED_LOSS)

## ----Outliers  - 2nd check----

boxplot(mig_data$ORIGINAL_EXPOSURE) # 1 highest and replace 0 with NaN
boxplot(mig_data$EAD_AMOUNT)        # 1 highest and replace 0 with NaN
boxplot(mig_data$RWA)               # 2 highest and replace 0 with NaN
boxplot(mig_data$EXPECTED_LOSS)     # 2 highest and replace 0 with NaN

# Replacing outliers with NaN
mig_data_outliers <- mig_data %>% 
  mutate(
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE %in% tail(sort(ORIGINAL_EXPOSURE), 1), NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT %in% tail(sort(EAD_AMOUNT), 1), NaN, EAD_AMOUNT),
    RWA = ifelse(RWA %in% tail(sort(RWA), 2), NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS %in% tail(sort(EXPECTED_LOSS), 2), NaN, EXPECTED_LOSS),
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE == 0, NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT == 0, NaN, EAD_AMOUNT),
    RWA = ifelse(RWA == 0, NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS == 0, NaN, EXPECTED_LOSS)
  )

boxplot(mig_data_outliers$ORIGINAL_EXPOSURE)  
boxplot(mig_data_outliers$EAD_AMOUNT)        
boxplot(mig_data_outliers$RWA)               
boxplot(mig_data_outliers$EXPECTED_LOSS)

## ----Outliers  - 3rd check----

boxplot(mig_data$ORIGINAL_EXPOSURE) # 1 highest and lower than 1st Qu. = 7
boxplot(mig_data$EAD_AMOUNT)        # 1 highest and lower than 1st Qu. = 5
boxplot(mig_data$RWA)               # 2 highest and lower than 1st Qu. = 1
boxplot(mig_data$EXPECTED_LOSS)     # 2 highest and lower than Median = 0.1

# Replacing outliers with NaN
mig_data_outliers <- mig_data %>% 
  mutate(
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE %in% tail(sort(ORIGINAL_EXPOSURE), 1), NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT %in% tail(sort(EAD_AMOUNT), 1), NaN, EAD_AMOUNT),
    RWA = ifelse(RWA %in% tail(sort(RWA), 2), NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS %in% tail(sort(EXPECTED_LOSS), 2), NaN, EXPECTED_LOSS),
    ORIGINAL_EXPOSURE = ifelse(ORIGINAL_EXPOSURE < quantile(ORIGINAL_EXPOSURE, 0.25, na.rm = TRUE), NaN, ORIGINAL_EXPOSURE),
    EAD_AMOUNT = ifelse(EAD_AMOUNT < quantile(EAD_AMOUNT, 0.25, na.rm = TRUE), NaN, EAD_AMOUNT),
    RWA = ifelse(RWA < quantile(RWA, 0.25, na.rm = TRUE), NaN, RWA),
    EXPECTED_LOSS = ifelse(EXPECTED_LOSS < 0.1, NaN, EXPECTED_LOSS)
  )

boxplot(mig_data_outliers$ORIGINAL_EXPOSURE)  
boxplot(mig_data_outliers$EAD_AMOUNT)        
boxplot(mig_data_outliers$RWA)               
boxplot(mig_data_outliers$EXPECTED_LOSS)
