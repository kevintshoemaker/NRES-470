################# 
# UPDATED SPRING 2026

Base_MS_RA = 2400.00     # annual (1 month)
Base_MS_TA = 1700.00     # annual (1 month)
Base_MS_TA2 = 2200.00     # annual (1 month)
Base_PhD_RA = 2400.00   # annual (1 month)
Base_PhD_TA = 2200.00   # annual (1 month)
Base_PhD_RA_DRIVE = 2500.00   # annual (1 month)

Tuition_res_9cred = 383.25*9 + 383.25*9    # annual $6,899
Health_insurance = 2684+2684   # annual $5,368 (domestic)
Health_insurance_int = 1710 + 1710    # annual $3,420 (international)
Tuition_nonres = 10094.50 + 10094.50  # annual (for nonresidents) $20,189  (both domestic and international)

MS_yrs = 2
PhD_yrs = 4 

TotVal_res_ms = Base_MS_TA * 10 * MS_yrs +  (Tuition_res_9cred*MS_yrs)*1 + Health_insurance*MS_yrs 
TotVal_nonres_ms = Base_MS_TA * 10 * MS_yrs +  (Tuition_res_9cred*MS_yrs)*1 + Health_insurance*MS_yrs + Tuition_nonres*MS_yrs
TotVal_res_phd = Base_PhD_TA * 10 * PhD_yrs +  (Tuition_res_9cred*PhD_yrs)*1 + Health_insurance*PhD_yrs 
TotVal_nonres_phd = Base_PhD_TA * 10 * PhD_yrs +  (Tuition_res_9cred*PhD_yrs)*1 + Health_insurance*PhD_yrs + Tuition_nonres*PhD_yrs
TotVal_nonres_phd_int = Base_PhD_TA * 10 * PhD_yrs +  (Tuition_res_9cred*PhD_yrs)*1 + Health_insurance_int*PhD_yrs + Tuition_nonres*PhD_yrs

TotVal_res_ms     # $58,533
TotVal_nonres_ms    # $98,911
TotVal_res_phd      # $137,066
TotVal_nonres_phd    # $217,822
TotVal_nonres_phd_int   # 210,030

TotVal_1yr_natalia = Base_PhD_RA_DRIVE * 10 +  (Tuition_res_9cred)*1 + Health_insurance_int + Tuition_nonres   # 55,507.50
TotVal_1yr_natalia2 = Base_PhD_RA_DRIVE * 10 +  (Tuition_res_9cred)*1 + Health_insurance_int + Tuition_nonres + Base_PhD_RA*2  # 60,307.50


fees = list()
fees$newstudent = 35
fees$academic_success = 5
fees$student_union = 97
fees$varsity_athletics = 3.5*9
fees$counseling = 95
fees$fitness_center = 65
fees$health_center = 125
fees$performing_arts = 5
fees$technology = 18*9

do.call("sum",fees)   # 620.5 first semester 
do.call("sum",fees)-fees$newstudent   # 585.5 after (fall)
do.call("sum",fees)-fees$newstudent - fees$varsity_athletics # 554 after (spring)

fees$international = 145
do.call("sum",fees)     # 765.5 first semester
do.call("sum",fees)-fees$newstudent   # 730.5 after (fall)
do.call("sum",fees)-fees$newstudent - fees$varsity_athletics # 699 after (spring)


#################
