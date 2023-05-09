********** Quantitative Methods for Policy Evaluation
********** Fall 2020 CUHKSZ
********** Randomized Control Trials
********** Prepared by Chongyu Fang



********** Basic Setup **********

clear /* clears memory of all data */

set mem 100m /* setting memory size */

set matsize 500 /* setting number of RHS variables in a model */

set more off /* turn the above thing off if you want it to do stuff continuously */

cap log close /* close the log file if any already open */

pwd /* shows us what directory Stata treats as working directory at the moment */

cd "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Assignments/Assignment 1/PS1_Data/CEPS_wave1" /* change directory */

***** Open a log file *****

log using PS1_Chongyu_Fang.log, text replace /* creates a new log file for storing results */

***** Load data *****

use cepsw1parentEN.dta, clear /* parent */

***** Merge the samples of students, classes and schools to parent *****

merge 1:1 ids using cepsw1studentEN.dta /* student */

drop _merge

merge m:1 clsids using cepsw1teacherEN.dta /* teacher (classes) */

drop _merge

merge m:1 schids using cepsw1principalEN.dta /* principal (schools) */

drop _merge

***** Only keep schools that RANDOMLY assign students into classes to satisfy RCT design *****

keep if ple1503 == 1

***** Define treatment groups and control groups *****

gen treatment = 1 if matb01 == 2 /* treatment group: math teacher is female */

replace treatment = 0 if matb01 == 1 /* control group: math teacher is male */

***** Generate a new variable indicating ethnicity *****

gen ethn = 1 if a03 == 1 /* Han ethnicity */

replace ethn = 0 if a03 != 1 /* non-Han ethnicity */

***** Generate a new variable indicating whether student is local resident *****

gen localrsdt = 1 if a05 == 1 /* local */

replace localrsdt = 0 if a05 == 2 /* non-local */

***** Generate a new variable indicating student's birth weights *****

gen weight = 2.5 if a09 == 1 /* light */

replace weight = 3.5 if a09 == 2 /* normal */

replace weight = 4.5 if a09 == 3 /* heavy */

***** Generate a new variable indicating whether student is the only child at home *****

gen onlychild = 1 if b01 == 1 /* only child */

replace onlychild = 0 if b01 == 2 /* not the only child */

***** Generate a new variable indicating father's years of schooling *****

gen fedu = 0 if stfedu == 1 /* never educated */

replace fedu = 6 if stfedu == 2 /* primary school */

replace fedu = 9 if stfedu == 3 /* junior high school */

replace fedu = 12 if stfedu == 4 /* technical secondary school (ZhongZhuan) */

replace fedu = 12 if stfedu == 5 /* professional high school (ZhiGao) */

replace fedu = 12 if stfedu == 6 /* high school */

replace fedu = 15 if stfedu == 7 /* junior college (DaZhuan) */

replace fedu = 15 if stfedu == 8 /* undergraduate */

replace fedu = 19 if stfedu == 9 /* postgraduate */

***** Generate a new variable indicating mother's years of schooling *****

gen medu = 0 if stmedu == 1 /* never educated */

replace medu = 6 if stmedu == 2 /* primary school */

replace medu = 9 if stmedu == 3 /* junior high school */

replace medu = 12 if stmedu == 4 /* technical secondary school (ZhongZhuan) */

replace medu = 12 if stmedu == 5 /* professional high school (ZhiGao) */

replace medu = 12 if stmedu == 6 /* high school */

replace medu = 15 if stmedu == 7 /* junior college (DaZhuan) */

replace medu = 15 if stmedu == 8 /* undergraduate */

replace medu = 19 if stmedu == 9 /* postgraduate */

***** Generate a new variable indicating student's preschool education *****

gen preedu = 1 if c01 == 1 /* went to kindergarten before */

replace preedu = 0 if c01 == 2 /* never went to kindergarten before */

***** Generate a new variable indicating student's math performance in primary school sixth grade *****

gen primath = 60 if c1001 == 1

replace primath = 70 if c1001 == 2

replace primath = 80 if c1001 == 3

replace primath = 90 if c1001 == 4

replace primath = 100 if c1001 == 5

***** Balancing test for student's gender *****

reg stsex treatment, robust

***** Balancing test for student's ethnicity *****

reg ethn treatment, robust

***** Balancing test for whether student is local *****

reg localrsdt treatment, robust

***** Balancing test for student's birth weight *****

reg weight treatment, robust

***** Balancing test for whether student is only child at home *****

reg onlychild treatment, robust

***** Balancing test for student father's years of schooling *****

reg fedu treatment, robust

***** Balancing test for student mather's years of schooling *****

reg medu treatment, robust

***** Balancing test for student's preschool education *****

reg preedu treatment, robust

***** Balancing test for student's math performance in primary school *****

reg primath treatment, robust

***** What about balancing tests within school? *****

egen group = group(schids grade9) /* generate groups */

reg stsex treatment i.group, robust

reg ethn treatment i.group, robust

reg localrsdt treatment i.group, robust

reg weight treatment i.group, robust

reg onlychild treatment i.group, robust

reg fedu treatment i.group, robust

reg medu treatment i.group, robust

reg preedu treatment i.group, robust

reg primath treatment i.group, robust

***** Find treatment effect on student's math score *****

reg stdmat treatment, robust /* pooled */

reg stdmat treatment i.group, robust /* within school */

***** Check heterogeneous treatment effect across student's gender *****

reg stdmat treatment if a01 == 1, robust /* treatment effect on boys */

reg stdmat treatment if a01 == 2, robust /* treatment effect on girls */

reg stdmat treatment i.group if a01 == 1, robust

reg stdmat treatment i.group if a01 == 2, robust

***** Close the log file *****

cap log close
























