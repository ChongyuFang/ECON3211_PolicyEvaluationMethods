********** ECO3211 Quantitative Methods for Policy Evaluation

********** Fall 2020, CUHKSZ

********** Problem Set 3 Difference-in-Differences

********** Prepared by Chongyu Fang

********** Basic Setup **********

* clears memory of all data

clear

* close the log file if any already open

capture log close

* turn the -more- off if you want STATA to report results continuously

set more off

* set our working directory

cd "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/PS3_Data"

* open a log file

log using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/PS3_Chongyu_Fang.log", text replace

* load in data and append

use "judicature_2010.dta", clear

append using "judicature_2012.dta"

append using "judicature_2014.dta"

append using "judicature_2016.dta"

* merge provincial characteristics with judicature data

merge m:1 hk_now_prov using "province.dta"

* let STATA identify the panel data by province id and reform year

xtset pid year

sort pid year

* generate the variable representing the time of reform

gen t0=2014 if hk_now_prov==22
replace t0=2014 if hk_now_prov==31
replace t0=2014 if hk_now_prov==42
replace t0=2014 if hk_now_prov==44
replace t0=2014 if hk_now_prov==46
replace t0=2014 if hk_now_prov==52
replace t0=2014 if hk_now_prov==63

replace t0=2015 if hk_now_prov==14
replace t0=2015 if hk_now_prov==15
replace t0=2015 if hk_now_prov==23
replace t0=2015 if hk_now_prov==32
replace t0=2015 if hk_now_prov==33
replace t0=2015 if hk_now_prov==34
replace t0=2015 if hk_now_prov==35
replace t0=2015 if hk_now_prov==37
replace t0=2015 if hk_now_prov==50
replace t0=2015 if hk_now_prov==53
replace t0=2015 if hk_now_prov==64

replace t0=2016 if hk_now_prov==11 
replace t0=2016 if hk_now_prov==12
replace t0=2016 if hk_now_prov==13
replace t0=2016 if hk_now_prov==21
replace t0=2016 if hk_now_prov==36
replace t0=2016 if hk_now_prov==41
replace t0=2016 if hk_now_prov==43
replace t0=2016 if hk_now_prov==45
replace t0=2016 if hk_now_prov==51
replace t0=2016 if hk_now_prov==54
replace t0=2016 if hk_now_prov==61
replace t0=2016 if hk_now_prov==62
replace t0=2016 if hk_now_prov==65

* generate post dummy for the status of having initiated the reform

gen Post = 1 if year >= t0

replace Post = 0 if year < t0

* generate treatment dummy

gen Treat = 1

* generate interaction term

gen Treat_Post = Treat*Post

* check whether some variables can predict the timing of reform

reg t0 population revenue expenditure highschool rural_inc urban_inc if year==2010, cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results1.xls", excel dec(3) replace

* coefficients of all these variables are significant, we should include them in regression

* then we construct f(Wi, t) using first-order and second-order polynomial of time

gen xp1 = population*year

gen xp2 = population*year*year

gen xr1 = revenue*year

gen xr2 = revenue*year*year

gen xe1 = expenditure*year

gen xe2 = expenditure*year*year

gen xh1 = highschool*year

gen xh2 = highschool*year*year

gen xri1 = rural_inc*year

gen xri2 = rural_inc*year*year

gen xui1 = urban_inc*year

gen xui2 = urban_inc*year*year

gen year2 = year*year

* DID estimation of impacts of judicial reforms on citizen's evaluation of local government

* DID without controlling for f(Wi, t)

xi: xtreg govnwork Treat_Post i.year, fe cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results2.xls", drop(_Iyear*) excel dec(3) replace

* DID controlling for f(Wi, t)

xi: xtreg govnwork Treat_Post i.year xp* xr* xe* xh* xri* xui*, fe cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results2.xls", drop(_Iyear*) excel dec(3)

* DID controlling for first-order province time trend

xi: xtreg govnwork Treat_Post i.year i.hk_now_prov*year, fe cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results2.xls", drop(_Iyear*) excel dec(3)

* DID controlling for second-order province time trend

xi: xtreg govnwork Treat_Post i.year i.hk_now_prov*year2, fe cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results2.xls", drop(_Iyear*) excel dec(3)

* check common trend assumption now

* generate Treat*Year dummy to replace Treat*Post dummy

* note that in this dataset, one period equals two years

gen lag3 = t0 - 6

gen pre3 = 0

replace pre3 = 1 if year == lag3

gen lag2 = t0 - 4

gen pre2 = 0

replace pre2 = 1 if year == lag2

gen lag1 = t0 - 2

gen pre1 = 0

replace pre1 = 1 if year == lag1

gen pre0 = 0

replace pre0 = 1 if year == t0

gen lead1 = t0 + 2

gen post1 = 0

replace post1 = 1 if year >= lead1

* generate Treat*Period dummies

gen treatpre3 = Treat*pre3

gen treatpre2 = Treat*pre2

gen treatpre1 = Treat*pre1

gen treatpre0 = Treat*pre0

gen treatpost1 = Treat*post1

* DID using Treat*Period dummies 

xi: xtreg govnwork treatpre3 treatpre2 treatpre1 treatpre0 treatpost1 i.year i.hk_now_prov*year, fe cluster(pid)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 3/results3.xls", drop(_Iyear* xp* xr* xe* xh* xri* xui*) excel dec(3) replace

* test that the post-treatment coefficients (not 0) are significantly different from pre-treatment coefficients (0)

test treatpre3 = treatpre2 = treatpre1 = treatpre0 = treatpost1 = 0

* Finished! Close the log file

cap log close
