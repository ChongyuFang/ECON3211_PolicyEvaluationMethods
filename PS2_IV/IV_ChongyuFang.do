********** ECO3211 Quantitative Methods for Policy Evaluation

********** Fall 2020, CUHKSZ

********** Instrumental Variable

********** Prepared by Chongyu Fang



********** Basic Setup **********

* clears memory of all data

clear

* close the log file if any already open

capture log close

* turn the -more- off if you want STATA to report stuff continuously

set more off

* set our working directory

cd "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/CFPS2010"

* open a log file

log using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2_Chongyu_Fang.log", text replace

* load in data

use "2010adult_English.dta", clear

* generate birth year (adjusted) and month variables

gen yob = qa1y_best

summarize qa1y_best // there are some useless negative values

replace yob = . if qa1y_best < 0 // nullify these negative values

gen mob = qa1m

summarize qa1m // there are some useless negative values

replace mob = . if qa1m < 0 // nullify these negative values

* divide the birth month into two groups

gen half = .

replace half = 1 if mob >= 1 & mob < 9

replace half = 2 if mob >= 9 & mob <= 12

* let STATA know that birth cohorts are time series data

gen birth_half = yh(yob, half)

format birth_half %th

* compute total education years

gen eduyears = cfps2010eduy_best

summarize cfps2010eduy_best

replace eduyears = . if cfps2010eduy_best < 0 // nullify these negative values

* graph the relationship between education years and birth cohort

by birth_half, sort: egen eduyears_mean = mean(eduyears)

preserve

keep if yob >= 1965 & yob <= 1985

twoway (connected eduyears_mean birth_half), yscale(range(0 12)) title("Total Education Years and Birth Cohorts")

* save graph to my PS2 LaTeX directory

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/TeX codes/part2step3.pdf", as(pdf) replace

restore

* consider some proxies for health, both physical and mental

* physical: self-perceived health outcome, whether hospitalized last year

gen selfhealth = . // self-perceived health outcome

replace selfhealth = 1 if qp3 == 5 // extremely not healthy

replace selfhealth = 2 if qp3 == 4

replace selfhealth = 3 if qp3 == 3

replace selfhealth = 4 if qp3 == 2

replace selfhealth = 5 if qp3 == 1 // very healthy

gen hospital = qp6 // whether hospitalized last year

summarize hospital // 0 for No, 1 for Yes

replace hospital= . if qp6 < 0

* mental: depression, hard to stay calm

gen depressed = .

replace depressed = 1 if qq601 == 5 // never feel depressed

replace depressed = 2 if qq601 == 4

replace depressed = 3 if qq601 == 3

replace depressed = 4 if qq601 == 2

replace depressed = 5 if qq601 == 1 // everyday feel depressed

gen calm = .

replace calm = 1 if qq603 == 5 // never feel hard to stay calm

replace calm = 2 if qq603 == 4

replace calm = 3 if qq603 == 3

replace calm = 4 if qq603 == 2

replace calm = 5 if qq603 == 1 // everyday feel hard to stay calm

* investigate the effects of education on physical and mental health

tab half, gen(halfdummy) // generate dummy first

xi: ivreg2 selfhealth (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 hospital (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985, cluster(yob) partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 depressed (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 calm (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

* validity check

* relevance check results can be found in above codes: first stage regression results produced by ivreg2

* check exclusion now

xi: reg selfhealth halfdummy2 eduyears i.yob if yob >= 1965 & yob < 1985, cluster(yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: reg hospital halfdummy2 eduyears i.yob if yob >= 1965 & yob < 1985, cluster(yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: reg depressed halfdummy2 eduyears i.yob if yob >= 1965 & yob < 1985, cluster(yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: reg calm halfdummy2 eduyears i.yob if yob >= 1965 & yob < 1985, cluster(yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

* heterogeneous effects across Hukou status and gender

* Hukou: let's use the Hukou status at age 12 because that is school age

gen urbanhukou = . // include no Hukou or not applicable

replace urbanhukou = 1 if qa402 == 3 // urban

replace urbanhukou = 0 if qa402 == 1 // agricultural

* gender

gen male = 0

replace male = 1 if gender == 1

replace male = . if gender < 0

* heterogeneous effects across Hukou

xi: ivreg2 selfhealth (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 selfhealth (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 hospital (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 1, cluster(yob) partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 hospital (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 0, cluster(yob) partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 depressed (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 depressed (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 calm (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 calm (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & urbanhukou == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

* hheterogenous effects across gender

xi: ivreg2 selfhealth (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 selfhealth (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 hospital (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 1, cluster(yob) partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 hospital (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 0, cluster(yob) partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 depressed (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 depressed (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 calm (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 1, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

xi: ivreg2 calm (eduyears = halfdummy2) i.yob if yob >= 1965 & yob <= 1985 & male == 0, cluster(yob) first partial(i.yob)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 2/PS2results.xls", excel dec(3) drop(_I* o._I*)

* Finished! Close the log file

cap log close


















