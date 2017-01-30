capture log close
log using "king_assignment3.log", replace 

clear

//LPO 9951: Assignment 3
//Robb King
//Date: January 29, 2017

use "http://www.ats.ucla.edu/stat/stata/seminars/svy_stata_intro/apipop.dta"

/*1. Create a series of macros, one for each of the following characteristics 
of schools: parents, teachers and student.*/

local parents not_hsg hsg some_col col_grad grad_sch avg_ed

local teachers acs_k3 acs_46 acs_core full emer

local students meals ell enroll api_stu

/*2. Create a table of descriptive statistics for these variables using the 
esttab command. Make sure it is clearly labeled.*/

label variable not_hsg "Parent not a high school graduate" 
label variable hsg "Parent is a high school graduate"
label variable some_col "Parent has some college"
label variable col_grad "Parent is a college graduate"
label variable grad_sch "Parent did graduate school"
label variable avg_ed "Parent average level of education"
label variable acs_k3 "Average class size K-3"
label variable acs_46 "Average class size 4-6"
label variable acs_core "Average class size core"
label variable full "Percent full credential"
label variable emer "Percent emer credential"
label variable meals "Percent of students receiving free meals"
label variable ell "Percent of students labeled ELL"
label variable enroll "Total number of students enrolled"
label variable api_stu "Test score on API 2000"

forvalue stype = 1/3{ 
eststo descriptives: estpost tabstat `parents' `teachers' `students', ///
	statistics(mean sd) ///
	columns(statistics) ///
	listwise
	
esttab descriptives using esttab_means_`stype'.rtf, ///
	main(mean) ///
	aux(sd) ///
	nostar ///
	nonote ///
	label ///
	nonumber ///
	replace


/*3. Generate a table of conditional means of api00 as a function of other 
interesting independent variables.*/

sum api00, detail

gen api00_pct_`stype'=api00<=r(p25)

replace api00_pct_`stype'=2 if api00>r(p25) & api00<=r(p75)

replace api00_pct_`stype'=3 if api00>r(p75)

replace api00_pct_`stype'=. if api00==.

label define scores_`stype' 1 "Low API Score" 2 "Medium API Score" 3 "High API Score"

label values api00_pct_`stype' scores_`stype'

eststo conditional_me: estpost tabstat api00 `parents' `teachers' `students', ///
	by(api00_pct_`stype') ///
	statistics(mean sd) ///
	columns(statistics) ///
	listwise

esttab conditional_me using esttab_means_scores_`stype'.rtf, ///
	main(mean) ///
	aux(sd) ///
	nostar ///
	nonote ///
	label ///
	unstack ///
	nonumbers ///
	nomtitles ///
	collabels(none) ///
	replace
}

/*4. Create a scatterplot or scatterplots that display some of the key 
relationships in the data. Clearly label this scatterplot.*/

local ranvar hsg some_col col_grad grad_sch 

forvalue stype = 1/3 {

foreach var of local ranvar {
	scatter api00 `var'
	graph save "api00_parents_`var'_`stype'", replace
	}
}

/*5. Run a series of regressions, one for each “set” of characteristics and
 one fully specified model, with api00 as the dependent variable.*/
 
forvalue stype = 1/3 {
 quietly reg api00 `parents'
estimates store parents_model_`stype', title ("Model 1 `stype'")

quietly reg api00 `parents' `teachers'
estimates store par_tea_model_`stype', title ("Model 2 `stype'")

quietly reg api00 `parents' `teachers' `students'
estimates store combine_model_`stype', title ("Model 3 `stype'")

/*6. Report the results of your regression in a beautifully formatted table.*/

#delimit;

esttab *_model_`stype' using api00_models_`stype'.rtf,
	label
	nodepvars
	nostar
	b(2)
	se(2)
	r2 (2)
	ar2(2)
	scalar(F "df_m DF model" "df_r DF residual" N)
	sfmt(2 0 0 0)
	replace
	;
	
#delimit cr
}

/*7. Create a graphic the shows the impact of the various independent 
variables on the outcome variable. Clearly label and describe this graphic.*/

forvalues stype = 1/3 {
estimates restore parents_model_`stype'

plotbeta not_hsg|hsg|some_col|col_grad|grad_sch|avg_ed, labels xtitle(Parameters) ///
	title("Model w/ No Covariates") subtitle("From OLS regression. Dep Var= Test Scores") 

graph save "parents_model_`stype'.gph", replace

estimates restore par_tea_model_`stype'

plotbeta not_hsg|hsg|some_col|col_grad|grad_sch|avg_ed|acs_k3|acs_46|acs_core|full|emer, ///
	labels xtitle(Parameters) title("Model w/ Teachers") subtitle("From OLS regression. Dep Var= Test Scores") 

graph save "par_tea_model_`stype'.gph", replace

estimates restore combine_model_`stype'

plotbeta not_hsg|hsg|some_col|col_grad|grad_sch|avg_ed|acs_k3|acs_46|acs_core|full|emer|meals|ell|enroll|api_stu, ///
	labels xtitle(Parameters) title("Full Model") subtitle("From OLS regression. Dep Var= Test Scores") 

graph save "combine_model_`stype'.gph", replace

graph combine "parents_model_`stype'.gph" "par_tea_model_`stype'.gph" "combine_model_`stype'.gph", ///
	cols(2) ///
	rows(2)

graph save "all_models_`stype'.gph", replace
}

exit



