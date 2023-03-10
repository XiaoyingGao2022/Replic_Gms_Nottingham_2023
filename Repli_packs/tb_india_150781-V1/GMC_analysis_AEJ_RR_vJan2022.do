****************************************************
**** DO-FILE FOR ***********************************
**** GOLDBERG-MACIS-CHINTAGUNTA ********************
**** INCENTIVIZED PEER REFERRALS FOR TB SCREENING **
****************************************************

clear all
#delimit ;
set mem 500m;
set matsize 10000;
set maxvar 10000;


* -------------------------------------------------------------------- ;
* Set global macros for file locations;
* -------------------------------------------------------------------- ;

global d=1;

if $d == 1 {;
	global raw "/Users/Jess/Dropbox/India/OpASHA/18. Experiment Data/All Phases Cleaned Data/Cleaned datasets/";
	global created "/Users/Jess/Dropbox/India/OpASHA/18. Experiment Data/2. created data/may2019/";
	global output "/Users/Jess/Dropbox/India/OpASHA/18. Experiment Data/3. output/aej-rr/";
};

if $d == 2 {;
	global raw "/Users/mmacis1/Dropbox/OpASHA/18. Experiment Data/All Phases Cleaned Data/Cleaned datasets/";
	global created "/Users/mmacis1/Dropbox/OpASHA/18. Experiment Data/2. created data/may2019/";
	global output "/Users/mmacis1/Dropbox/OpASHA/18. Experiment Data/3. output/aej-rr/";
};

	
if $d == 3 {;

	global raw "C:\Users/Anusuya Sivaram/Dropbox/OpASHA Anu Replication/Data/raw/";
	global created "C:\Users\Anusuya Sivaram/Dropbox/OpASHA Anu Replication/Data/created/";
	global output "C:\Users\Anusuya Sivaram/Dropbox/OpASHA Anu Replication/Output/";

};


*------------------------------------------------------------------;
* Install user-written packages;
*------------------------------------------------------------------;

cap ssc install boottest;
cap ssc install orth_out;
cap ssc install putexcel;
cap ssc install estout;
cap ssc install smileplot;
cap ssc install qqvalue; 
cap ssc install winsor;
cap ssc install ritest;
cap ssc install mdesc; 
cap ssc install outreg2;


/*;

*************************************************************************************;
** Removing/pseudonimyzing possibly identifying center- and patient-level information;
*************************************************************************************;

/* 1. MAIN DATASET */;

use "${created}phase2to6_patient_level_cleaned.dta", clear ;

* -------------------------------------------------------------------- ;

* Remove center names;
drop centre_name;

* Remove label from id_centre;
label drop id_centre;

* Remove/pseudonimyze possibly identifying information ;
drop bl_key el_key el_instanceid el_instancename;
drop _meAdmin _meReferral _meRefpatient;
rename bl_native_home_loc x_bl_native_home_loc;
encode x_bl_native_home_loc, gen(bl_native_home_loc);
drop x_bl_native_home_loc;
* -------------------------------------------------------------------- ;

save "${created}TB_data.dta", replace ;

clear;

/* 2. AUXILIARY DATASETS */;

use "${created}new_patient_data.dta";
keep id_state id_city id_centre resp_* ptype asset_index;
label drop id_centre;
save "${created}new_patient_data_toappend.dta", replace;

clear;
use "${created}new_patient_admin_merged.dta";
label drop id_centre;
keep id_state id_city id_centre resp_*  asset_index t_incentive t_outreach_peer t0 referral insamp;
save "${created}new_patient_data_merged.dta", replace;

clear;

*************************************************************************************;

*/;

#delimit ;
cap log close;
log using "${output}GMC_analysis_AEJ-RR.log", replace;

use "${created}TB_data.dta", clear ;

* -------------------------------------------------------------------- ;
*Label treatment indicators;
* -------------------------------------------------------------------- ;

label var t_outreach_peer "Peer outreach";
label var t_outreach_identified "Identified contact tracing";
label var t_outreach_anonymous "Anonymous contact tracing";
label var t_incentive_encouragement "Encouragement";
label var t_incentive_unconditional "Unconditional incentive";
label var t_incentive_conditional "Conditional incentive";

gen outreach=.;
replace outreach=0 if t0==1;
replace outreach=1 if t_outreach_peer==1;
replace outreach=2 if t_outreach_identified==1;
replace outreach=3 if t_outreach_anonymous==1;
label define outreachx 0 "control" 1 "peer outreach" 2 "health worker outreach, identified" 3 "health worker outreach, anonymous";
label values outreach outreachx;

gen incentive=.;
replace incentive=0 if t0==1;
replace incentive=1 if t_incentive_encouragement==1;
replace incentive=2 if t_incentive_conditional==1;
replace incentive=3 if t_incentive_unconditional==1;

label define incentivex 0 "control" 1 "encouragement" 2 "incentive, conditional" 3 "incentive, unconditional";
label values incentive incentivex;

* -------------------------------------------------------------------- ;
*Create pooled treatments indicators;
* -------------------------------------------------------------------- ;

gen t_outreach_any=0;
replace t_outreach_any=1 if t_outreach_peer==1 | t_outreach_identified==1 | t_outreach_anonymous==1;
gen t_incentive_any=0;
replace t_incentive_any=1 if t_incentive_encouragement==1 | t_incentive_unconditional==1 | t_incentive_conditional==1;

label var t_outreach_any "Any type of outreach";
label var t_incentive_any "Any type of incentive";

gen t_peer_encouragement=(t_outreach_peer==1 & t_incentive_encouragement==1);
gen t_ctr_encouragement=(t_outreach_peer==0 & t_incentive_encouragement==1);
gen t_incentive_peer=(t_outreach_peer==1 & (t_incentive_unconditional==1|t_incentive_conditional==1));
gen t_incentive_ctr=((t_outreach_identified==1|t_outreach_anonymous==1) & (t_incentive_unconditional==1|t_incentive_conditional==1));

label var t_peer_encouragement "Peer outreach, no financial incentive";
label var t_ctr_encouragement "Contact tracing, no financial incentive";
label var t_incentive_peer "Peer outreach, financial incentive";
label var t_incentive_ctr "Contact tracing, financial incentive";

gen t_outreach_CT=0;
replace t_outreach_CT=1 if t_outreach_identified==1 | t_outreach_anonymous==1;
gen t_incentive_financial=0;
replace t_incentive_financial=1 if t_incentive_unconditional==1 | t_incentive_conditional==1;

label var t_outreach_CT "Health worker outreach";
label var t_incentive_financial "Financial incentive";

* -------------------------------------------------------------------- ;
*Clean and label baseline controls;
* -------------------------------------------------------------------- ;

*winsorize asset index;
winsor bl_asset_index, gen(wbl_asset) p(0.01); 
drop bl_asset_index;
ren wbl_asset bl_asset_index;
label var bl_asset_index "Asset index";

egen med_asset=median(bl_asset_index);
gen dbl_asset=.;
replace dbl_asset=1 if bl_asset_index>=med_asset & bl_asset_index!=.;
replace dbl_asset=0 if bl_asset_index<med_asset & bl_asset_index!=.;
tab dbl_asset, sum(bl_asset_index);
drop med_asset;

egen med_contacts=median(bl_resp_ppltalk);
gen dbl_contacts=.;
replace dbl_contacts=1 if bl_resp_ppltalk>=med_contacts & bl_resp_ppltalk!=.;
replace dbl_contacts=0 if bl_resp_ppltalk<med_contacts & bl_resp_ppltalk!=.;
tab dbl_contacts, sum(bl_resp_ppltalk);
drop med_contacts;
label var bl_resp_ppltalk "Number of social contacts";

recode bl_diff_symptoms_test (1 2 3 = 1) (4 5 6 7 = 0) (. .o = .), gen(bl_nodelay);
label var bl_nodelay "Tested within 1 month of symptoms" ;

recode bl_treat2survey (0/2 = 1) (3/24 = 0) (25/200 = .), gen (bl_treatIP);
recode bl_treat2survey (0/2 = 0) (3/24 = 1) (25/200 = .), gen (bl_treatCP);
label var bl_treatIP "Intensive phase (0-2 months since treatment started)";
label var bl_treatCP "Continuation phase (3-24 months since treatment started)";

global bl_health "bl_past_tb_treated bl_nodelay";

label var bl_past_tb_treated "Previously treated for TB";

gen dbl_lit=bl_resp_somelit;

* -------------------------------------------------------------------- ;
*Macros for varlists;
* -------------------------------------------------------------------- ;
global bl_char     "bl_resp_female bl_resp_hindu bl_resp_muslim bl_resp_somelit bl_resp_secondedu bl_asset_index bl_resp_havebank bl_resp_ppltalk" ;
global bl_hetvars "dbl_asset dbl_contacts bl_nodelay bl_resp_female bl_treatIP";
global ref_num1 "a_num_ref_visit";
global ref_num2 "a_num_rec_test admin_num_tested a_num_pos";



*********************************;

*********** ANALYSES ************;

*********************************;



*********************************;

** INDIVIDUAL-LEVEL REGRESSIONS**;

*********************************;


* -------------------------------------------------------------------- ;
*Appendix Table B1: attrition;
* -------------------------------------------------------------------- ;
gen surveyed=(bl_survey_status_cleaned==1);
tab surveyed phase, col;
xi: reg surveyed t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);
outreg2 using "${output}/AppTableB1.xls", symbol(***,**,*) bdec(3) sdec(3) replace;
xi: reg surveyed t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city, cluster(id_centre);
outreg2 using "${output}/AppTableB1.xls", symbol(***,**,*) bdec(3) sdec(3) append;


* -------------------------------------------------------------------- ;
* Save working data set;
* -------------------------------------------------------------------- ;
save "${created}TB_analysis_data.dta", replace /*dataset used below for center-level analyses */;

* -------------------------------------------------------------------- ;
*Keep only CPs who were surveyed;
* -------------------------------------------------------------------- ;
keep if bl_survey_status_cleaned==1;

* -------------------------------------------------------------------- ;
*Drop Bhubaneshwar centers;
* -------------------------------------------------------------------- ;
/* Because a change in Operation ASHA's relationship with the leadership at 
the government testing center in Bhubaneshwar (Odisha), we were not
permitted to access administrative endline data for these patients*/;
drop if id_city==10;


* -------------------------------------------------------------------- ;
* Table 1: experimental design and sample sizes;
* -------------------------------------------------------------------- ;
tab treatment_group;
egen tag=tag(treatment_group id_centre);
egen distinct=total(tag), by(treatment_group);
tab treatment_group, sum(distinct);

* -------------------------------------------------------------------- ;
* Tables 2 and 3: effect of incentives and outreach on TB screening and detection;
* -------------------------------------------------------------------- ;

estimates clear;

*create weights (for appendix tables);
bysort id_centre: egen ncenter=count(id_centre);
local N=_N;
gen w=ncenter/`N';
sum w;
table id_centre, c(mean ncenter mean w);


foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);
	est store bI_`var';
	matrix MI=e(V);
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	test t_incentive_unconditional t_incentive_conditional;
	estadd scalar F_incentive= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_incentive_encouragement = t_incentive_unconditional;
	estadd scalar P_encouragement_unconditional=r(p);
	estadd scalar B_encouragement_unconditional=_b[t_incentive_unconditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_unconditional=sqrt(MI[2,2]+MI[1,1]-2*MI[2,1]);
	test t_incentive_encouragement=t_incentive_conditional;
	estadd scalar P_encouragement_conditional=r(p);
	estadd scalar B_encouragement_conditional=_b[t_incentive_conditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_conditional=sqrt(MI[3,3]+MI[1,1]-2*MI[3,1]);
	test t_incentive_conditional=t_incentive_unconditional;
	estadd scalar P_incentives=r(p);
	estadd scalar B_conditional_unconditional=_b[t_incentive_conditional]-_b[t_incentive_unconditional];
	estadd scalar SE_conditional_unconditional=sqrt(MI[3,3]+MI[2,2]-2*MI[3,2]);
	matrix drop MI;
	
	xi: reg `var' t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city, cluster(id_centre);
	est store bO_`var';
	matrix MO=e(V);
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	estadd scalar F_outreach= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_outreach_peer=t_outreach_identified;
	estadd scalar P_peer_ident=r(p);
	estadd scalar B_peer_identified=_b[t_outreach_peer]-_b[t_outreach_identified];
	estadd scalar SE_peer_identified=sqrt(MO[2,2]+MO[1,1]-2*MO[2,1]);
	test t_outreach_peer=t_outreach_anonymous;
	estadd scalar P_peer_anon=r(p);
	estadd scalar B_peer_anon=_b[t_outreach_peer]-_b[t_outreach_anonymous];
	estadd scalar SE_peer_anon=sqrt(MO[3,3]+MO[1,1]-2*MO[3,1]);
	test t_outreach_identified=t_outreach_anonymous;
	estadd scalar P_tracing=r(p);
	estadd scalar B_anon_identified=_b[t_outreach_anonymous]-_b[t_outreach_identified];
	estadd scalar SE_anon_identified=sqrt(MO[2,2]+MO[3,3]-2*MO[3,2]);
	matrix drop MO;

	};
	
	
estout bI_* using "${output}Table2.tex", replace
	style(tex)
	keep(t_incentive_encouragement t_incentive_unconditional t_incentive_conditional)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_incentive B_encouragement_unconditional SE_encouragement_unconditional B_encouragement_conditional SE_encouragement_conditional B_conditional_unconditional SE_conditional_unconditional, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Encouragement-Unconditional" " " "Encouragement-Conditional" " " "Conditional-Unconditional" " " )) legend
	;
	
estout bO_* using "${output}Table3.tex", replace
	style(tex)
	keep(t_outreach_peer t_outreach_identified t_outreach_anonymous)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach B_peer_identified SE_peer_identified B_peer_anon SE_peer_anon B_anon_identified SE_anon_identified, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Peer-Identified" " " "Peer-Anonymous" " " "Anonymous-Identified" " ")) legend
	;

estimates clear;


*Randomization-inference p-values for Tables 2 and 3;
*USING RITEST (SIMON HESS) https://blogs.worldbank.org/impactevaluations/finally-way-do-easy-randomization-inference-stata;
cap drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_incentive_encouragement==1;
replace treatgr=2 if t_incentive_unconditional==1;
replace treatgr=3 if t_incentive_conditional==1;

foreach var in $ref_num1 $ref_num2 {;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(2 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* unconditional vs control*/;
ritest treatgr (_b[0.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 2): reg `var' i.treatgr i.id_city, cluster(id_centre) /* conditional vs control*/;
ritest treatgr (_b[1.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* encouragement vs unconditional*/;
ritest treatgr (_b[1.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 2): reg `var' i.treatgr i.id_city, cluster(id_centre) /* encouragement vs conditional*/;
ritest treatgr (_b[2.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 1): reg `var' i.treatgr i.id_city, cluster(id_centre) /* unconditional vs conditional*/;
};

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_outreach_peer==1;
replace treatgr=2 if t_outreach_identified==1;
replace treatgr=3 if t_outreach_anonymous==1;
foreach var in $ref_num1 $ref_num2 {;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(2 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* identified vs control*/;
ritest treatgr (_b[0.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 2): reg `var' i.treatgr i.id_city, cluster(id_centre) /* anonymous vs control*/;
ritest treatgr (_b[1.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer vs identified*/;
ritest treatgr (_b[1.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 2): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer vs anonymous*/;
ritest treatgr (_b[2.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 1): reg `var' i.treatgr i.id_city, cluster(id_centre) /* identified vs anonymous*/;
};



* -------------------------------------------------------------------- ;
* Table 4 : Complementarities between peer outreach and financial incentives;
* -------------------------------------------------------------------- ;

estimates clear;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var'  t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city, cluster(id_centre);
	est store bZ_`var';
	test t_peer_encouragement=t_incentive_peer;
	estadd scalar p_peer=r(p);
	test t_ctr_encouragement=t_incentive_ctr;
	estadd scalar p_ct=r(p);
	test t_peer_encouragement=t_ctr_encouragement;
	estadd scalar p_enc=r(p);
	test t_incentive_peer = t_incentive_ctr;
	estadd scalar p_inc=r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	};
	
estout bZ_* using "${output}Table4.tex.tex", replace
	style(tex)
	keep(t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar p_peer p_ct p_enc p_inc, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: Peer encouragement = Peer incentives" 
	"P-value: Contact tracing encouragement = Contact tracing incentives" "P-value: Contact tracing encouragement = Peer encouragement" "P-value: Contact tracing incentives = Peer incentives")) legend
	;

estimates clear;

*Randomization-inference p-values;
*USING RITEST (SIMON HESS) https://blogs.worldbank.org/impactevaluations/finally-way-do-easy-randomization-inference-stata;
#delimit;
drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_peer_encouragement==1;
replace treatgr=2 if t_ctr_encouragement==1;
replace treatgr=3 if t_incentive_peer==1;
replace treatgr=4 if t_incentive_ctr==1;

foreach var in $ref_num1 $ref_num2 {;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(2 3 4): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 3 4): reg `var' i.treatgr i.id_city, cluster(id_centre) /* health worker encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 2 4): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer incentivized vs control*/;
ritest treatgr (_b[0.treatgr]-_b[4.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(1 2 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* health worker incentivized vs control*/;

ritest treatgr (_b[1.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 2 4): reg `var' i.treatgr i.id_city, cluster(id_centre) /* peer encouragement vs peer incentive*/;
ritest treatgr (_b[2.treatgr]-_b[4.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 1 3): reg `var' i.treatgr i.id_city, cluster(id_centre) /* hw encouragement vs hw incentive*/;
ritest treatgr (_b[1.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 3 4): reg `var' i.treatgr i.id_city, cluster(id_centre) /* hw encouragement vs peer encouragement*/;
ritest treatgr (_b[3.treatgr]-_b[4.treatgr]), reps(2000) seed(999) strata(id_city) cluster(id_centre) fixlevels(0 1 2): reg `var' i.treatgr i.id_city, cluster(id_centre) /* hw incentive vs peer incentive*/;

};



* -------------------------------------------------------------------- ;
*Compute p-values corrected for multiple-hypothesis testing            ;
*For tables 2, 3 and 4            ;
* -------------------------------------------------------------------- ;

preserve;

unab y : $ref_num1 $ref_num2;
local o : word count `y';
local l=`o'*3;
local m=`o'*4; ***for interaction specification;

matrix pincentives = J(`l',1,.);
matrix poutreach = J(`l',1,.);
matrix pinteract = J(`m',1,.);

foreach var in enc uncon cond {;
	matrix rownames pincentives=`y'_`var';
	};
	
foreach var in peer ident anon {;
	matrix rownames poutreach=`y'_`var';
	};
	
foreach var in peer_enc CT_enc peer_inc CT_inc {;
	matrix rownames pinteract=`y'_`var';
	};

matrix colnames pincentives="p";
matrix colnames poutreach="p";
matrix colnames pinteract="p";

matlist pincentives;
matlist poutreach;
matlist pinteract;


* Incentives (Table 2) and Outreach (Table 3);

estimates clear;
local i=0;
local j=0;
foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);
	est store bI_`var';
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	estadd scalar F_incentive= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_incentive_encouragement = t_incentive_unconditional;
	estadd scalar P_encouragement_unconditional=r(p);
	test t_incentive_encouragement=t_incentive_conditional;
	estadd scalar P_encouragement_conditional=r(p);
	test t_incentive_conditional=t_incentive_unconditional;
	estadd scalar P_incentives=r(p);
	foreach tvar in t_incentive_encouragement t_incentive_unconditional t_incentive_conditional {;
		local i =`i'+1;
		matrix pincentives[`i',1]=2*ttail(e(df_r), abs(_b[`tvar']/_se[`tvar']));
		};
	
	xi: reg `var' t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city, cluster(id_centre);
	est store bO_`var';
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	estadd scalar F_outreach= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_outreach_peer=t_outreach_identified;
	estadd scalar P_peer_ident=r(p);
	test t_outreach_peer=t_outreach_anonymous;
	estadd scalar P_peer_anon=r(p);
	test t_outreach_identified=t_outreach_anonymous;
	estadd scalar P_tracing=r(p);
	foreach tvar in t_outreach_peer t_outreach_identified t_outreach_anonymous {;
		local j =`j'+1;
		matrix poutreach[`j',1]=2*ttail(e(df_r), abs(_b[`tvar']/_se[`tvar']));
		};
	
	};


estout bI_* using "${output}CPlevel_incentive_uncorrectedP.tex", replace
	style(tex)
	keep(t_incentive_encouragement t_incentive_unconditional t_incentive_conditional)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(5)) p  )  starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_incentive P_encouragement_conditional P_encouragement_unconditional P_incentives, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"P-value: Encouragement = Conditional" "P-value: Encouragement = Unconditional" "P-value: Conditional = Unconditional")) legend
	;
	
estout bO_* using "${output}CPlevel_outreach_uncorrectedP.tex", replace
	style(tex)
	keep(t_outreach_peer t_outreach_identified t_outreach_anonymous)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(5)) p) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach P_peer_ident P_peer_anon P_tracing, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"P-value: Peer = Identified" "P-value: Peer = Anonymous" "P-value: Identified = Anonymous")) legend
	;
	
matlist pincentives;
matlist poutreach;

* Interactions (Table 4);

local i=0;
foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var'  t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city, cluster(id_centre);
	est store bZ_`var';
	test t_peer_encouragement=t_incentive_peer;
	estadd scalar p_peer=r(p);
	test t_ctr_encouragement=t_incentive_ctr;
	estadd scalar p_ct=r(p);
	test t_peer_encouragement=t_ctr_encouragement;
	estadd scalar p_enc=r(p);
	test t_incentive_peer = t_incentive_ctr;
	estadd scalar p_inc=r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	foreach ttvar in t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr {;
		local i =`i'+1;
		matrix pinteract[`i',1]=2*ttail(e(df_r), abs(_b[`ttvar']/_se[`ttvar']));
		};
	};


estout bZ_* using "${output}CPlevel_PeerxIncentive_uncorrectedP.tex", replace
	style(tex)
	keep(t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(5)) p) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar p_peer p_ct p_enc p_inc, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: Peer encouragement = Peer incentives" 
	"P-value: Contact tracing encouragement = Contact tracing incentives" "P-value: Contact tracing encouragement = Peer encouragement" "P-value: Contact tracing incentives = Peer incentives")) legend
	;

capture file close fp;
file open fp using "${output}pincentives.csv", write replace;
file write fp "p" _n;
forval i=1/`l' {;
	di(pincentives[`i',1]);
	local print=pincentives[`i',1];
	file write fp "`print'" _n;
	};
	
capture file close fp;

capture file close fp1;
file open fp1 using "${output}poutreach.csv", write replace;
file write fp1 "p" _n;
forval i=1/`l' {;
	di(poutreach[`i',1]);
	local print=poutreach[`i',1];
	file write fp1 "`print'" _n;
	};

capture file close fp1;	

capture file close fp2;
file open fp2 using "${output}pinteract.csv", write replace;
file write fp2 "p" _n;
forval i=1/`m' {;
	di(pinteract[`i',1]);
	local print=pinteract[`i',1];
	file write fp2 "`print'" _n;
	};

capture file close fp2;	

* q values for multiple hypothesis corrections;
	
estimates clear;

clear;
insheet using "${output}pincentives.csv", clear;
***calculate using Benjamini and Hochberg;
sort p;
local m=_N;
gen qhoch=p*(`m'-_n+1);
gen qsimes=p*`m'/_n;
list;
drop q*;
*** built in;
multproc, method(hochberg) puncor(.05);
qqvalue p, method(hochberg) qvalue(qvals_incentivesH);
list;

estimates clear;

clear;
insheet using "${output}poutreach.csv", clear;
multproc, method(hochberg);
qqvalue p, method(hochberg) qvalue(qvals_outreachH);
list;

clear;
insheet using "${output}pinteract.csv", clear;
multproc, method(hochberg);
qqvalue p, method(hochberg) qvalue(qvals_interactH);
list;

restore;


* -------------------------------------------------------------------- ;
*Appendix Table B3;
* -------------------------------------------------------------------- ;
sum $bl_char $bl_health;
sum $bl_char $bl_health if treatment_group==0;
sum $bl_char $bl_health if t_incentive_encouragement==1;
sum $bl_char $bl_health if t_incentive_conditional==1;
sum $bl_char $bl_health if t_incentive_unconditional==1;


* -------------------------------------------------------------------- ;
*Appendix Table B4;
* -------------------------------------------------------------------- ;
sum $bl_char $bl_health;
sum $bl_char $bl_health if treatment_group==0;
sum $bl_char $bl_health if t_outreach_peer==1;
sum $bl_char $bl_health if t_outreach_identified==1;
sum $bl_char $bl_health if t_outreach_anonymous==1;



* -------------------------------------------------------------------- ;
* Appendix Table B5: F-stats (and p-values) for all pairwise comparisons;
* -------------------------------------------------------------------- ;

* T/C only (No table, reported in the text in section 3.2);
xi: reg t_incentive_any $bl_char $bl_health i.id_city if a_num_ref_visit!=.;
test $bl_char $bl_health;


*** each outcome vs. control;

foreach var in t_incentive_encouragement t_incentive_unconditional t_incentive_conditional t_outreach_peer t_outreach_identified t_outreach_anonymous {;
	reg `var' $bl_char $bl_health i.id_city if (phase2==1 | phase3==1 | phase4==1 | phase5==1 | phase6==1) & a_num_ref_visit!=. & (`var'==1 | t0==1);
	test $bl_char $bl_health;
	local p_`var'_c=r(p);
	};
	
*** incentives vs. encouragement;

foreach var in t_incentive_unconditional t_incentive_conditional {;
	reg `var' $bl_char $bl_health i.id_city if (phase2==1 | phase3==1 | phase4==1 | phase5==1 | phase6==1) & a_num_ref_visit!=. & (`var'==1 | t_incentive_encouragement==1);
	test $bl_char $bl_health;
	local p_`var'_enc=r(p);
	};	
	
*** conditional vs. unconditional;

reg t_incentive_conditional $bl_char $bl_health i.id_city if (phase2==1 | phase3==1 | phase4==1 | phase5==1 | phase6==1) & a_num_ref_visit!=. & (t_incentive_conditional==1 | t_incentive_unconditional==1);
test $bl_char $bl_health;
local p_conditional_un=r(p);

*** peer vs. contact tracing;

foreach var in t_outreach_identified t_outreach_anonymous {;
	reg `var' $bl_char $bl_health i.id_city if (phase2==1 | phase3==1 | phase4==1 | phase5==1 | phase6==1) & a_num_ref_visit!=. & (`var'==1 | t_outreach_peer==1);
	test $bl_char $bl_health;
	local p_`var'_peer=r(p);
	};	
	
*** identified vs. anon;

reg t_outreach_identified $bl_char $bl_health i.id_city if (phase2==1 | phase3==1 | phase4==1 | phase5==1 | phase6==1) & a_num_ref_visit!=. & (t_outreach_identified==1 | t_outreach_anonymous==1);
test $bl_char $bl_health;
local p_ident_anon=r(p);

capture file close fp;
file open fp using "${output}pvals.csv", write replace;
file write fp "p" _n;

foreach var in t_incentive_encouragement t_incentive_unconditional t_incentive_conditional t_outreach_peer t_outreach_identified t_outreach_anonymous {;
	di("`var' against control `p_`var'_c'");
	file write fp "`p_`var'_c'" _n;
	};

foreach var in t_incentive_unconditional t_incentive_conditional {;
	di("`var' against encouragement `p_`var'_enc'");
	file write fp "`p_`var'_enc'" _n;
	};
	
di("conditional against unconditional `p_conditional_un'");
file write fp "`p_conditional_un'" _n;

foreach var in t_outreach_identified t_outreach_anonymous {;
	di("`var' against peer `p_`var'_peer'");
	file write fp "`p_`var'_peer'" _n;
	};
	
di("identified against anon `p_ident_anon'");
file write fp "`p_ident_anon'" _n;

capture file close fp;


* -------------------------------------------------------------------- ;
* Appendix Table B6: Number of names given by CPs;
* -------------------------------------------------------------------- ;
*Peer outreach, without and with incentives;
tab bl_num_ref_named if t_peer_encouragement==1;
tab bl_num_ref_named if t_incentive_peer==1;
*Health worker outreach, without and with incentives;
tab bl_num_ref_named if t_ctr_encouragement==1, m;
tab bl_num_ref_named if t_incentive_ctr==1, m;


* -------------------------------------------------------------------- ;
* Appendix Table B7: Number of returned cards (peer outreach);
* -------------------------------------------------------------------- ;
gen cards_bought=el_bybck_amnt/10;
replace cards_bought=10 if cards_bought==11;

*Panel A;
xi: reg cards_bought t_incentive_unconditional t_incentive_conditional i.id_city if t_outreach_peer==1 & cards_bought ~=., cluster(id_centre);
outreg2 using "${output}/AppTableB7.xls", symbol(***,**,*) bdec(3) sdec(3) slow(2500) replace;
sum cards_bought if e(sample)==1 & t_peer_encouragement==1;

*Panel B;
tab cards_bought if id_city~=10 & t_peer_encouragement==1 & cards_bought ~=., m;
tab cards_bought if id_city~=10 & (t_incentive_peer==1 & t_incentive_unconditional==1) & cards_bought ~=., m;
tab cards_bought if id_city~=10 & (t_incentive_peer==1 & t_incentive_conditional==1) & cards_bought ~=., m;

*Kolmogorov-Smirnov tests;
gen peergroup=.;
replace peergroup=0 if t_outreach_peer==1;
replace peergroup=1 if t_outreach_peer==1 & (t_incentive_unconditional==1|t_incentive_conditional==1);
tab peergroup;
ksmirnov cards_bought, by(peergroup) /* encouragement vs. any incentive */;
drop peergroup;
gen peergroup=.;
replace peergroup=1 if t_outreach_peer==1 & t_incentive_unconditional==1;
replace peergroup=2 if t_outreach_peer==1 & t_incentive_conditional==1;
tab peergroup;
ksmirnov cards_bought, by(peergroup) /* unconditional vs. conditional incentive */;

*averages;
sum cards_bought if t_peer_encouragement==1 & cards_bought ~=.;
sum cards_bought if (t_incentive_peer==1 & t_incentive_unconditional==1) & cards_bought ~=.;
sum cards_bought if (t_incentive_peer==1 & t_incentive_conditional==1) & cards_bought ~=.;

* -------------------------------------------------------------------- ;
* Relationship between existing patient and new suspects;
* Results described in section 3.3 of the paper;
* -------------------------------------------------------------------- ;

preserve;

forvalues i=1(1)6 {;
gen neighbor`i'=(bl_rel_ref`i'==13);
replace neighbor`i'=. if bl_rel_ref`i'==.;

gen relative`i'=(bl_rel_ref`i'==1|bl_rel_ref`i'==2|bl_rel_ref`i'==3|bl_rel_ref`i'==4|bl_rel_ref`i'==5|bl_rel_ref`i'==6|bl_rel_ref`i'==7|bl_rel_ref`i'==8);
replace relative`i'=. if bl_rel_ref`i'==.;

gen other`i'=(bl_rel_ref`i'==9|bl_rel_ref`i'==10|bl_rel_ref`i'==11|bl_rel_ref`i'==12);
replace other`i'=. if bl_rel_ref`i'==.;

};

#delimit ;

count ;
keep if (bl_rel_ref1~=.|bl_rel_ref2~=.|bl_rel_ref3~=.|bl_rel_ref4~=.|bl_rel_ref5~=.|bl_rel_ref6~=.);

keep resp_id id_centre id_city
treatment_group t_incentive_encouragement t_incentive_unconditional t_incentive_conditional
t_outreach_peer t_outreach_identified t_outreach_anonymous t_outreach_CT
neighbor1-neighbor6
relative1-relative6
other1-other6;

reshape long neighbor relative other, i(resp_id) j(nref);

drop if treatment_group==0;

*all groups;
tab neighbor; 
tab other;
tab relative;

*limited to health worker outreach groups;
tab neighbor if t_outreach_CT==1;
tab other if  t_outreach_CT==1;
tab relative if  t_outreach_CT==1;

*limited to peer outreach groups;
tab neighbor if t_outreach_peer==1;
tab other if t_outreach_peer==1;
tab relative if t_outreach_peer==1;

*regressions to test effect of incentives;
xi: reg relative t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);
xi: reg neighbor t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);
xi: reg other t_incentive_unconditional t_incentive_conditional i.id_city, cluster(id_centre);

restore;

* -------------------------------------------------------------------- ;
*Appendix Table B8: Effect of financial incentives, including baseline covariates;
*Appendix Table B9: Effect of outreach types, including baseline covariates;
*Appendix Table B10: Complementarities between incentives and outreach types, including baseline covariates;
* -------------------------------------------------------------------- ;

estimates clear;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional $bl_char $bl_health i.id_city, cluster(id_centre);
	est store bI_`var';
	matrix MI=e(V);
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	estadd scalar F_incentive= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_incentive_encouragement = t_incentive_unconditional;
	estadd scalar P_encouragement_unconditional=r(p);
	estadd scalar B_encouragement_unconditional=_b[t_incentive_unconditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_unconditional=sqrt(MI[2,2]+MI[1,1]-2*MI[2,1]);
	test t_incentive_encouragement=t_incentive_conditional;
	estadd scalar P_encouragement_conditional=r(p);
	estadd scalar B_encouragement_conditional=_b[t_incentive_conditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_conditional=sqrt(MI[3,3]+MI[1,1]-2*MI[3,1]);
	test t_incentive_conditional=t_incentive_unconditional;
	estadd scalar P_incentives=r(p);
	estadd scalar B_conditional_unconditional=_b[t_incentive_conditional]-_b[t_incentive_unconditional];
	estadd scalar SE_conditional_unconditional=sqrt(MI[3,3]+MI[2,2]-2*MI[3,2]);
	matrix drop MI;
	
	xi: reg `var' t_outreach_peer t_outreach_identified t_outreach_anonymous $bl_char $bl_health i.id_city, cluster(id_centre);
	est store bO_`var';
	matrix MO=e(V);
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	estadd scalar F_outreach= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_outreach_peer=t_outreach_identified;
	estadd scalar P_peer_ident=r(p);
	estadd scalar B_peer_identified=_b[t_outreach_peer]-_b[t_outreach_identified];
	estadd scalar SE_peer_identified=sqrt(MO[2,2]+MO[1,1]-2*MO[2,1]);
	test t_outreach_peer=t_outreach_anonymous;
	estadd scalar P_peer_anon=r(p);
	estadd scalar B_peer_anon=_b[t_outreach_peer]-_b[t_outreach_anonymous];
	estadd scalar SE_peer_anon=sqrt(MO[3,3]+MO[1,1]-2*MO[3,1]);
	test t_outreach_identified=t_outreach_anonymous;
	estadd scalar P_tracing=r(p);
	estadd scalar B_anon_identified=_b[t_outreach_anonymous]-_b[t_outreach_identified];
	estadd scalar SE_anon_identified=sqrt(MO[2,2]+MO[3,3]-2*MO[3,2]);
	matrix drop MO;
	
	};
	
estout bI_* using "${output}AppTableB8.tex", replace
	style(tex)
	keep(t_incentive_encouragement t_incentive_unconditional t_incentive_conditional)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_incentive B_encouragement_unconditional SE_encouragement_unconditional B_encouragement_conditional SE_encouragement_conditional B_conditional_unconditional SE_conditional_unconditional, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Encouragement-Unconditional" " " "Encouragement-Conditional" " " "Conditional-Unconditional" " " )) legend
	;
	

estout bO_* using "${output}AppTableB9.tex", replace
	style(tex)
	keep(t_outreach_peer t_outreach_identified t_outreach_anonymous)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach B_peer_identified SE_peer_identified B_peer_anon SE_peer_anon B_anon_identified SE_anon_identified, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Peer-Identified" " " "Peer-Anonymous" " " "Anonymous-Identified" " ")) legend
	;	

estimates clear;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var'  t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr $bl_char $bl_health  i.id_city, cluster(id_centre);
	est store bY_`var';
	test t_peer_encouragement=t_incentive_peer;
	estadd scalar p_peer=r(p);
	test t_ctr_encouragement=t_incentive_ctr;
	estadd scalar p_ct=r(p);
	test t_peer_encouragement=t_ctr_encouragement;
	estadd scalar p_enc=r(p);
	test t_incentive_peer = t_incentive_ctr;
	estadd scalar p_inc=r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	};
	
estout bY_* using "${output}AppTableB10.tex", replace
	style(tex)
	keep(t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	stats(N r2 mdepvar p_peer p_ct p_enc p_inc, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: Peer encouragement = Peer incentives" 
	"P-value: Contact tracing encouragement = Contact tracing incentives" "P-value: Contact tracing encouragement = Peer encouragement" "P-value: Contact tracing incentives = Peer incentives")) legend
	;
	
estimates clear;


* -------------------------------------------------------------------- ;
*Appendix Table B14: Effect of financial incentives, weighted regressions;
*Appendix Table B15: Effect of outreach types, weighted regressions;
*Appendix Table B16: Complementarities between incentives and outreach types, weighted regressions;
* -------------------------------------------------------------------- ;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city [aw=w] , cluster(id_centre);
	est store bI_`var';
	matrix MI=e(V);
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	test t_incentive_unconditional t_incentive_conditional;
	estadd scalar F_incentive= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_incentive_encouragement = t_incentive_unconditional;
	estadd scalar P_encouragement_unconditional=r(p);
	estadd scalar B_encouragement_unconditional=_b[t_incentive_unconditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_unconditional=sqrt(MI[2,2]+MI[1,1]-2*MI[2,1]);
	test t_incentive_encouragement=t_incentive_conditional;
	estadd scalar P_encouragement_conditional=r(p);
	estadd scalar B_encouragement_conditional=_b[t_incentive_conditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_conditional=sqrt(MI[3,3]+MI[1,1]-2*MI[3,1]);
	test t_incentive_conditional=t_incentive_unconditional;
	estadd scalar P_incentives=r(p);
	estadd scalar B_conditional_unconditional=_b[t_incentive_conditional]-_b[t_incentive_unconditional];
	estadd scalar SE_conditional_unconditional=sqrt(MI[3,3]+MI[2,2]-2*MI[3,2]);
	matrix drop MI;
	
	xi: reg `var' t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city [aw=w], cluster(id_centre);
	est store bO_`var';
	matrix MO=e(V);
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	estadd scalar F_outreach= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_outreach_peer=t_outreach_identified;
	estadd scalar P_peer_ident=r(p);
	estadd scalar B_peer_identified=_b[t_outreach_peer]-_b[t_outreach_identified];
	estadd scalar SE_peer_identified=sqrt(MO[2,2]+MO[1,1]-2*MO[2,1]);
	test t_outreach_peer=t_outreach_anonymous;
	estadd scalar P_peer_anon=r(p);
	estadd scalar B_peer_anon=_b[t_outreach_peer]-_b[t_outreach_anonymous];
	estadd scalar SE_peer_anon=sqrt(MO[3,3]+MO[1,1]-2*MO[3,1]);
	test t_outreach_identified=t_outreach_anonymous;
	estadd scalar P_tracing=r(p);
	estadd scalar B_anon_identified=_b[t_outreach_anonymous]-_b[t_outreach_identified];
	estadd scalar SE_anon_identified=sqrt(MO[2,2]+MO[3,3]-2*MO[3,2]);
	matrix drop MO;
	};
	
	
estout bI_* using "${output}AppTableB14.tex.tex", replace
	style(tex)
	keep(t_incentive_encouragement t_incentive_unconditional t_incentive_conditional)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_incentive B_encouragement_unconditional SE_encouragement_unconditional B_encouragement_conditional SE_encouragement_conditional B_conditional_unconditional SE_conditional_unconditional, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Encouragement-Unconditional" " " "Encouragement-Conditional" " " "Conditional-Unconditional" " " )) legend
	;
	
estout bO_* using "${output}AppTableB15.tex", replace
	style(tex)
	keep(t_outreach_peer t_outreach_identified t_outreach_anonymous)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach B_peer_identified SE_peer_identified B_peer_anon SE_peer_anon B_anon_identified SE_anon_identified, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Peer-Identified" " " "Peer-Anonymous" " " "Anonymous-Identified" " ")) legend
	;

estimates clear;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var'  t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city [aw=w], cluster(id_centre);
	est store bY_`var';
	test t_peer_encouragement=t_incentive_peer;
	estadd scalar p_peer=r(p);
	test t_ctr_encouragement=t_incentive_ctr;
	estadd scalar p_ct=r(p);
	test t_peer_encouragement=t_ctr_encouragement;
	estadd scalar p_enc=r(p);
	test t_incentive_peer = t_incentive_ctr;
	estadd scalar p_inc=r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	};
	
estout bY_* using "${output}AppTableB16.tex", replace
	style(tex)
	keep(t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	stats(N r2 mdepvar p_peer p_ct p_enc p_inc, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: Peer encouragement = Peer incentives" 
	"P-value: Contact tracing encouragement = Contact tracing incentives" "P-value: Contact tracing encouragement = Peer encouragement" "P-value: Contact tracing incentives = Peer incentives")) legend
	;
	
estimates clear;

* -------------------------------------------------------------------- ;
* Appendix Table B21: Heterogeneous effects of incentives on the number of referrals;
* Appendix Table B22: Heterogeneous effects of outreach types on the number of referrals;
* -------------------------------------------------------------------- ;

gen vhet=.;
gen v_encourage=.;
gen v_uncond=.;
gen v_cond=.;
gen v_fin=.;

label var vhet "Above median";
label var v_encourage "Above median * Encouragement";
label var v_uncond "Above median * Unconditional incentive";
label var v_cond "Above median * Conditional incentive";
label var v_fin "Above median * Financial incentive";

gen v_peer=.;
gen v_ident=.;
gen v_anon=.;
gen v_ct=.;

label var v_peer "Above median * Peer";
label var v_ident "Above median * Identified";
label var v_anon "Above median * Anonymous";
label var v_ct "Above median * Contact tracing";

foreach het in $bl_hetvars {;
	
	*** Make one table with all heterogeneity (outcome: number of referrals sent for testing);
	
	gen `het'_encourage=`het'*t_incentive_encouragement;
	gen `het'_uncond=`het'*t_incentive_unconditional;
	gen `het'_cond=`het'*t_incentive_conditional;
	gen `het'_fin=`het'*t_incentive_financial;

	gen `het'_peer=`het'*t_outreach_peer;
	gen `het'_ident=`het'*t_outreach_identified;
	gen `het'_anon=`het'*t_outreach_anonymous;
	gen `het'_ct=`het'*t_outreach_CT;

	replace vhet=`het';
	replace v_encourage=`het'_encourage;
	replace v_uncond=`het'_uncond;
	replace v_cond=`het'_cond;
	replace v_fin=`het'_fin;
	
	replace v_peer=`het'_peer;
	replace v_ident=`het'_ident;
	replace v_anon=`het'_anon;
	replace v_ct=`het'_ct;
	
		xi: reg a_num_ref_visit vhet t_incentive_encouragement t_incentive_financial v_encourage v_fin i.id_city, cluster(id_centre);
		est store bI_`het';
		test t_incentive_encouragement t_incentive_financial;
		estadd scalar F_incentive= r(p);
		qui sum a_num_ref_visit if t0==1 & vhet==0;
		estadd scalar mdepvar_below=r(mean);
		qui sum a_num_ref_visit if t0==1 & vhet==1;
		estadd scalar mdepvar_above=r(mean);

		
		xi: reg a_num_ref_visit vhet t_outreach_peer t_outreach_CT v_peer v_ct i.id_city, cluster(id_centre);
		est store bO_`het';
		test t_outreach_peer t_outreach_CT;
		estadd scalar F_outreach= r(p);
		qui sum a_num_ref_visit if t0==1 & vhet==0;
		estadd scalar mdepvar_below=r(mean);
		qui sum a_num_ref_visit if t0==1 & vhet==1;
		estadd scalar mdepvar_above=r(mean);

		
};	
	

	estout bI_* using "${output}AppTable B21.tex", replace
	style(tex)
	keep(vhet t_incentive_encouragement t_incentive_financial v_encourage v_fin)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar_above mdepvar_below F_incentive, fmt(0 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group, above median" "Mean of dep. var in control group, below median" "P-value: treatments jointly 0")) legend
	;
	
	estout bO_* using "${output}AppTableB22.tex", replace
	style(tex)
	keep(vhet t_outreach_peer t_outreach_CT v_peer v_ct)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach, fmt(0 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0")) legend
	;

	
* -------------------------------------------------------------------- ;
*Appendix Table B23 Comparison of existing patients and new symptomatics;
* -------------------------------------------------------------------- ;

gen ptype="cp";

ren bl_resp_female resp_female;
ren bl_resp_somelit resp_somelit;
ren bl_asset_index asset_index;
ren bl_resp_ppltalk resp_ppltalk;

append using "${created}new_patient_data_toappend.dta", force;

orth_out resp_female resp_somelit asset_index resp_ppltalk using "${output}AppendixTableB23.tex", by(ptype) se compare test count latex replace;

clear;
	
* -------------------------------------------------------------------- ;
*Appendix Table B24 Effects of incentive type on characteristics of referred patients;
*Appendix Table B25 Effects of outreach type on characteristics of referred patients ;
* -------------------------------------------------------------------- ;

use "${created}new_patient_data_merged.dta", clear ;


global outcomes "resp_female resp_somelit asset_index resp_ppltalk";

	foreach var in $outcomes {;
	xi: reg `var' t_incentive i.id_city if t0==0 & referral==1 & insamp==1, cluster(id_centre);
    sum `var' if t_incentive==0 & e(sample)==1;
    sum `var' if t_incentive==1 & e(sample)==1;
	
	xi: reg `var' t_outreach_peer i.id_city if t0==0 & referral==1 & insamp==1, cluster(id_centre);
    sum `var' if t_outreach_peer==0 & e(sample)==1;
	sum `var' if t_outreach_peer==1 & e(sample)==1;
	
	};

clear;


*********** ANALYSES ************;

*********************************;

**** CENTER-LEVEL REGRESSIONS****;

*********************************;



* -------------------------------------------------------------------- ;
use "${created}TB_analysis_data.dta", clear;
* -------------------------------------------------------------------- ;

* -------------------------------------------------------------------- ;
*Keep only CPs who were surveyed;
* -------------------------------------------------------------------- ;
keep if bl_survey_status_cleaned==1;

* -------------------------------------------------------------------- ;
*Drop Bhubaneshwar centers;
* -------------------------------------------------------------------- ;
/* Because a change in Operation ASHA's relationship with the leadership at 
the government testing center in Bhubaneshwar (Odisha), we were not
permitted to access administrative endline data for these patients*/;
drop if id_city==10;

* -------------------------------------------------------------------- ;
*Create globals;
* -------------------------------------------------------------------- ;
global bl_char "bl_resp_female bl_resp_hindu bl_resp_muslim bl_resp_somelit bl_resp_secondedu bl_asset_index bl_resp_havebank bl_resp_ppltalk" ;
global bl_health "bl_past_tb_treated bl_nodelay";
global bl_hetvars "dbl_asset dbl_contacts bl_nodelay bl_resp_female bl_treatIP";
global ref_num1 "a_num_ref_visit";
global ref_num2 "a_num_rec_test admin_num_tested a_num_pos";
global ref_char "ref_age ref_hindu ref_somelit ref_asset ref_bank ref_contacts ref_knowTB ref_poslife ref_betlife";
global ref_char_short "ref_asset ref_contacts ref_knowTB";
global ref_health "ref_nodelay ref_wksdelay";
global ref_health_short "ref_nodelay";


* -------------------------------------------------------------------- ;
*Compute number of current patients in each center;
* -------------------------------------------------------------------- ;
encode resp_id, gen(respnum);
bysort id_centre: egen ncenter=count(id_centre);

* -------------------------------------------------------------------- ;
*Coollapse data at the center level;
* -------------------------------------------------------------------- ;
keep a_num_ref_visit a_num_rec_test admin_num_tested a_num_pos id_city 
bl_resp_female bl_resp_hindu bl_resp_muslim bl_resp_somelit bl_resp_secondedu bl_asset_index bl_resp_havebank bl_resp_ppltalk bl_past_tb_treated bl_nodelay
t_incentive_encouragement t_incentive_unconditional t_incentive_conditional t_outreach_peer t_outreach_identified t_outreach_anonymous t0 treatment_group
t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr admin_num_started_treatment bl_patient_surveyed respnum id_centre ncenter;

collapse 
(mean) a_num_ref_visit a_num_rec_test admin_num_tested a_num_pos id_city ncenter
bl_resp_female bl_resp_hindu bl_resp_muslim bl_resp_somelit bl_resp_secondedu bl_asset_index bl_resp_havebank bl_resp_ppltalk bl_past_tb_treated bl_nodelay
t_incentive_encouragement t_incentive_unconditional t_incentive_conditional t_outreach_peer t_outreach_identified t_outreach_anonymous t0 treatment_group
t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr
(count) bl_patient_surveyed
(sum) admin_num_started_treatment
,by(id_centre) ;


* -------------------------------------------------------------------- ;
*Label treatment indicators;
* -------------------------------------------------------------------- ;

label var t_outreach_peer "Peer outreach";
label var t_outreach_identified "Identified contact tracing";
label var t_outreach_anonymous "Anonymous contact tracing";

label var t_incentive_encouragement "Encouragement";
label var t_incentive_unconditional "Unconditional incentive";
label var t_incentive_conditional "Conditional incentive";

label var t_peer_encouragement "Peer outreach, no financial incentive";
label var t_ctr_encouragement "Contact tracing, no financial incentive";
label var t_incentive_peer "Peer outreach, financial incentive";
label var t_incentive_ctr "Contact tracing, financial incentive";

gen t_any=(treatment_group>0);
label var t_any "Any treatment";

gen t_incentive_money=(t_incentive_unconditional==1 | t_incentive_conditional==1);
label var t_incentive_money "Conditional or unconditional incentive";

gen t_outreach_healthwrkr=(t_outreach_identified==1|t_outreach_anonymous==1);
label var t_outreach_healthwrkr "Identified or anonymous health worker outreach";


* -------------------------------------------------------------------- 
* Appendix Table B2: Testing whether the size of OPASHA centers is associated with experimental condition;
* -------------------------------------------------------------------- 

*Incentive types;
xi: reg ncenter t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city, robust;
lincom t_incentive_unconditional-t_incentive_encouragement;
lincom t_incentive_conditional-t_incentive_encouragement;
lincom t_incentive_unconditional-t_incentive_conditional;

*Outreach types;
xi: reg ncenter t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city, robust;
lincom t_outreach_peer-t_outreach_identified;
lincom t_outreach_peer-t_outreach_anonymous;
lincom t_outreach_identified-t_outreach_anonymous;


* -------------------------------------------------------------------- ;
*Appendix Table B17: Effect of financial incentives, clinic-level specification;
* -------------------------------------------------------------------- ;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city ncenter, robust;
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	lincom t_incentive_unconditional-t_incentive_encouragement;
	lincom t_incentive_conditional-t_incentive_encouragement;
	lincom t_incentive_unconditional-t_incentive_conditional;
	};


* -------------------------------------------------------------------- ;
*Appendix Table B18: Effect of outreach types, clinic-level specification;
* -------------------------------------------------------------------- ;

foreach var in $ref_num1 $ref_num2 {;
	xi: reg `var' t_outreach_peer t_outreach_identified t_outreach_anonymous i.id_city ncenter, robust;
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	lincom t_outreach_peer-t_outreach_identified;
	lincom t_outreach_peer-t_outreach_anonymous;
	lincom t_outreach_identified-t_outreach_anonymous;
	};

* -------------------------------------------------------------------- ;
*Appendix Table B19: Complementarities between incentives and outreach types, clinic-level specification;
* -------------------------------------------------------------------- ;

foreach var in $ref_num1 $ref_num2{;
	xi: reg `var'  t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city ncenter, robust;
	test t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr;
	lincom t_incentive_peer-t_peer_encouragement;
	lincom t_ctr_encouragement-t_incentive_ctr;
	lincom t_peer_encouragement-t_ctr_encouragement;
	lincom t_incentive_peer - t_incentive_ctr;
};

* -------------------------------------------------------------------- ;
* Table 5: Effect of treatment on referrals "quality";
* -------------------------------------------------------------------- ;

* 1. Share of referrals recommended for testing;
gen share = a_num_rec_test/a_num_ref_visit;
replace share=0 if share==.;

xi: reg share t_incentive_encouragement t_incentive_money i.id_city ncenter, robust /* encoragenent or incentive vs control*/;
xi: reg share t_outreach_peer t_outreach_healthwrkr i.id_city ncenter, robust /* peer outreach or health worker outreach vs control */;
xi: reg share t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city ncenter, robust /* "complementarities" specification */;


*Randomization-inference p-values;
cap drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_incentive_encouragement==1;
replace treatgr=2 if t_incentive_money==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2): reg share i.treatgr i.id_city ncenter, robust /* encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1): reg share i.treatgr i.id_city ncenter, robust /* incentive vs control*/;

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_outreach_peer==1;
replace treatgr=2 if t_outreach_healthwrkr==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2): reg share i.treatgr i.id_city ncenter, robust /* outreach peer vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1): reg share i.treatgr i.id_city ncenter, robust /* outreach health worker vs control*/;

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_peer_encouragement==1;
replace treatgr=2 if t_ctr_encouragement==1;
replace treatgr=3 if t_incentive_peer==1;
replace treatgr=4 if t_incentive_ctr==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2 3 4): reg share i.treatgr i.id_city ncenter, robust /* peer encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 3 4): reg share i.treatgr i.id_city ncenter, robust /* health worker encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 2 4): reg share i.treatgr i.id_city ncenter, robust /* peer incentivized vs control*/;
ritest treatgr (_b[0.treatgr]-_b[4.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 2 3): reg share i.treatgr i.id_city ncenter, robust /* health worker incentivized vs control*/;


* 2. Share of tested individuals/patients screened;
gen share2 = admin_num_tested/a_num_ref_visit;
replace share2=0 if share2==.;

xi: reg share2 t_incentive_encouragement t_incentive_money i.id_city ncenter, robust /* encoragenent or incentive vs control*/;
xi: reg share2 t_outreach_peer t_outreach_healthwrkr i.id_city ncenter, robust /* peer outreach or health worker outreach vs control */;
xi: reg share2 t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr i.id_city ncenter, robust /* "complementarities" specification */;


*Randomization-inference p-values;

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_incentive_encouragement==1;
replace treatgr=2 if t_incentive_money==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2): reg share2 i.treatgr i.id_city ncenter, robust /* encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1): reg share2 i.treatgr i.id_city ncenter, robust /* incentive vs control*/;

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_outreach_peer==1;
replace treatgr=2 if t_outreach_healthwrkr==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2): reg share2 i.treatgr i.id_city ncenter, robust /* outreach peer vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1): reg share2 i.treatgr i.id_city ncenter, robust /* outreach health worker vs control*/;

drop treatgr;
gen treatgr=.;
replace treatgr=0 if treatment_group==0;
replace treatgr=1 if t_peer_encouragement==1;
replace treatgr=2 if t_ctr_encouragement==1;
replace treatgr=3 if t_incentive_peer==1;
replace treatgr=4 if t_incentive_ctr==1;
tab treatgr;
ritest treatgr (_b[0.treatgr]-_b[1.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(2 3 4): reg share2 i.treatgr i.id_city ncenter, robust /* peer encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[2.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 3 4): reg share2 i.treatgr i.id_city ncenter, robust /* health worker encouragement vs control*/;
ritest treatgr (_b[0.treatgr]-_b[3.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 2 4): reg share2 i.treatgr i.id_city ncenter, robust /* peer incentivized vs control*/;
ritest treatgr (_b[0.treatgr]-_b[4.treatgr]), reps(2000) seed(999) strata(id_city) fixlevels(1 2 3): reg share2 i.treatgr i.id_city ncenter, robust /* health worker incentivized vs control*/;


* -------------------------------------------------------------------- ;
*Appendix Table B20: New patients enrolled at Operation ASHA clinics ;
* -------------------------------------------------------------------- ;

sort id_centre;
merge 1:1 id_centre using "${created}nonref_num_patients_listed.dta";

tab _merge;
keep if _merge==3 ;  /* 122 centers */;

gen new_patients= nonref_num_patients_listed+admin_num_started_treatment;
replace new_patients=. if admin_num_started_treatment==.;
gen new_patients_perc=new_patients/bl_patient_surveyed;

*controlling for baseline patients, no weights *;
xi: reg new_patients_perc t_any bl_patient_surveyed i.id_city;
outreg2 using "${output}/AppTableB20.xls", symbol(***,**,*) bdec(3) sdec(3) cttop("bl,no wgt") slow(2500) replace;
xi: reg new_patients_perc t_incentive_encouragement t_incentive_money bl_patient_surveyed i.id_city;
outreg2 using "${output}/AppTableB20.xls", symbol(***,**,*) bdec(3) sdec(3) cttop("bl,no wgt") slow(2500) append;
xi: reg new_patients_perc t_outreach_peer t_outreach_healthwrkr bl_patient_surveyed i.id_city;
outreg2 using "${output}/AppTableB20.xls", symbol(***,**,*) bdec(3) sdec(3) slow(2500) cttop("bl,no wgt") append;
xi: reg new_patients_perc t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr bl_patient_surveyed i.id_city;
outreg2 using "${output}/AppTableB20.xls", symbol(***,**,*) bdec(3) sdec(3) slow(2500) cttop("bl,no wgt") append;


clear;


************************************************************;
** NOTE: The following code requires Stata version 16 to run;
************************************************************;

use "${created}TB_analysis_data.dta", clear;

* -------------------------------------------------------------------- ;
*Macros for varlists;
* -------------------------------------------------------------------- ;
global bl_char "bl_resp_female bl_resp_hindu bl_resp_muslim bl_resp_somelit bl_resp_secondedu bl_asset_index bl_resp_havebank bl_resp_ppltalk" ;
global bl_hetvars "dbl_asset dbl_contacts bl_nodelay bl_resp_female bl_treatIP";
global ref_num1 "a_num_ref_visit";
global ref_num2 "a_num_rec_test admin_num_tested a_num_pos";
global city_fe "_Iid_city_4 _Iid_city_6 _Iid_city_7 _Iid_city_8 _Iid_city_9 _Iid_city_11 _Iid_city_12 _Iid_city_13";

* -------------------------------------------------------------------- ;
*Appendix Table B11: Effect of financial incentives, including baseline covariates selected with double lasso;
*Appendix Table B12 Effect of outreach strategies, including baseline covariates selected with double lasso;
*Appendix Table B13: Complementarities between incentives and outreach, including baseline covariates selected with double lasso;
* -------------------------------------------------------------------- ;

estimates clear;
set seed 123456789;

* -------------------------------------------------------------------- ;
*Keep only CPs who were surveyed;
* -------------------------------------------------------------------- ;
keep if bl_survey_status_cleaned==1;

* -------------------------------------------------------------------- ;
*Drop Bhubaneshwar centers;
* -------------------------------------------------------------------- ;
/* Because a change in Operation ASHA's relationship with the leadership at 
the government testing center in Bhubaneshwar (Odisha), we were not
permitted to access administrative endline data for these patients*/;
drop if id_city==10;

drop el_*;
vl set;
vl move (id_state centre_type bl_survey_status_cleaned bl_consent bl_minor bl_visit_number bl_interview_location admin_* ref_* refpatient_* num_* treatment_outreach_num treatment_incentive_num t_outreach_peer-t_incentive_financial t0-t9 dbl_* phase* treatment_group id_city a_num_ref_visit a_num_rec_test a_num_pos bl_patient_surveyed) vlother;
*_meRefpatient da_*   dadmin_* _meAdmin _meReferral;

vl move (id_centre resp_personal_id bl_lng_knw*) vlother;

vl create cc = vlcontinuous;
vl create fc = vlcategorical;

qui foreach var of varlist $vlcategorical {;
	gen mi_`var' = (mi(`var'));
	*recode `var' (.=0);
	replace `var' = 0 if mi(`var');
	cap assert mi_`var'==0; 		
	if _rc==0 {;
		drop mi_`var';
		};	
};

qui foreach var of varlist $vlcontinuous {;
	gen mi_`var' = (mi(`var'));
	qui sum `var';
	local m=r(mean);
	*recode `var' (.=`m');
	replace `var' = 0 if mi(`var');
	cap assert mi_`var'==0; 		
	if _rc==0 {;
		drop mi_`var';
		};	
};



qui xi: reg a_num_ref_visit t_incentive_encouragement t_incentive_unconditional t_incentive_conditional i.id_city;


foreach var in $ref_num1 $ref_num2 {;
	dsregress `var' t_incentive_encouragement t_incentive_unconditional t_incentive_conditional $city_fe, controls($cc i.($fc) mi_*) rseed(1234) missingok;
	est store bI_`var';
	matrix MI=e(V);
	test t_incentive_encouragement t_incentive_unconditional t_incentive_conditional;
	test t_incentive_unconditional t_incentive_conditional;
	estadd scalar F_incentive= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_incentive_encouragement = t_incentive_unconditional;
	estadd scalar P_encouragement_unconditional=r(p);
	estadd scalar B_encouragement_unconditional=_b[t_incentive_unconditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_unconditional=sqrt(MI[2,2]+MI[1,1]-2*MI[2,1]);
	test t_incentive_encouragement=t_incentive_conditional;
	estadd scalar P_encouragement_conditional=r(p);
	estadd scalar B_encouragement_conditional=_b[t_incentive_conditional]-_b[t_incentive_encouragement];
	estadd scalar SE_encouragement_conditional=sqrt(MI[3,3]+MI[1,1]-2*MI[3,1]);
	test t_incentive_conditional=t_incentive_unconditional;
	estadd scalar P_incentives=r(p);
	estadd scalar B_conditional_unconditional=_b[t_incentive_conditional]-_b[t_incentive_unconditional];
	estadd scalar SE_conditional_unconditional=sqrt(MI[3,3]+MI[2,2]-2*MI[3,2]);
	matrix drop MI;
	
	dsregress `var' t_outreach_peer t_outreach_identified t_outreach_anonymous $city_fe, controls($cc i.($fc) mi_*) rseed(1234) missingok;
	est store bO_`var';
	matrix MO=e(V);
	test t_outreach_peer t_outreach_identified t_outreach_anonymous;
	estadd scalar F_outreach= r(p);
	qui sum `var' if t0==1;
	estadd scalar mdepvar=r(mean);
	test t_outreach_peer=t_outreach_identified;
	estadd scalar P_peer_ident=r(p);
	estadd scalar B_peer_identified=_b[t_outreach_peer]-_b[t_outreach_identified];
	estadd scalar SE_peer_identified=sqrt(MO[2,2]+MO[1,1]-2*MO[2,1]);
	test t_outreach_peer=t_outreach_anonymous;
	estadd scalar P_peer_anon=r(p);
	estadd scalar B_peer_anon=_b[t_outreach_peer]-_b[t_outreach_anonymous];
	estadd scalar SE_peer_anon=sqrt(MO[3,3]+MO[1,1]-2*MO[3,1]);
	test t_outreach_identified=t_outreach_anonymous;
	estadd scalar P_tracing=r(p);
	estadd scalar B_anon_identified=_b[t_outreach_anonymous]-_b[t_outreach_identified];
	estadd scalar SE_anon_identified=sqrt(MO[2,2]+MO[3,3]-2*MO[3,2]);
	matrix drop MO;
	
	dsregress `var' t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr $city_fe, controls($cc i.($fc) mi_*) rseed(1234) missingok;
	est store bZ_`var'; 
	test t_peer_encouragement=t_incentive_peer; 
	estadd scalar p_peer=r(p); 
	test t_ctr_encouragement=t_incentive_ctr; 
	estadd scalar p_ct=r(p); 
	test t_peer_encouragement=t_ctr_encouragement; 
	estadd scalar p_enc=r(p); 
	test t_incentive_peer = t_incentive_ctr; 
	estadd scalar p_inc=r(p); 
	qui sum `var' if t0==1; 
	estadd scalar mdepvar=r(mean);
	};
	
	
estout bI_* using "${output}TableB11.tex", replace
	style(tex)
	keep(t_incentive_encouragement t_incentive_unconditional t_incentive_conditional)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_incentive B_encouragement_unconditional SE_encouragement_unconditional B_encouragement_conditional SE_encouragement_conditional B_conditional_unconditional SE_conditional_unconditional, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Encouragement-Unconditional" " " "Encouragement-Conditional" " " "Conditional-Unconditional" " " )) legend
	;
	
		 	
estout bO_* using "${output}TableB12.tex", replace
	style(tex)
	keep(t_outreach_peer t_outreach_identified t_outreach_anonymous)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par)) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar F_outreach B_peer_identified SE_peer_identified B_peer_anon SE_peer_anon B_anon_identified SE_anon_identified, 
	fmt(0 2 2 2 3 3 3 3 3 3) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: treatments jointly 0"
	"Peer-Identified" " " "Peer-Anonymous" " " "Anonymous-Identified" " ")) legend
	;
	
estout bZ_* using "${output}TableB13.tex", replace
	style(tex)
	keep(t_peer_encouragement t_ctr_encouragement t_incentive_peer t_incentive_ctr)
	numbers label
	title("", @title)
	collabels(, none) mlabels(, dep)
	posthead("") varwidth(30) modelwidth(10)
	cells(b(star fmt(3)) se(par) ri(par({}))) starlevels(* 0.10 ** 0.05 *** 0.001) 
	stats(N r2 mdepvar p_peer p_ct p_enc p_inc, fmt(0 2 2 2 2 2 2) labels("Observations" "R-squared" "Mean of dep. var. in control group" "P-value: Peer encouragement = Peer incentives" 
	"P-value: Contact tracing encouragement = Contact tracing incentives" "P-value: Contact tracing encouragement = Peer encouragement" "P-value: Contact tracing incentives = Peer incentives")) legend
	;

lassocoef (bI_a_num_ref_visit, for(a_num_ref_visit)) (bI_a_num_rec_test, for(a_num_rec_test)) (bI_admin_num_tested, for(admin_num_tested)) (bI_a_num_pos, for(a_num_pos));
lassocoef (bO_a_num_ref_visit, for(a_num_ref_visit)) (bO_a_num_rec_test, for(a_num_rec_test)) (bO_admin_num_tested, for(admin_num_tested)) (bO_a_num_pos, for(a_num_pos));
lassocoef (bZ_a_num_ref_visit, for(a_num_ref_visit)) (bZ_a_num_rec_test, for(a_num_rec_test)) (bZ_admin_num_tested, for(admin_num_tested)) (bZ_a_num_pos, for(a_num_pos));

estimates clear;

log close;

*********************;
*** END OF DO FILE***;
*********************;
