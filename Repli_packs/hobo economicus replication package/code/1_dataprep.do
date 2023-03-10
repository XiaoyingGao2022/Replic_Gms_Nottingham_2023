*********    REPLICATION DATA PREPARATION FOR HOBO ECONOMICUS   ***********

****************************************************************************************
**** REPLICATION CODE FOR GENERATING VARIABLES AND DATA USING NUMBER OF PANHANDLERS
import delimited using "data\metrovisitslog",clear
save "data\metrovisitslog.dta",replace
encode metro,gen(metroid)
order metroid,first
gen day=date(date,"MDY")
format day %tdDayname_Month_dd,_CCYY
gen month=month(day)
gen year=year(day)
sort metroid day
save "data\metrovisitslog.dta",replace

****************************************************************************************
**** REPLICATION CODE FOR GENERATING VARIABLES AND DATA USING NUMBER OF PASSERSBY
import delimited using "data\passersby",clear
save "data\passersby.dta",replace
encode metro,gen(metroid)
order metroid,first
save "data\passersby.dta",replace

****************************************************************************************
**** REPLICATION CODE FOR GENERATING VARIABLES AND DATA USING METRO STATION CHARACTERISTICS
import delimited using "data\service",clear colr(1:3)
save "data\service.dta",replace
drop if metro==""
encode metro,gen(metroid)
order metroid,first
gen service=(servicemin<=10)
label var metro "Metro station"
label var metroid "Metro station ID"
label var servicemin "Walking distance to closest service in minutes"
label var closestservice "Address of closest homeless service provider"
label var service "Within 10 minutes of walking distance to closest service"
save "data\service.dta",replace

import delimited using "data\shuttle",clear colr(1:3)
save "data\shuttle.dta",replace
drop if metro==""
encode metro,gen(metroid)
order metroid,first
gen shuttle=(shuttlemin<=10)
label var metro "Metro station"
label var metroid "Metro station ID"
label var shuttlemin "Walking distance to closest shuttle stop in minutes"
label var closestshuttle "Address of closest homeless shuttle stop"
label var shuttle "Within 10 minutes of walking distance to closest homeless shuttle stop"
save "data\shuttle.dta",replace

****************************************************************************************
**** REPLICATION CODE FOR GENERATING VARIABLES AND DATA USING FRIENDLINESS OF PASSERSBY
import delimited using "data\friendliness",clear
save "data\friendliness.dta",replace
encode metro,gen(metroid)
order metroid,first

gen friendly=5 if passresp5==1
replace friendly=4 if (passresp4==1 & friendly==.)
replace friendly=3 if (passresp3==1 & friendly==.)
replace friendly=2 if (passresp2==1 & friendly==.)
replace friendly=1 if (passresp1==1 & friendly==.)

label var passerbyid "Passerby ID"
label var passresp1 "Passerby response category 1"
label var passresp2 "Passerby response category 2"
label var passresp3 "Passerby response category 3"
label var passresp4 "Passerby response category 4"
label var passresp5 "Passerby response category 5"
label var friendly "Passerby's friendliest response"
label var date "Date"
label var metro "Metro station"
label var metroid "Metro station ID"
save "data\friendliness.dta",replace
*** GENERATE METRO STATION FRIENDLINESS OF PASSERSBY:
preserve
collapse (first) metro (mean) friendly, by(metroid)
save "data\stationfriendliness.dta",replace

****************************************************************************************
**** REPLICATION CODE FOR COMBINING REQUIRED DATASETS FOR TABLE 2:
use "data\metrovisitslog.dta"
save "data\stationvisit.dta",replace
merge m:1 metroid month year using "data\passersby.dta"
drop _merge
merge m:1 metroid using "data\service.dta",keepusing(service)
drop _merge
merge m:1 metroid using "data\shuttle.dta",keepusing(shuttle)
drop _merge
merge m:1 metroid using "data\stationfriendliness.dta"
drop _merge

gen subsample=(metroid==9|metroid==10|metroid==14|metroid==17|metroid==18)
label var date "Date"
label var year "Year"
label var month "Month"
label var day "Date (Stata format)"
label var metro "Metro station"
label var metroid "Metro station ID"
label var panhandlers "Number of panhandlers"
label var passersby "Number of passersby per"
label var service "Within 10 minutes of walking distance to closest homeless service provider"
label var shuttle "Within 10 minutes of walking distance to closest homeless shuttle stop"
label var friendly "Average of passersby's friendliest response"
label var subsample "Five station subsample dummy"
compress
save "data\stationvisit.dta",replace

****************************************************************************************
**** REPLICATION CODE FOR GENERATING VARIABLES AND DATA USING PANHANDLING RECEIPTS
import delimited using "data\receipts",clear
save "data\receipts.dta",replace
gen metroid=9 if metro=="Farragut North"
replace metroid=10 if metro=="Farragut West"
replace metroid=14 if metro=="Gallery Place"
replace metroid=17 if metro=="McPherson Square"
replace metroid=18 if metro=="Metro Center"
order metroid,after(metro)
labmask metroid,values(metro)

gen day=date(date,"MDY")
format day %tdDayname_Month_dd,_CCYY
gen month=month(day)
gen year=year(day)
order day month year,after(date)
sort metroid day
gen hourlydonations=60*donations/minutes
gen hourlydollars=60*(cash+inkind)/minutes

label var observationid "Observation ID"
label var panhandlerid "Unique panhandler in subsample ID"
label var date "Date"
label var year "Year"
label var month "Month"
label var day "Date (Stata format)"
label var metro "Metro station"
label var metroid "Metro station ID"
label var minutes "Minutes observed working"
label var donations "Number of donations received"
label var inkind "Dollars received in in-kind donations"
label var cash "Dollars received in cash donations"
label var declinedoffer "Declined offer to count dollars received in cash donations"
label var hourlydonations "Donations received per hour"
label var hourlydollars "Dollars received per hour"
compress
save "data\receipts.dta",replace
