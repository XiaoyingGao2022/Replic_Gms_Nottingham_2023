******************    REPLICATION CODE FOR HOBO ECONOMICUS   *******************

log using "output\table1",text name(table1) nomsg replace
***********************************************************************************
**** REPLICATION OF SUMMARY STATISTICS REPORTED IN TABLE 1

* PANEL A: SUMMARY STATISTICS USING STATION VISITS:
use "data\stationvisit.dta",clear
* FULL SAMPLE
sum panhandlers passersby friendly service shuttle
* FIVE-STATION SUBSAMPLE
sum panhandlers passersby friendly service shuttle if subsample==1

* PANEL B: SUMMARY STATISTICS USING PANHANDLING RECEIPTS
use "data\receipts.dta",clear
sum hourlydonations hourlydollars
***********************************************************************************
log close table1


***********************************************************************************
**** REPLICATION OF RESULTS REPORTED IN TABLE 2: DETERMINANTS OF THE NUMBER OF PANHANDLERS ACROSS METRO STATIONS
use "data\stationvisit.dta",clear
reg panhandlers passersby friendly shuttle service i.day,vce(cluster metroid)
outreg using "output\table2.doc", keep(passersby friendly shuttle service) se sdec(3) ///
varlabel title("Table 2. Determinants of the number of panhandlers across metro stations") nostars summstat(r2_a\N) summtitle("Adjusted R2"\"N") addrows("Date fixed effects","X")  ctitles("Dependent variable: Number of panhandlers", "Full Sample (1)") note("Notes: Observations are Metro station-visits. Robust standard errors clustered by Metro station in parentheses. Appendix A maps and Appendix B enumerates stations in both samples.") nocons nodisplay replace

reg panhandlers pass friendly shuttle service i.day if subsample==1,vce(cluster metroid)
outreg using "output\table2.doc", keep(pass friendly shuttle service) se sdec(3) varlabel nostars summstat(r2_a\N) summtitle("Adjusted R2"\"N") addrows("Date fixed effects","X") ctitles("", "Five-station subsample (2)") nocons nodisplay merge
***********************************************************************************


log using "output\table3",text name(table3) nomsg replace
use "data\receipts.dta",clear
***********************************************************************************
**** REPLICATION OF RESULTS REPORTED IN TABLE 3: TEST OF EQUALITY OF VARIANCE

** PANEL A: Donations per hour
robvar hourlydonations,by(metro)

** PANEL B: Dollars per hour
robvar hourlydollars,by(metro)

* NOTES: W50 reports the Brown-Forsythe statistic from table 3.
* W0 reports Levene's test, reported in footnote 17.
***********************************************************************************
log close table3


log using "output\table4",text name(table4) nomsg replace
use "data\receipts.dta",clear
***********************************************************************************
**** REPLICATION OF RESULTS REPORTED IN TABLE 4: TEST OF EQUALITY OF MEAN

** PANEL A: Donations per hour
oneway hourlydonations metroid

** PANEL B: Dollars per hour
oneway hourlydollars metroid
***********************************************************************************
log close table4


log using "output\table5",text name(table5) nomsg replace
use "data\receipts.dta",clear
***********************************************************************************
**** REPLICATION OF RESULTS REPORTED IN TABLE 5: TEST OF EQUALITY OF MEDIAN

** PANEL A: Donations per hour
kwallis hourlydonations,by(metro)

** PANEL B: Dollars per hour
kwallis hourlydollars,by(metro)

* MEDIAN DONATIONS PER HOUR AND DOLLARS PER HOUR BY METRO STATION
tabstat hourlydonations hourlydollars,by(metro) s(median) c(s) varwidth(20) noseparator long
***********************************************************************************
log close table5


***********************************************************************************
************** REPLICATION CODE FOR FIGURE 1, PANELS A AND B
use "data\receipts.dta",clear
*PANEL A: DONATIONS PER HOUR
stripplot hourlydonations,mcolor(black) xsize(22) ysize(16) scheme(s1mono) over(metro) cumul cumprob bar(lcolor(black) mean(mcolor(black)) level(95)) boffset(0) centre vertical refline(lcolor(black)) reflevel(median) ytitle("Donations per hour") xtitle("Metro station",) yla(#6, ang(h) labsize(small)) xla(,labsize(small) noticks) ysc(titlegap(2)) xsc(titlegap(4)) name(figure1a,replace)
graph export "output\figure1a.png",name(figure1a) replace

*PANEL B: DOLLARS PER HOUR
stripplot hourlydollars,mcolor(black) xsize(22) ysize(16) scheme(s1mono) over(metro) cumul cumprob bar(lcolor(black) mean(mcolor(black)) level(95)) boffset(0) centre vertical refline(lcolor(black)) reflevel(median) ytitle("Dollars per hour") xtitle("Metro station",) yla(#6, ang(h) labsize(small)) xla(,labsize(small) noticks) ysc(titlegap(2)) xsc(titlegap(4)) name(figure1b,replace)
graph export "output\figure1b.png",name(figure1b) replace
***********************************************************************************


log using "output\intext",text name(intext) nomsg replace
use "data\receipts.dta",clear
***********************************************************************************
************** REPLICATION CODE IN-TEXT POWER ANALYSIS

**** POWER ANALYSIS FOR ANOVA TEST OF EQUALITY IN MEAN DONATIONS PER HOUR ACROSS STATIONS

/* POWER ESTIMATE (1):
WITHIN- AND BETWEEN VARIANCE EQUAL THE SAMPLE ESTIMATE*/
oneway hourlydonations metroid
power oneway, ngroups(5) n1(14) n2(12) n3(17) n4(17) n5(20) varerror(10.40) varmeans(4.14) table
/* POWER ESTIMATE (2):
WITHIN-VARIANCE IS 15% LARGER AND BETWEEN VARIANCE IS 15 % SMALLER THAN SAMPLE ESTIMATE*/
display 4.14224943*0.85
display 10.3963257*1.15
power oneway, ngroups(5) n1(14) n2(12) n3(17) n4(17) n5(20) varerror(11.96) varmeans(3.52) table
/* POWER ESTIMATE (3):
WITHIN-VARIANCE IS 15% SMALLER AND BETWEEN VARIANCE IS 15 % LARGER THAN SAMPLE ESTIMATE*/
display 4.14224943*1.15
display 10.3963257*0.85
power oneway, ngroups(5) n1(14) n2(12) n3(17) n4(17) n5(20) varerror(8.84) varmeans(4.76) table


**** POWER ANALYSIS FOR ANOVA TEST OF EQUALITY IN MEAN DOLLARS PER HOUR ACROSS STATIONS

/* POWER ESTIMATE (1):
WITHIN- AND BETWEEN VARIANCE EQUAL THE SAMPLE ESTIMATE*/
oneway hourlydollars metroid
power oneway, ngroups(5) n1(11) n2(9) n3(15) n4(16) n5(16) varerror(128.48) varmeans(19.38) table
/* POWER ESTIMATE (2):
WITHIN-VARIANCE IS 15% LARGER AND BETWEEN VARIANCE IS 15 % SMALLER THAN SAMPLE ESTIMATE*/
display 19.3847426*.85
display 128.478645*1.15
power oneway, ngroups(5) n1(11) n2(9) n3(15) n4(16) n5(16) varerror(147.75) varmeans(16.48) table
/* POWER ESTIMATE (3):
WITHIN-VARIANCE IS 15% SMALLER AND BETWEEN VARIANCE IS 15 % LARGER THAN SAMPLE ESTIMATE*/
display 19.3847426*1.15
display 128.478645*0.85
power oneway, ngroups(5) n1(11) n2(9) n3(15) n4(16) n5(16) varerror(109.21) varmeans(22.29) table
***********************************************************************************
log close intext
