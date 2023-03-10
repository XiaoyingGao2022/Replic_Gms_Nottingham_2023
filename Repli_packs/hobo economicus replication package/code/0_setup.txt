*********    REPLICATION SETUP FOR HOBO ECONOMICUS   ***********
clear all
set more off
set linesize 225

*** MANUALLY EDIT PATH BELOW:
cd "C:\Documents\Projects\replicate"

* Install STRIPPLOT
ssc install stripplot,replace
ssc install outreg,replace
ssc install labutil,replace
