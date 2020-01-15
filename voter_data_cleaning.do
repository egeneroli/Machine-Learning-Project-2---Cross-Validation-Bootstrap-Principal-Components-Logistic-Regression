set more off
cd "C:\Users\Evan Generoli\Dropbox\SaML_Project_2"
clear
use voter_data.dta

drop if year < 1994


gen vote = .
replace vote = 0 if voted == 1
replace vote = 1 if voted == 2


gen faminc2 = .
replace faminc2 = 2500  if faminc == 100
replace faminc2 = 500   if faminc == 110
replace faminc2 = 1500  if faminc == 120
replace faminc2 = 2500  if faminc == 130
replace faminc2 = 3500  if faminc == 140
replace faminc2 = 4500  if faminc == 150
replace faminc2 = 6250  if faminc == 210
replace faminc2 = 5500  if faminc == 220
replace faminc2 = 6750  if faminc == 231
replace faminc2 = 8750  if faminc == 300
replace faminc2 = 11250 if faminc == 430
replace faminc2 = 11000 if faminc == 440
replace faminc2 = 13500 if faminc == 460
replace faminc2 = 13750 if faminc == 470
replace faminc2 = 17500 if faminc == 500
replace faminc2 = 16250 if faminc == 540
replace faminc2 = 18750 if faminc == 550
replace faminc2 = 22500 if faminc == 600
replace faminc2 = 37500 if faminc == 700
replace faminc2 = 27500 if faminc == 710
replace faminc2 = 32500 if faminc == 720
replace faminc2 = 37500 if faminc == 730
replace faminc2 = 45000 if faminc == 740
replace faminc2 = 50000 if faminc == 800
replace faminc2 = 62500 if faminc == 810
replace faminc2 = 55000 if faminc == 820
replace faminc2 = 67500 if faminc == 830
replace faminc2 = 75000 if faminc == 840
replace faminc2 = 87500 if faminc == 841
replace faminc2 = 125000 if faminc == 842
replace faminc2 = 150000 if faminc == 843
replace faminc2 = . if faminc == 995
replace faminc2 = . if faminc == 996
replace faminc2 = . if faminc == 997
replace faminc2 = . if faminc == 999
rename faminc2 family_income

replace race = 650 if race==651 | race==652
replace race = 700 if race > 700
label define race_lbl 700 "other", modify
label define race_lbl 100 "White" 200 "Black/Negro" 300 "American Indian/Aleut/Eskimo" ///
650 "Asian or Pacific Islander" 700 "Other", replace

gen white = (race == 100)
gen black = (race == 200)
gen asian = (race == 650)
gen othrace = (race == 300 | race == 700)


gen college = .
replace college = 0 if educ > 1 | educ < 80
replace college = 1 if educ >= 80 & educ != 999

gen metro_area = .
replace metro_area = 0 if metro == 1
replace metro_area = 1 if metro == 2 | metro == 3

gen married = .
replace married = 1 if marst <= 3
replace married = 0 if marst > 3 & marst != 9 

replace uhrsworkt = . if uhrsworkt == 997 | uhrsworkt == 999
label drop uhrsworkt_lbl
recast int uhrsworkt
rename uhrsworkt usual_hrs_worked

gen employed = .
replace employed = 0 if inlist(empstat, 1,10,12,36)
replace employed = 1 if empstat > 12 & empstat != 36

replace sex = . if sex == 9
label define sex_lbl 1 "Male" 2 "Female", replace
gen male = 1 if sex==1
replace male = 0 if sex==2

gen veteran = .
replace veteran = 0 if vetstat < 2
replace veteran = 1 if vetstat == 2

gen foreign_born = .
replace foreign_born = 0 if nativity < 5
replace foreign_born = 1 if nativity == 5

gen hispanic = .
replace hispanic = 0 if hispan == 0
replace hispanic = 1 if hispan > 0 & hispan < 900

gen lab_force = . 
replace lab_force = 0 if labforce == 1
replace lab_force = 1 if labforce == 2

gen midterm = .
replace midterm = 0 if inlist(year, 1996,2000,2004,2008,2012,2016)
replace midterm = 1 if inlist(year, 1994,1998,2002,2006,2010,2014)

replace usual_hrs_worked = . if usual_hrs_worked > 80

recast int age
label drop age_lbl

drop if statefip > 56
label define statefip_lbl 1 "Alabama" 2 "Alaska" 4 "Arizona" ///
 5 "Arkansas" 6 "California" 8 "Colorado" 9 "Connecticut" ///
 10 "Delaware" 11 "District of Columbia" 12 "Florida" ///
 13 "Georgia" 15 "Hawaii" 16 "Idaho" 17 "Illinois" ///
 18 "Indiana" 19 "Iowa" 20 "Kansas" 21 "Kentucky" ///
 22 "Louisiana" 23 "Maine" 24 "Maryland" 25 "Massachusetts" ///
 26 "Michigan" 27 "Minnesota" 28 "Mississippi" 29 "Missouri" ///
 30 "Montana" 31 "Nebraska" 32 "Nevada" 33 "New Hampshire" ///
 34 "New Jersey" 35 "New Mexico" 36 "New York" ///
 37 "North Carolina" 38 "North Dakota" 39 "Ohio" ///
 40 "Oklahoma" 41 "Oregon" 42 "Pennsylvania" ///
 44 "Rhode Island" 45 "South Carolina" 46 "South Dakota" ///
 47 "Tennessee" 48 "Texas" 49 "Utah" 50 "Vermont" ///
 51 "Virginia" 53 "Washington" 54 "West Virginia" ///
 55 "Wisconsin" 56 "Wyoming", replace
rename statefip state

drop serial month hwtfinl cpsid region nativity metro metarea ///
	county statecensus faminc pernum wtfinl cpsidp sex marst ///
	vetstat citizen hispan empstat labforce classwkr uhrswork1 ///
	wkstat educ hourwage union voteres voted voreg nfams ///
	nfams ncouples nmothers nfathers race employed usual_hrs_worked

label drop nchild_lbl  famsize_lbl


 
 

saveold voter_data_cleaned.dta, replace version(13)

set more on

