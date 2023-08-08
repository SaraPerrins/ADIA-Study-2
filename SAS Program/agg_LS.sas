options nofmterr;

libname dat 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';
libname RB 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\RB';
libname pce 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\PCE';
libname MH  'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\MH';
libname OCS 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\OCS';


proc format;
value timefmt 
1 = 'early childhood(0-6 yrs)'
2 = 'childhood(7-10 yrs)'
3 = 'adolescent(11-14 yrs)'
4 = 'other(>14 yrs)';
value YESNO
0="NO"
1="YES";
value sexf
0="Male"
1="Female";
value racef
1="White"
2="Black"
3="Hispanic"
4="Multiracial"
5="Other";
value incomef
1="Less than $20,000"
2="$20,000-$49,999"
3="$50,000+";
value centerf
1="EA"
2="MW"
3="NW"
4="SO"
5="SW";
run;

%macro freq(var, time);
Data LS_all1;
set LS_all1;
if time >= &time; 
run;
proc sort data=LS_all1; by time; run;
proc freq data=LS_all1;
by time;
tables  &var / out=cnt binomial (level="1") alpha=0.05;
exact binomial;
ods output binomialprop=bin(where=(name1 in ('XL_BIN', 'XU_BIN')));
run;
proc sort data=cnt;
by time &var;
run;
data cntx;
set cnt;
by time &var;
if first.time;
keep time &var;
run;
proc sort data=bin;
by time;
run;
Data comb;
merge bin cntx;
by time;
run;
proc transpose data=comb out=ci prefix=ci;
by time;
id name1;
var nvalue1;
run;
proc sort data=LS_all1; by id time; run;
proc summary data=LS_all1;
class id time;
var &var;
output out=agg(keep=id time &var) max=;
run;
Data agg;
set agg;
if id ne "" & time ne .;
run;
Proc summary data=agg;
class time;
var &var;
output out=total(keep=time total) N=total;
run;
Data total;
set total;
if time ne .;
run;
Data freq (keep=time pct);
set cnt;
if &var=1;
pct=percent/100;
format pct percent8.2;
run;
Data &var;
length name $20.;
merge total freq ci;
by time;
name="&var";
run;
%mend;


Data LS_all;
set dat.LS_all_long;
run;

Data LS_all1;
set LS_all;
if center="EA" then Region=1;
if center="MW" then Region=2;
if center="NW" then Region=3;
if center="SO" then Region=4;
if center="SW" then region=5;

neighborhood_safety_old=neighborhood_safety;

if time=3 then neighborhood_safety=neighborhood_safety_old;
if time=2 then neighborhood_safety=.;

if time=2 then neighborhood_safety_y=neighborhood_safety_old;


label 
Region="Region"
anysupadu ='Experience of ANY supportive adult-binary'
anysupparent ='Experience of a supportive parent'
anysuprelative ='Experience of a supportive relative'
anysupnonfam ='Experience of a supportive non-familial adult'
fam_sat ='Caregiver satisfaction with family functioning-binary'
prrelation ="Experience of positive peer relationships-binary"
bestfriend ="Experience of a best friend during adolescence"
socialpart ="Strength of social participation in adolescence (ages 12 or 14)-binary"
home_safety ="Perception of safety at home-binary"
parent_involv ="Strength of parental involvement in youth activities during adolescence as a Protective Factor"
resid_stab ="Number of years the child has lived in the neighborhood (residential stability)"
neighborhood_safety ="Caregiver perception of neighborhood safety during adolescence"
neighborhood_exp ="Caregiver perception of positive neighborhood experiences during adolescence—presence of supportive 
adults in the community (neighborhood’s collective efficacy)"
neighborhood_safety_y ="youth perception of neighborhood safety"
school_safety_y ="Youth perception of school safety"
school_safety_t ="Teacher perception of school safety during early childhood"
srvc_use ="Access to preventative or ongoing medical service utilization In the past year"
childcare ="Presence of child care during early childhood"
M ='MMCS allegation'
MP ='phy mtx'
MS ='sexual mtx'
MN ='neglect'
MF ='neglect-ftp'
ML ='neglect-los'
ME ='emotional mtx'
MM ='moral mtx'
MD ='edu mtx'
MA ='alcohol/drug' 
ocs_neglect_beh ="Neglectful behavior"
ocs_harsh_pv ="Harsh Physical Violence"
ocs_BN_food ="Basic Need Instability – Food Insecurity"
ocs_BN_cstressor ="Caregiver stressors"
ocs_BN_poverty ="Basic Need Instability- Poverty"
ocs_BN_homeless ="Basic Need Instability- Homeless"
ocs_sep ="Traumatic separation from a loved one"
ocs_trauma_sys ="Contact with Potentially traumatic system"
ocs_trauma_court ="Contact with potentially traumatic system (court involvement)"
ocs_RB_house ="Risk behaviors of caregiver and household members"
ocs_RB_peer ="Risk behaviors of friends"
ocs_crime_victim ="Victim of a crime"
ocs_wit_vio ="Witnessed violence";

format childsex_bsl sexf. childrace_bsl racef. caregiver_married16_3 YESNO. hh_income16_3 incomef. region centerf.;

run;

***************************************************************************************************************************************************************;

************************************************************DESCRIPTIVES STATISTICS STARTS HERE****************************************************************;

***************************************************************************************************************************************************************;

****************************************************;
**demographics;
**https://communities.sas.com/t5/Statistical-Procedures/95-CI-for-Categorical-Variables/td-p/245713;
****************************************************; 

Data control;
length ID $7.;
set dat.longscan_crosssectional_dataset2;
keep id childsex_bsl childrace_bsl hh_income: caregiver_married: childrace_bsl2 /*fr_supportparent_cat1416 fr_supportoth_cat1416 comm_safety: comm_ce:*/ ;
run;

proc contents data=control; run;

Data control;
set control;
if substr(ID,1,2)="EA" then center=1;
if substr(ID,1,2)="MW" then center=2;
if substr(ID,1,2)="NW" then center=3;
if substr(ID,1,2)="SO" then center=4;
if substr(ID,1,2)="SW" then center=5;
run;

*****************************************************;
**PCE ;
*https://www.lexjansen.com/pharmasug/2009/cc/CC20.pdf;
*****************************************************;

%freq(neighborhood_exp, 1);
%freq(neighborhood_safety, 1);
%freq(neighborhood_safety_y, 1);
%freq(school_safety_t, 1);
%freq(school_safety_y, 1);
%freq(childcare, 1);

Data pce_freq;
set neighborhood_exp
neighborhood_safety
neighborhood_safety_y
school_safety_t
school_safety_y
childcare;
by time;
run;

proc sort data=pce_freq; by name time; run;
proc transpose data=pce_freq out=total prefix=n;
by name;
id time;
var total ;
run;
proc transpose data=pce_freq out=pct prefix=pct;
by name;
id time;
var  pct ;
run;
proc transpose data=pce_freq out=ciXL_BIN prefix=ciXL;
by name;
id time;
var ciXL_BIN ;
run;
proc transpose data=pce_freq out=ciXU_BIN prefix=ciXU;
by name;
id time;
var ciXU_BIN;
run;

Data pce_freqout(drop=_name_ _label_);
length label $100.;
merge total pct ciXL_BIN ciXU_BIN;
by name;
if name ="neighborhood_safety" then label="Caregiver perception of neighborhood safety during adolescence";
if name ="neighborhood_safety_y" then label="Youth perception of neighborhood safety";
if name ="neighborhood_exp" then label="Caregiver perception of positive neighborhood experiences during adolescence—presence of supportive adults in the community (neighborhood’s collective efficacy)";
if name ="school_safety_y" then label="Youth perception of school safety";
if name ="school_safety_t" then label="Teacher perception of school safety during early childhood";
if name ="childcare" then label="Presence of child care during early childhood";
run;
Data pce_freqout1;
retain name label n1 pct1 ciXL1 ciXU1
n2 pct2 ciXL2 ciXU2
n3 pct3 ciXL3 ciXU3
n4 pct4 ciXL4 ciXU4;
set pce_freqout;
format pct1-pct4 ciXL1-ciXL4 ciXU1-ciXU4 percent8.2;
run;


ods excel file="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\pce_freqout2_add_neigh_safety.xlsx"; 
proc print data=pce_freqout1 noobs;
RUN;
ods excel close;

*****************************************************;
**ACE * Demographics cross-tab;
*https://support.sas.com/resources/papers/proceedings17/1482-2017.pdf;
*****************************************************;
Data control;
length ID $7. center $2;
set dat.longscan_crosssectional_dataset2;
keep id childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 center;
center=substr(ID,1,2);
run;

proc sort data=LS_all1; by id visit; run;

*ever been exposed to ACES, OCS, PCE;
proc summary data=LS_all1;
class id;
var M
MP
MS
MN
MF
ML
ME
MM
MD
ocs_neglect_beh
ocs_harsh_pv
ocs_BN_food
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless
ocs_sep
ocs_trauma_sys
ocs_trauma_court
ocs_RB_house
ocs_RB_peer
ocs_crime_victim
ocs_wit_vio
anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
neighborhood_safety_y
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare;
output out=LS_agg(keep=id 
M
MP
MS
MN
MF
ML
ME
MM
MD
ocs_neglect_beh
ocs_harsh_pv
ocs_BN_food
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless
ocs_sep
ocs_trauma_sys
ocs_trauma_court
ocs_RB_house
ocs_RB_peer
ocs_crime_victim
ocs_wit_vio
anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
neighborhood_safety_y
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare) max=;
run;

Data LS_agg;
set LS_agg;
if id ne "";
run;

proc sort data=control; by id; run;

Data LS_agg1;
merge control LS_agg;
by id;

run;

Data dat.LS_agg1_add_neigh_safety;
set LS_agg1;
run;


proc freq data=dat.LS_agg1_add_neigh_safety;

table anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
neighborhood_safety_y
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare; run;


proc contents data=LS_all1; run;

proc summary data=LS_all1;
class id;
var M
MP
MS
MN
MF
ML
ME
MM
MD
ocs_neglect_beh
ocs_harsh_pv
ocs_BN_food
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless
ocs_sep
ocs_trauma_sys
ocs_trauma_court
ocs_RB_house
ocs_RB_peer
ocs_crime_victim
ocs_wit_vio
anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
neighborhood_safety_y
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare
;
output out=LS_agg(keep=id 
M
MP
MS
MN
MF
ML
ME
MM
MD
ocs_neglect_beh
ocs_harsh_pv
ocs_BN_food
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless
ocs_sep
ocs_trauma_sys
ocs_trauma_court
ocs_RB_house
ocs_RB_peer
ocs_crime_victim
ocs_wit_vio
anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
neighborhood_safety_y
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare) max=;
run;

Data LS_agg;
set LS_agg;
if id ne "";
run;

proc sort data=control; by id; run;

Data LS_agg1;
merge control LS_agg;
by id;

run;

Data dat.LS_agg1_add_neigh_safety;
set LS_agg1;
run;