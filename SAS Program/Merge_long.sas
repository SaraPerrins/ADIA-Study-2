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
run;


*---------------------------------------------------------------------------*;
*--------------------------------      ACE     -----------------------------*;
*---------------------------------------------------------------------------*;
Proc sort data=dat.childace_l; by center ID visit; run;
proc contents data=dat.childace_l order=varnum; run;
proc freq data=dat.childace_l;
table visit * M: /norow nocol nopercent nocum;
run;
proc contents data=dat.child_ace order=varnum; run;

Data childace_l;
set dat.childace_l;
label M='MMCS allegation'
	MP='phy mtx'
	MS='sexual mtx'
	MN='neglect'
	MF='neglect-ftp'
	ML='neglect-los'
	ME='emotional mtx'
	MM='moral mtx'
	MD='edu mtx'
	MA='alcohol/drug' ;
run;

proc freq data=childace_l;
table visit M MP MS MN MF ML ME MM MD;
run;

*---------------------------------------------------------------------------*;
*------------------------child demographics---------------------------------*;
*---------------------------------------------------------------------------*;
proc contents data=dat.longscan_crosssectional_dataset2 order=varnum; run;
Data control;
length ID $7.;
set dat.longscan_crosssectional_dataset2;
keep id childsex_bsl childrace_bsl hh_income: caregiver_married: childrace_bsl2 /*fr_supportparent_cat1416 fr_supportoth_cat1416 comm_safety: comm_ce:*/ ;
run;
proc contents data=dat.demo order=varnum; run;
Data age;
set dat.demo;
keep id ChildDOB childage;
format ChildDOB date9.;
childage=intck('year', ChildDOB, '01Jan2012'd, 'continuous');
run;
proc sort data=age out=childage nodupkey; by id; run;
proc freq data=childage;
table childage; run;

*---------------------------------------------------------------------------*;
*---------------------------PCE Across time---------------------------------*;
*---------------------------------------------------------------------------*;
proc contents data=PCE.PCE_s; run;
proc freq data=PCE.PCE_s;
table visit time anysupadu time*anysupadu;
run;

Data pce_s;
set PCE.PCE_s;
run;

proc freq data=pce_s;
where time=4;
tables  anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
childcare; run;
*---------------------------------------------------------------------------*;
*---------------------------OCS-------------------*;
*---------------------------------------------------------------------------*;
Data ocs;
set OCS.ocs;
run;

proc contents data=OCS;run;


*---------------------------------------------------------------------------*;
*-------------------merge demographics, outcomes, ace & ocs-----------------*;
*---------------------------------------------------------------------------*;

proc sort data=childace_l; by id visit; run;
proc sort data=pce_s; by id visit; run;
proc sort data=ocs; by id visit; run;

Data M1;
merge childace_l
pce_s
ocs;
by id visit;
drop center;
run;

proc sort data=control; by id; run;
proc sort data=childage; by id; run;

Data M2;
merge M1
control
childage
;
by id;
drop time;
run;

Data LS_all;
set M2;
length center $2.;
center=substr(ID,1,2);
label childage = 'Child DOB - 1/1/2012';
if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;
run;

proc freq data=LS_all;
table center time visit visit*time /missing norow nocol nocum nopercent; run;

Data dat.LS_all_long;
set LS_all;
run;

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
school_safety_y ="Youth perception of community safety"
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


proc format;
value centerf
1="EA"
2="MW"
3="NW"
4="SO"
5="SW";
run; 

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
proc freq data=demo;
tables  center childsex_bsl childrace_bsl hh_income16_3 caregiver_married16_3 ;
run;
proc freq data=control;
tables  childsex_bsl caregiver_married16_3/ binomial(level="1") alpha=0.05;
run;

proc freq data=control;
tables hh_income16_3 / binomial (level='1') alpha=0.01666666666667;
tables hh_income16_3 / binomial (level='2') alpha=0.01666666666667;
tables hh_income16_3 / binomial (level='3') alpha=0.01666666666667;
run;

proc freq data=control;
tables childrace_bsl / binomial (level='1') alpha=0.01;
tables childrace_bsl / binomial (level='2') alpha=0.01;
tables childrace_bsl / binomial (level='3') alpha=0.01;
tables childrace_bsl / binomial (level='4') alpha=0.01;
tables childrace_bsl / binomial (level='5') alpha=0.01;
run;

proc freq data=control;
tables center / binomial (level='1') alpha=0.01;
tables center / binomial (level='2') alpha=0.01;
tables center / binomial (level='3') alpha=0.01;
tables center / binomial (level='4') alpha=0.01;
tables center / binomial (level='5') alpha=0.01;
run;


*****************************************************;
**PCE ;
*https://www.lexjansen.com/pharmasug/2009/cc/CC20.pdf;
*****************************************************;
proc contents data=LS_all1; run;
proc freq data=LS_all1;
table M MP ocs_neglect_beh;
run;
%macro freq(var);
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


%freq(anysupadu);
%freq(anysupnonfam);
%freq(anysupparent);
%freq(anysuprelative);
%freq(fam_sat);
%freq(home_safety);
%freq(neighborhood_exp);
%freq(neighborhood_safety);
%freq(parent_involv);
%freq(prrelation);
%freq(school_safety_t);
%freq(school_safety_y);
%freq(socialpart);
%freq(srvc_use);
%freq(bestfriend);
%freq(childcare);


Data pce_freq;
set anysupadu
anysupnonfam
anysupparent
anysuprelative
fam_sat
home_safety
neighborhood_exp
neighborhood_safety
parent_involv
prrelation
school_safety_t
school_safety_y
socialpart
srvc_use
bestfriend
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
if name ="anysupadu" then label='Experience of ANY supportive adult-binary';
if name ="anysupparent" then label='Experience of a supportive parent';
if name ="anysuprelative" then label='Experience of a supportive relative';
if name ="anysupnonfam" then label='Experience of a supportive non-familial adult';
if name ="fam_sat" then label='Caregiver satisfaction with family functioning-binary';
if name ="prrelation" then label="Experience of positive peer relationships-binary";
if name ="bestfriend" then label="Experience of a best friend during adolescence";
if name ="socialpart" then label="Strength of social participation in adolescence (ages 12 or 14)-binary";
if name ="home_safety" then label="Perception of safety at home-binary";
if name ="parent_involv" then label="Strength of parental involvement in youth activities during adolescence as a Protective Factor";
if name ="resid_stab" then label="Number of years the child has lived in the neighborhood (residential stability)";
if name ="neighborhood_safety" then label="Caregiver perception of neighborhood safety during adolescence";
if name ="neighborhood_exp" then label="Caregiver perception of positive neighborhood experiences during adolescence—presence of supportive adults in the community (neighborhood’s collective efficacy)";
if name ="school_safety_y" then label="Youth perception of community safety";
if name ="school_safety_t" then label="Teacher perception of school safety during early childhood";
if name ="srvc_use" then label="Access to preventative or ongoing medical service utilization In the past year";
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
ods excel file="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\pce_freqout1.xlsx"; 
proc print data=pce_freqout1 noobs;
RUN;
ods excel close;

*****************************************************;
**OCS Expanded ACES;
*****************************************************;
%freq(M);
%freq(MP);
%freq(MS);
%freq(MN);
%freq(MF);
%freq(ML);
%freq(ME);
%freq(MM);
%freq(MD);

%freq(ocs_neglect_beh);
%freq(ocs_harsh_pv);
*%freq(ocs_BN_food);
%freq(ocs_BN_cstressor);
%freq(ocs_BN_poverty);
%freq(ocs_BN_homeless);
%freq(ocs_sep);
%freq(ocs_trauma_sys);
%freq(ocs_trauma_court);
*%freq(ocs_RB_house);
%freq(ocs_RB_peer);
%freq(ocs_crime_victim);
%freq(ocs_wit_vio);



Data ace_ocs_freq;
set M
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
/*ocs_BN_food*/
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless
ocs_sep
ocs_trauma_sys
ocs_trauma_court
/*ocs_RB_house*/
ocs_RB_peer
ocs_crime_victim
ocs_wit_vio;
by time;
run;
proc sort data=ace_ocs_freq; by name time; run;
proc transpose data=ace_ocs_freq out=total prefix=n;
by name;
id time;
var total ;
run;
proc transpose data=ace_ocs_freq out=pct prefix=pct;
by name;
id time;
var  pct ;
run;
proc transpose data=ace_ocs_freq out=ciXL_BIN prefix=ciXL;
by name;
id time;
var ciXL_BIN ;
run;
proc transpose data=ace_ocs_freq out=ciXU_BIN prefix=ciXU;
by name;
id time;
var ciXU_BIN;
run;
Data ace_ocs_freqout(drop=_name_);
length label $100.;
merge total pct ciXL_BIN ciXU_BIN;
by name;
if name ="M" then label='MMCS allegation';
if name ="MP" then label='phy mtx';
if name ="MS" then label='sexual mtx';
if name ="MN" then label='neglect';
if name ="MF" then label='neglect-ftp';
if name ="ML" then label='neglect-los';
if name ="ME" then label='emotional mtx';
if name ="MM" then label='moral mtx';
if name ="MD" then label='edu mtx';
if name ="MA" then label='alcohol/drug' ;
if name ="ocs_neglect_beh" then label="Neglectful behavior";
if name ="ocs_harsh_pv" then label="Harsh Physical Violence";
if name ="ocs_BN_food" then label="Basic Need Instability – Food Insecurity";
if name ="ocs_BN_cstressor" then label="Caregiver stressors";
if name ="ocs_BN_poverty" then label="Basic Need Instability- Poverty";
if name ="ocs_BN_homeless" then label="Basic Need Instability- Homeless";
if name ="ocs_sep" then label="Traumatic separation from a loved one";
if name ="ocs_trauma_sys" then label="Contact with Potentially traumatic system";
if name ="ocs_trauma_court" then label="Contact with potentially traumatic system (court involvement)";
if name ="ocs_RB_house" then label="Risk behaviors of caregiver and household members";
if name ="ocs_RB_peer" then label="Risk behaviors of friends";
if name ="ocs_crime_victim" then label="Victim of a crime"; 
if name ="ocs_wit_vio" then label="Witnessed violence";
run;

Data ace_ocs_freqout1;
retain name label n1 pct1 ciXL1 ciXU1
n2 pct2 ciXL2 ciXU2
n3 pct3 ciXL3 ciXU3
n4 pct4 ciXL4 ciXU4;
set ace_ocs_freqout;
run;
Proc export data=ace_ocs_freqout1 outfile="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\ace_ocs_freqout2.xlsx" 
DBMS=xlsx label REPLACE;
Sheet="ace_ocs_freqout2";
RUN;

proc tabulate data=LS_all1;
class time;
class M MP MS MN MF ML ME MM MD;
table (M all MP all MS all MN all MF all ML all ME all MM all MD all) * n * f=comma9., time all;
RUN;

proc freq data=LS_all1;
table ocs_BN_cstressor * time;
RUN;


*****************************************************;
**ACE * Demographics cross-tab;
*https://support.sas.com/resources/papers/proceedings17/1482-2017.pdf;
*****************************************************;
proc format;
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



%macro colvars_TypeApproach_slim;
*class type / descending;
class M / preloadfmt order=data;
format M  YESNO.;
*where also type =: 'Sig';
%mend;
%macro columns_TypeApproach_slim;
/*type='' * approach='' /* no ALL */
M=''  /* no ALL */
%mend;
 
%macro colvars_TypeApproach_slim;
class M MP;
format M MP YESNO.;
*where also type =: 'Sig';
%mend;

proc template;
 define style sdf17_sample;
 parent=styles.htmlblue;
 style Header from _self_ / fontsize=8pt;
 style RowHeader from _self_ / fontsize=8pt;
 style Data from _self_ / fontsize=8pt;
 style DataEmphasis from _self_ / fontsize=8pt;
 end;
run;

%macro crosstab(var);
proc tabulate data=LS_all1;
class &var;
class childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 region;
table
 N * f=comma12.
 (childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 region) * colpctn=''*f=6.2
, &var
;
format &var YESNO.;
run;
%mend;


ods excel
 file='C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\Crosstab.xlsx'
 style=sgf18_sample
;
%crosstab(M);
%crosstab(MP);
%crosstab(MS);
%crosstab(MN);
%crosstab(MF);
%crosstab(ML);
%crosstab(ME);
%crosstab(MM);
%crosstab(MD);

%crosstab(ocs_neglect_beh);
%crosstab(ocs_harsh_pv);
*%crosstab(ocs_BN_food);
%crosstab(ocs_BN_cstressor);
%crosstab(ocs_BN_poverty);
%crosstab(ocs_BN_homeless);
%crosstab(ocs_sep);
%crosstab(ocs_trauma_sys);
%crosstab(ocs_trauma_court);
*%crosstab(ocs_RB_house);
%crosstab(ocs_RB_peer);
%crosstab(ocs_crime_victim);
%crosstab(ocs_wit_vio);

%crosstab(anysupadu);
%crosstab(anysupnonfam);
%crosstab(anysupparent);
%crosstab(anysuprelative);
%crosstab(fam_sat);
%crosstab(home_safety);
%crosstab(neighborhood_exp);
%crosstab(neighborhood_safety);
%crosstab(parent_involv);
%crosstab(prrelation);
%crosstab(school_safety_t);
%crosstab(school_safety_y);
%crosstab(socialpart);
%crosstab(srvc_use);
%crosstab(bestfriend);
%crosstab(childcare);

/*
proc tabulate data=LS_all1;
%colvars_TypeApproach_slim;
class childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 region;
table
 N * f=comma12.
 (childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 region) * colpctn=''*f=6.2
, %colvars_TypeApproach_slim
;
RUN;
*/
/*
proc tabulate data=LS_all;
%colvars_TypeApproach_slim;
class
 childsex_bsl caregiver_married16_3
 / style=[background=white pretext='A0A0'x]; %* white background, indented;
table
 N * f=comma12.
 childsex_bsl  * colpctn=''*f=6.2*[s=[tagattr='format:0.00%;;-_)']]
 caregiver_married16_3 * colpctn=''*f=6.2*[s=[tagattr='format:0.00%;;-_)']]
, %columns_TypeApproach_slim
/ nocellmerge;
run;

proc tabulate data=LS_all;
%colvars_TypeApproach_slim;
class
 childsex_bsl caregiver_married16_3
 / style=[background=white pretext='A0A0'x]; %* white background, indented;
table
 N * f=comma12.
  (childsex_bsl caregiver_married16_3) * colpctn=''*f=6.2*[s=[tagattr='format:0.00%;;-_)']]
, %columns_TypeApproach_slim
/ nocellmerge;
run;
*/


ODS EXCEL CLOSE;

