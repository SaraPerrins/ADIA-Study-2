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

Data dat.childace_l;
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

proc summary data=dat.childace_l;
class ID;
var M MP MS MN MF ML ME MM MD;
output out=c_ace(drop=_type_ _freq_) sum= ;
run;
Data c_ace;
set c_ace;
if ID = '' then delete;
array var M MP MS MN MF ML ME MM MD;
do i = 1 to 9;
if var[i]=0 then var[i]=0;
if var[i]>0 then var[i]=1;
end; 
drop i;
run;
proc freq data=c_ace;
table M:;
run;

*---------------------------------------------------------------------------*;
*------------------------child demographics---------------------------------*;
*---------------------------------------------------------------------------*;
proc contents data=dat.longscan_crosssectional_dataset2 order=varnum; run;
Data control;
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
proc contents data=pce.pce_f_s; run;
proc means data=pce.pce_f_s;
var _numeric_;
run;
Data pce_f;
set pce.pce_f_s;
run;

*---------------------------------------------------------------------------*;
*---------------------------MH , dep, & SUD outcome 16 & 18-----------------*;
*---------------------------------------------------------------------------*;
Data SUDacrosstime;
set dat.SUDacrosstime;
drop center;
run;

Data MH;
set dat.MH18;
drop center visit;
run;

Data dep;
set dat.dep16_18;
drop center;
run;

*---------------------------------------------------------------------------*;
*---------------------------Risk behavior outcome 16 & 18-------------------*;
*---------------------------------------------------------------------------*;

proc contents data=dat.RBacrosstime;run;
Data RBacrosstime;
set dat.RBacrosstime;/*RB outcomes*/
keep id RB_: RB_deliq_Hi RB_vio RB_sexb /*RB_preg RB_preg_female*/
RB_HIV RB_su inj_victim PH_health future_op:;
run;

*---------------------------------------------------------------------------*;
*---------------------------OCS-------------------*;
*---------------------------------------------------------------------------*;
proc contents data=OCS.ocsacrosstime;run;
Data ocsacrosstime;
set OCS.ocsacrosstime;
drop center;
run;

*---------------------------------------------------------------------------*;
*-------------------merge demographics, outcomes, ace & ocs-----------------*;
*---------------------------------------------------------------------------*;

proc sort data=control;			by id; run;
proc sort data=childage; 		by id; run;
proc sort data=c_ace;			by id; run;
proc sort data=pce_f; 			by id; run;
proc sort data=SUDacrosstime; 	by id; run;
proc sort data=MH; 				by id; run;
proc sort data=dep; 			by id; run;
proc sort data=RBacrosstime; 	by id; run;
proc sort data=ocsacrosstime; 	by id; run;


Data LS_all;
length center $2.;
merge control childage c_ace pce_f SUDacrosstime MH dep RBacrosstime ocsacrosstime;
by id;
center=substr(ID,1,2);
label childage = 'Child DOB - 1/1/2012';
run;

proc freq data=LS_all;
table center; run;

*check pregancy variable;
proc freq data=LS_all;
table RB_preg;
run;

proc means data=LS_all;
class childsex_bsl;
var  RB_preg;
run;

Data LS_all_1;
set LS_all;
*redo RB_preg variable;
if childsex_bsl=0 then RB_preg_female = .;       /*male*/
if childsex_bsl=1 then RB_preg_female = RB_preg; /*female*/

*combine some of the outcomes;
*depression+anyMH+anySUD;
*dicotomized YMDSYMP to binary outcome for depression outcome;
if YMDSYMP <7 then dep=0;
if YMDSYMP>=7 then dep=1;

if sum(dep, anyMH, anysud) >=1 then MHout=1;
if sum(dep, anyMH, anysud) = 0 then MHout=0;
if sum(dep, anyMH, anysud) = . then MHout=.;

*sexual risk behavior+STD/HIV+Pregancy;
if sum(RB_sexb, RB_HIV, RB_preg_female) >=1 then SEXout=1;
if sum(RB_sexb, RB_HIV, RB_preg_female) = 0 then SEXout=0;
if sum(RB_sexb, RB_HIV, RB_preg_female) = . then SEXout=.;

*violence+high deliquency;
if sum(RB_vio, RB_deliq_Hi) >=1 then VIOout=1;
if sum(RB_vio, RB_deliq_Hi) = 0 then VIOout=0;
if sum(RB_vio, RB_deliq_Hi) = . then VIOout=.;

label RB_preg_f='Sexual Health - Pregnancy (female response)'
	  MHout    ='Depression+anyMH+anySUD'
      SEXout   ='Sexual risk behavior+pregancy+STD/HIV'
	  VIOout   ='Violence+high deliquency' ;
run;

proc print data=LS_all_1;
var YMDSYMP anyMH anysud MHout;
run;
proc print data=LS_all_1;
var RB_sexb RB_hiv RB_preg_female SEXout;
run;
proc print data=LS_all_1;
var RB_vio RB_deliq_hi VIOout;
run;

proc freq data=LS_all_1;
table MHout SEXout VIOout;
run;

Data dat.LS_all_c1;
set LS_all_1;
run;

Data LS_all_c1;
set dat.LS_all_c1;
run;


*output to csv;
Proc export data=dat.LS_all_c1
		outfile="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\LS_all_c1.csv"
		dbms=csv replace;
		putnames=yes;
Run;



*collapse PCE across time;
Data pceacrosstime;
set pce.pceacrosstime;
run;
Data LS_all_c1;
set dat.LS_all_c1;
run;

Data LS_all_c2;
merge LS_all_c1 pceacrosstime;
by id;
run;

proc contents data=LS_all_c2; run;
proc contents data=pce.pceacrosstime; run;

%macro anyt(v);
if sum (of &v._:)>=1 then &v=1;
if sum (of &v._:) =0 then &v=0;
if sum (of &v._:) =. then &v=.;
%mend;

Data LS_all_c2;
SET LS_all_c2;
%anyt(anysupadu);
%anyt(anysupparent);
%anyt(anysuprelative);
%anyt(anysupnonfam);
%anyt(fam_sat);
%anyt(home_safety);*;
%anyt(prrelation);
%anyt(bestfriend);*;
%anyt(socialpart);*;
%anyt(parent_involv);*;
%anyt(resid_stab)*;
%anyt(neighborhood_safety);
%anyt(neighborhood_exp);
%anyt(school_safety_y);
%anyt(school_safety_t);
%anyt(srvc_use);
%anyt(childcare);

if sum(of anysupadu, anysupparent, anysuprelative, anysupnonfam, fam_sat, home_safety, prrelation, bestfriend, socialpart, parent_involv)>=1 then soc_cap=1;
if sum(of anysupadu, anysupparent, anysuprelative, anysupnonfam, fam_sat, home_safety, prrelation, bestfriend, socialpart, parent_involv) =0 then soc_cap=0;
if sum(of anysupadu, anysupparent, anysuprelative, anysupnonfam, fam_sat, home_safety, prrelation, bestfriend, socialpart, parent_involv) =. then soc_cap=.;

if sum(of resid_stab, neighborhood_safety, neighborhood_exp, school_safety_y)>=1 then sup_ev=1;
if sum(of resid_stab, neighborhood_safety, neighborhood_exp, school_safety_y) =0 then sup_ev=0;
if sum(of resid_stab, neighborhood_safety, neighborhood_exp, school_safety_y) =. then sup_ev=.;

if sum(of srvc_use, childcare)>=1 then hp_res=1;
if sum(of srvc_use, childcare) =0 then hp_res=0;
if sum(of srvc_use, childcare) =. then hp_res=.;
run;

Data dat.LS_all_c2;
set LS_all_c2;
run;


*output to csv;
Proc export data=dat.LS_all_c2
		outfile="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\LS_all_c2.csv"
		dbms=csv replace;
		putnames=yes;
Run;


***GET % and CI for outcomes;
Data LS_all_c1;
set dat.LS_all_c1;
*combine some of the outcomes;
*depression+anyMH+anySUD;
*dicotomized YMDSYMP to binary outcome for depression outcome;
if .<YMDSYMP <7 then dep1=0;
if YMDSYMP>=7 then dep1=1;

if sum(dep1, anyMH, anysud) >=1 then MHout=1;
if sum(dep1, anyMH, anysud) = 0 then MHout=0;
if sum(dep1, anyMH, anysud) = . then MHout=.;
*dicotomized future_op_pos to binary outcome;
if .<future_op_pos <4.5 then fe=0;
if future_op_pos>=4.5 then fe=1;

run;

proc freq data=LS_all_C1; 
table VIOout
RB_deliq_hi
RB_vio
SEXout
RB_sexb
RB_preg
RB_preg_female
RB_HIV
RB_su
inj_victim
PH_Health
MHout
anyMH
dep1
anySUD
FE / binomial(level="1") alpha=0.05;
run;

PROC MEANS DATA=LS_all_C1 n mean stderr lclm uclm alpha=0.05 vardef=df;
 VAR future_op_pos; 
 OUTPUT OUT=xxtmp N=n MEAN=mean STDERR=stderr LCLM=lclm Uclm=uclm;
RUN;
