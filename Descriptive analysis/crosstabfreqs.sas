options nofmterr;

libname dat 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';
libname RB 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\RB';
libname pce 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\PCE';
libname MH  'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\MH';
libname OCS 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\OCS';

libname out 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\crosstab';

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

data LS_agg1;
set dat.LS_agg1;
label caregiver_married16_3="Caregiver Marital Status" 
hh_income16_3="Annual Household Income"; 
run;


%macro cyclefreqs1(var=);
ods output crosstabfreqs = out.ft_&var;
ods output chisq  = out.ct_&var;
proc freq data= LS_agg1;
tables (childsex_bsl childrace_bsl caregiver_married16_3 hh_income16_3 center) * &var / chisq or;
format &var YESNO.;
format childsex_bsl sexf. childrace_bsl racef. caregiver_married16_3 YESNO. hh_income16_3 incomef. ;
run;

/* this set will be used to create proc tabulate calls 
   customized to each set of variables so
   we get "Prettier" tables
*/
data work.freqdisplaydriver;
   set out.ft_&var ;
   by notsorted table;
   where _type_ = '11';
   length rowvar colvar $ 25 rowlabel collabel $ 100;;
   rowvar = scan(table,2);
   colvar = scan(table,3);
   rowlabel = vlabelx(rowvar);
   collabel = vlabelx(colvar);
   if first.table;
   keep table rowvar colvar rowlabel collabel;
run;
/* this works because the frequency tables and chisq tables
   come from the same single proc freq call so the tabls are
   generated in the same order. Also restricting to the single
   chi-square test means the number of records used matches
*/
data OUT.fdd_&var;
   merge work.freqdisplaydriver
         OUT.ct_&var (where=(statistic='Chi-Square'));
run;
%mend;

%cyclefreqs1(var=M);
%cyclefreqs1(var=MP);
%cyclefreqs1(var=MS);
%cyclefreqs1(var=MN);
%cyclefreqs1(var=MF);
%cyclefreqs1(var=ML);
%cyclefreqs1(var=ME);
%cyclefreqs1(var=MM);
%cyclefreqs1(var=MD);

%cyclefreqs1(var=ocs_neglect_beh);
%cyclefreqs1(var=ocs_harsh_pv);
%cyclefreqs1(var=ocs_BN_food);
%cyclefreqs1(var=ocs_BN_cstressor);
%cyclefreqs1(var=ocs_BN_poverty);
%cyclefreqs1(var=ocs_BN_homeless);
%cyclefreqs1(var=ocs_sep);
%cyclefreqs1(var=ocs_trauma_sys);
%cyclefreqs1(var=ocs_trauma_court);
%cyclefreqs1(var=ocs_RB_house);
%cyclefreqs1(var=ocs_RB_peer);
%cyclefreqs1(var=ocs_crime_victim);
%cyclefreqs1(var=ocs_wit_vio);

%cyclefreqs1(var=anysupadu);
%cyclefreqs1(var=anysupnonfam);
%cyclefreqs1(var=anysupparent);
%cyclefreqs1(var=anysuprelative);
%cyclefreqs1(var=fam_sat);
%cyclefreqs1(var=home_safety);
%cyclefreqs1(var=neighborhood_exp);
%cyclefreqs1(var=neighborhood_safety);
%cyclefreqs1(var=parent_involv);
%cyclefreqs1(var=prrelation);
%cyclefreqs1(var=school_safety_t);
%cyclefreqs1(var=school_safety_y);
%cyclefreqs1(var=socialpart);
%cyclefreqs1(var=srvc_use);
%cyclefreqs1(var=bestfriend);
%cyclefreqs1(var=childcare);


%macro cyclefreqs2(var=);
title j=left font= 'Arial' c=black bold italic height=14pt underlin=1 "Crosstab Comparison of Demographic Characteristics with &var";
data work.junk;
   set OUT.fdd_&var;
   length lstr $200;
   file print;
   lstr = cat( "Title1 'Comparison of ",strip(rowlabel)," with';");
   Call execute (lstr);
   lstr = cat( "Title2 '",strip(collabel),"';");
   Call execute (lstr);
   lstr = cats("Proc tabulate data=out.ft_&var;
   where _type_='11' and table='",table,"';");
   Call execute (lstr);
   lstr = cats(catx(' ','Class',rowvar,colvar),';');
   Call execute (lstr);
   lstr = "   var frequency rowpercent ;";
   Call execute (lstr);
   lstr = catx(' ','table',rowvar,'=','"",');
   Call execute (lstr);
   lstr = catx(' ',colvar,"*(frequency='n'*sum=''*f=best5. RowPercent='Row %'*sum=''*f=5.1)");
   Call execute (lstr);
   lstr = catx(' ','/box=',rowvar,'nocellmerge;run;title;');
   Call execute (lstr);
   lstr = "Proc odstext;";
   Call execute (lstr);
   lstr = 'p "A chi-squared test was conducted to see if the distribution of responses were significantly different.";';
   Call execute (lstr);
   lstr = cat('p "The p-value for this test was ',put(prob,pvalue.),'.";');
   Call execute (lstr);
   lstr= 'run;';
   Call execute (lstr);
run;
%mend;


options missing=' ';
ods rtf file="C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output\Crosstab.rtf"
style=meadow notoc_data bodytitle startpage=no;

%cyclefreqs2(var=M);
%cyclefreqs2(var=MP);
%cyclefreqs2(var=MS);
%cyclefreqs2(var=MN);
%cyclefreqs2(var=MF);
%cyclefreqs2(var=ML);
%cyclefreqs2(var=ME);
%cyclefreqs2(var=MM);
%cyclefreqs2(var=MD);

%cyclefreqs2(var=ocs_neglect_beh);
%cyclefreqs2(var=ocs_harsh_pv);
%cyclefreqs2(var=ocs_BN_food);
%cyclefreqs2(var=ocs_BN_cstressor);
%cyclefreqs2(var=ocs_BN_poverty);
%cyclefreqs2(var=ocs_BN_homeless);
%cyclefreqs2(var=ocs_sep);
%cyclefreqs2(var=ocs_trauma_sys);
%cyclefreqs2(var=ocs_trauma_court);
%cyclefreqs2(var=ocs_RB_house);
%cyclefreqs2(var=ocs_RB_peer);
%cyclefreqs2(var=ocs_crime_victim);
%cyclefreqs2(var=ocs_wit_vio);

%cyclefreqs2(var=anysupadu);
%cyclefreqs2(var=anysupnonfam);
%cyclefreqs2(var=anysupparent);
%cyclefreqs2(var=anysuprelative);
%cyclefreqs2(var=fam_sat);
%cyclefreqs2(var=home_safety);
%cyclefreqs2(var=neighborhood_exp);
%cyclefreqs2(var=neighborhood_safety);
%cyclefreqs2(var=parent_involv);
%cyclefreqs2(var=prrelation);
%cyclefreqs2(var=school_safety_t);
%cyclefreqs2(var=school_safety_y);
%cyclefreqs2(var=socialpart);
%cyclefreqs2(var=srvc_use);
%cyclefreqs2(var=bestfriend);
%cyclefreqs2(var=childcare);

ods rtf close;
options missing='.';
