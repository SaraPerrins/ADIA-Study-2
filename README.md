# ADIA-Study-2
#Description for ADIA study 2 folder organization<br> 

1. "Data".  This folder include all data created and used for ADIA-study 2 analysis<br>
2. "SAS Program". This folder include SAS program of data management and cleaning<br>
    i."MH_SUD_outcomes.sas" create "any mental health" and "any suicidal attempt" outcomes <br>
    ii."RiskBeh_Outcome.sas" create any risk behavior outcomes <br>
    iii."PCE_correct_neigh_safety.sas" create positive childhood experiences <br>
    iv."OCS_expanded_ACE.sas" create expanded ACEs and OCS<br>
    v."Merge_long.sas" merge all demographics, expanded ACES and OCS, PCEs and outcomes to a long datafile by child ID and time<br>
    vi."Merge.sas" merge all demographics, expanded ACES and OCS, PCEs and outcomes to flat datafile by child ID<br>
    vii."agg_LS.sas" create a summary data with demographics, expanded ACES and OCS, PCEs and outcomes<br> 
3. "Analysis". Each subfolder include the analysis for each outcome with the following program codes<br>   
    i. "1.Matching.tree.r" code implement the "matching case-control" analysis as well as running "classical regression tree" analysis<br>
    ii."2.Causaltree.r" code implement the "causal regression tree analysis<br>
    iii."3.Matching.pclogit.r" code implement the Penalised conditional logistic regression analysis<br>
    iv.	"4.Bayesian clogit with horse shoe.r" code implement Bayesian clogit analysis<br>
4. "Descriptive". This folder include SAS program for the ad-hoc descriptive analysis<br>
    i."Descriptive_table.sas" create the descriptive frequency tables<br>
    ii."crosstabfreqs_add_neigh_safety.sas" create the descriptive frequency tables for PCEs "supportive environment" measures<br>
    iii."crosstabfreqs.sas" create the crosstabulation<br>
5. "R".  External function to find the balancing weights for "causal tree" analysis<br>
   -"find.weights.r"<br>  
6. "Output" folder
