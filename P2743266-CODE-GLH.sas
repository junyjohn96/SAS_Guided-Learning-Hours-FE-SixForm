/********************************************************************************/
	/*Directory :
	Author: Juny Maria John									Date: 14/04/23	
	Title:	P2743266-CODE.sas							Version: 1.0		
	Email: P2743266@my365.dmu.ac.uk						Course: IMAT5168	
	Purpose:To use SAS to examine the effect of region, institution type, institution size and year on GLH per learner.  
           To conduct a suitable analysis to determine by institution type and by size, the effect of region and year on GLH per learner.																		*/

/*Input data file -----------------
IMAT5168-FE.csv
IMAT5168-6FORM.csv  
 
/* Input variables -----------------------------
Institute_Type = Institution Type (Character variable)
Region = Region (Character variable)
Total_GLH_Year_1 = Total Guided Learning Hours for Year 1 
Learners_1 = Total Learners for Year 1 
Total_GLH_Year_2 = Total Guided Learning Hours for Year 2 
Learners_2 =Total Learners for Year 2 
Total_GLH_Year_3 = Total Guided Learning Hours for Year 3 
Learners_3 =Total Learners for Year 3
*/

/*  Created variables -------------------------------------
Total_GLH = Total count of Guided Learning Hours across all years
Average_Total_GLH = Average of Total count of Guided Learning Hours across all years
Total_Learners = Total count of Learners all years
Total_GLH_Per_Learners_1 = Total Guided Learning Hours Per Learner for year 1 
Total_GLH_Per_Learners_2 = Total Guided Learning Hours Per Learner for year 2
Total_GLH_Per_Learners_3 = Total Guided Learning Hours Per Learner for year 3
Average_GLH_Per_Learner = Average of Guided Learning Hours Per Learner for all years 
Institute_Size = Institute Size (Character variable)
Year_id = Year ID
Total_GLH_Per_Learner = Transposed Guided Learning Hours per Learner
Year = Year(Not Successive years but here assumed as X = 1,2,3 )

/*  Declaring macro variables ------------------------------------------
%let cont_Total_GLH_Per_Learner = Total_GLH_Per_Learner;
%let cat_Institute = Institute_Type;
%let cat_Region = Region;
%let cat_Institute_Size = Institute_Size; 
%let cat_Year= Year;

********************************************************************************/

/*****************************************************************/
/************************  DATA PREPROCESSING  *******************/
/*** Import raw Sixform & FE Institute csv files for Analysis ****/
/*****************************************************************/

/*filelocation*/
filename FE 'H:\P2743266_AnalyticalAssignment\data\IMAT5168-FE.csv';

data sixform_original;
	infile 'H:\P2743266_AnalyticalAssignment\data\IMAT5168-6FORM.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2;

	/*customize the way that data is displayed in output*/
	format Institute_Type $32.;
	format Region $32.;
	format Total_GLH_Year_1 best8.;
	format Learners_1 best8.;
	format Total_GLH_Year_2 best8.;
	format Learners_2 best8.;
	format Total_GLH_Year_3 best8.;
	format Learners_3 best8.;

	/*specifies the column in the file that should be read*/
	input	Institute_Type	: $32.
		Region			: $32.
		Total_GLH_Year_1
		Learners_1
		Total_GLH_Year_2
		Learners_2
		Total_GLH_Year_3
		Learners_3;
run;

/*Checking the contents*/
ods exclude enginehost;
proc contents data=sixform_original order=varnum;
title 'SIXFORM- CONTENTS';
run;
ods select all;

data furthereducation_original;
	LENGTH 
		Institute_Type $32
		Region $ 32
		Total_GLH_Year_1 8
		Learners_1 8
		Total_GLH_Year_2 8
		Learners_2 8
		Total_GLH_Year_3 8
		Learners_3 8;

	FORMAT
		Institute_Type $CHAR32. 
		Region $CHAR32.
		Total_GLH_Year_1 best8. 
		Learners_1 best12. 
		Total_GLH_Year_2 best8. 
		Learners_2 best8. 
		Total_GLH_Year_3 best8. 
		Learners_3 best8.;

	INFILE FE
		lrecl=108
		encoding= "WLATIN1"
		termstr= CRLF
		dlm=','
		missover
		dsd
		firstobs=2;

	INPUT	Institute_Type	: $32.
		Region			: $32.
		Total_GLH_Year_1 : BEST8.
		Learners_1 : BEST8.
		Total_GLH_Year_2 :BEST8.
		Learners_2:BEST8.
		Total_GLH_Year_3:BEST8.
		Learners_3:BEST8.
	;
run;

/*Checking the contents*/
ods exclude enginehost;
proc contents data=furthereducation_original order=varnum;
title 'FE - CONTENTS';
run;
ods select all;


/*****************************************************************/
/************************  DATA CLEANING  ************************/
/******************** Modifying Raw CSV files ********************/
/*****************************************************************/

/*Cleaning Sixthform based on Institute_type - retaining Sixform Institute only */
data sixform;
	set sixform_original;
	if (Institute_Type ^= 'Sixth Form College') then delete;
run;

/*Printing table to check contents*/
proc print data=sixform;
title 'SIX FORM INSTITUIONS';
run;

/*Cleaning Sixthform based on Institute_type - removing Sixform Institute only */
data sixform_total;
	set sixform_original;
	if Institute_Type = 'Sixth Form College' then delete;
run;

/*Renaming column-Region and dropping blank column*/
data sixform_total_format;
	set sixform_total (rename= (Institute_Type = Region)drop=Region);
	if Region ='Total' then delete;
run;

/*Printing table to check contents*/
proc print data=sixform_total_format;
title 'SIXFORM - REGIONS DETAILS';
run;

/*Cleaning FE based on Institute_type - retaining FE Institute only */
data furthereducation;
	set furthereducation_original;
	if Region='' then delete;
run;

/*Printing table to check contents*/
proc print data=furthereducation;
title 'FE INSTITUIONS';
run;

/*Cleaning FE based on Institute_type - removing FE Institute only */
data furthereducation_total;
	set furthereducation_original;
	if Institute_Type = 'FE College' then delete;
run;

/*Renaming column-Region and dropping blank column*/
data furthereducation_total_format;
	set furthereducation_total (rename= (Institute_Type = Region)drop=Region);
run;

/*Printing table to check contents*/
proc print data=sixform_total_format;
title 'FE - REGIONS DETAILS';
run;

/*Combining datasets- Sixform and FE Insitution*/
data merged_combined_data;
	merge sixform furthereducation;
	by Institute_Type;
run;

/*Checking the contents*/
ods exclude enginehost;
proc contents data=merged_combined_data order=varnum;
title 'MERGED SIXFORM & FE - CONTENTS';
run;
ods select all;

/*Adding Institute_Size Category to the merged Sixform and FE Insitution*/
data combined_data_size;
	set merged_combined_data;

	/*Formatting columns holding GLH per learners across years and its average */
	format   Total_GLH Average_Total_GLH Total_Learners Total_GLH_Per_Learners_1 Total_GLH_Per_Learners_2 Total_GLH_Per_Learners_3 Average_GLH_Per_Learner 15.2;
	
	/*Creating new column for category- Institute Size*/
	format 'Institute_Size'n $32.;

	/*Calculations to obtain Total & Average GLH ,GLH &  Average per learners and Total Learners*/
	Total_GLH = Total_GLH_Year_1 +Total_GLH_Year_2+Total_GLH_Year_3;
	Average_Total_GLH =(Total_GLH_Year_1 +Total_GLH_Year_2+Total_GLH_Year_3)/3;
	Total_Learners = 	Learners_1 + Learners_2 + Learners_3;

	Total_GLH_Per_Learners_1 = Total_GLH_Year_1 / Learners_1;
	Total_GLH_Per_Learners_2 = Total_GLH_Year_2 / Learners_2;
	Total_GLH_Per_Learners_3 = Total_GLH_Year_3 / Learners_3;
	Average_GLH_Per_Learner =Total_GLH/Total_Learners;

	/*Comparing the Total GLH and segregating to categories*/
	if Average_Total_GLH > 3000000 then Institute_Size = "Large";
	else if Average_Total_GLH > 2000000 and Average_Total_GLH <= 3000000 then Institute_Size = "Large-medium";
	else if Average_Total_GLH > 1000000 and Average_Total_GLH <= 2000000 then Institute_Size = "Medium";
	else if Average_Total_GLH > 500000 and Average_Total_GLH <= 1000000 then Institute_Size = "Small-medium";
	else Institute_Size = "Small";
run;

/*Sorting data based on Average Total GLH*/
proc sort data=combined_data_size;
	by Average_Total_GLH;
run;

/*Printing table with Institution Size to check contents*/
proc print data=combined_data_size;
title 'SIXFORM & FE INSTITUTIONS';
run;

/*Checking the contents*/
ods exclude enginehost;
proc contents data=combined_data_size order=varnum;
title 'SIXFORM & FE INSTITUTIONS - CONTENTS';
run;
ods select all;


/*****************************************************************/
/************************  DATA VISUALIZATION  *******************/
/********** Plotting to visualize Institute_Size & Region ********/
/*****************  for Average_GLH_Per_Learner ******************/
/*****************************************************************/

ods graphics on;

/*Using pie chart to visualize different types for ease*/
PROC GCHART data=combined_data_size;
	title 'SUM OF INSTITUTE CATEGORIES - TYPE , REGION AND SIZE';
	pie Institute_Type / sumvar=Average_GLH_Per_Learner LEGEND=legend1;
    pie Region / sumvar=Average_GLH_Per_Learner LEGEND=legend1;
	pie Institute_Size/ sumvar=Average_GLH_Per_Learner LEGEND=legend1;

run;

/*Customizing the legend field */
options greplay greplayctl;
proc greplay nofs igout=work.gseg;
	options greplay greplayctl;
legend1:
	legend1 across=1 position=down value=(weight=bold size=12pt);
run;

/* disable graphics output */
ods graphics off;

/*****************************************************************/
/**** Find the cumulative sum of Guided Learning Hours (GLH)  ****/
/********* & Learners for all FE Institute in region *************/
/********************** across all years? ************************/
/*****************************************************************/

/*To obtain the sum of all the GLHs and learners*/
proc means data=furthereducation_total_format sum;
class Region;
var Total_GLH_Year_1 Total_GLH_Year_2 Total_GLH_Year_3 ;
output out=region_glh_sum_fe  sum=total_glh;
title 'CUMULATIVE SUM OF ALL GLH AND LEARNERS FOR FE INSTITUTE ACROSS YEARS';
run;

proc means data=furthereducation_total_format sum;
class Region;
var  Learners_1 Learners_2 Learners_3;
output out=region_learners_sum_fe sum=Total_Learners;
run;

/*****************************************************************/
/**** Find the cumulative sum of Guided Learning Hours (GLH)  ****/
/********* & Learners for all Sixform Institute in region ********/
/********************** across all years? ************************/
/*****************************************************************/

/*To obtain the sum of all the GLHs and learners*/
proc means data=sixform_total_format sum;
class Region;
var Total_GLH_Year_1 Total_GLH_Year_2 Total_GLH_Year_3 ;
output out=region_glh_sum_sf  sum=total_glh;
title 'CUMULATIVE SUM OF ALL GLH AND LEARNERS FOR SIXFORM INSTITUTE ACROSS YEARS';
run;

proc means data=furthereducation_total_format sum;
class Region;
var  Learners_1 Learners_2 Learners_3;
output out=region_learners_sum_sf  sum=Total_Learners;
run;


/*****************************************************************/
/*************************  ANOMALY DETECTION  *******************/
/********** Checking for missing values and outliers *************/
/**************** to cleaning data for analysis ******************/
/*****************************************************************/

/* Checking missing value based on Institution Type and region */
proc freq data=combined_data_size;
	tables Institute_Type*Region / missing;
	tables Institute_Type Region / missing;
	title 'FREQUENCY OF MISSING VALUES BASED ON INSTITUTE TYPE & REGION';
run;

/*Checking for outliers - Here using box plot */
proc sgplot data=combined_data_size;
	vbox Average_GLH_Per_Learner / category=Institute_Type group= Region;
	keylegend;
	title 'DISTRIBUTION OF GLH PER LEARNER VS INSTITUTE TYPE';
run;

/*****************************************************************/
/*********************  HANDLING MISSING VALUES  *****************/
/******** Deleting missing values but retaining outliers *********/
/*****************************************************************/

/*Cleaning columns with missing data*/
data cleaned_combined_data_size;
	set combined_data_size;
	if Total_GLH_Year_1='.' then delete;
	if Total_GLH_Year_3='.' then delete;
run;

/*Printing table with Institution Size to check contents */
proc print data=cleaned_combined_data_size;
title 'SIXFORM & FE INSTITUTIONS - CLEANED';
run;

/*Prove missing values is deleted*/
ods noproctitle;
title 'CHECK MISSING VALUES IN SIXFORM & FE INSTITUTIONS';

/*Checking missing values based on institute_type , size and region */
proc means data= cleaned_combined_data_size n nmiss min q1 median q3 max range;
	class Institute_Type Institute_Size Region;
	var Average_GLH_Per_Learner;
run;
title;
ods proctitle;


/*****************************************************************/
/***********************  FEATURE ENGINEERING  *******************/
/*************  Extracting Year from existing column  ************/
/*****************************************************************/

/*Sorting data in default setting(descending) */
proc sort data=cleaned_combined_data_size;
	by Institute_Size;
run;

/*Printing table sorted with Institution Size to check contents*/
proc print data=cleaned_combined_data_size;
title 'SORTING SIXFORM & FE INSTITUTIONS BASED ON INSTITUTE SIZE';
run;

/*Declaring year variable and assigning ID */
data year_complete_combined_data;
	set cleaned_combined_data_size;
	/*Declaring ID to variable*/
	Year_id = _n_;
run;

/*Transposing the columns of Total_GLH_Per_Learners_X to rows along with ID*/
proc transpose data= year_complete_combined_data out= Year_id_table;
	by Year_id;
	var Total_GLH_Per_Learners_1 Total_GLH_Per_Learners_2 Total_GLH_Per_Learners_3;
run;

/*Sorting table based on the Year_id and _NAME_*/
proc sort data=Year_id_table;
	by Year_id _NAME_;
run;

/*Extract numerical value from the column _NAME_ to obtain Year - Here considered as 1 2 3 */
data extract_year;
	set Year_id_table;
	*format 'Year'n $32.;
	Year = input(substr(_NAME_,24), best.);
run;

/*Removing the _NAME_ column and renaming column Total_GLH_Per_Learner */
data cleaned_year;
	set extract_year;
	rename COL1 = Total_GLH_Per_Learner;
	drop _NAME_;
run;

/*Merge the feature engineered variables to final dataset(transposed) */
data final_Institution_data;
	merge year_complete_combined_data cleaned_year;
	by Year_id;
	drop  Total_GLH_Per_Learners_1 Total_GLH_Per_Learners_2 Total_GLH_Per_Learners_3;
run;

/*Printing Final dataset for Analysis*/
proc print data=final_Institution_data;
title 'COMPLETE INSTITUTION DATASET - SIXFORM AND FE INSTITUTE';
run;

/*Checking the Final Dataset before Analysis*/
proc contents data= final_Institution_data;
run;

/*****************************************************************/
/********************  EXPLORATORY DATA ANALYSIS  ****************/
/******************** Using  Statistical Modelling  **************/
/*****************************************************************/

/*****************************************************************/
/************************ RESEARCH QUESTIONS *********************/
/*****************************************************************/

/*Declaring macro variables */
%let cont_Total_GLH_Per_Learner = Total_GLH_Per_Learner;
%let cat_Institute = Institute_Type;
%let cat_Region = Region;
%let cat_Institute_Size = Institute_Size; 
%let cat_Year= Year;

/*********************************************************************/
/*** Investigate the effect of Institute type on GLH per learner ? ***/
/****************************** TTEST & WILCOXON *********************/
/*********************************************************************/

/*Check the distribution of Institute_Type- here not normally distributed*/
/*There is some departure from qq plots for both groups however the number
	subjects is smalls so might anticipate some departure.*/
proc univariate	data = final_Institution_data;
	var &cont_Total_GLH_Per_Learner.;
	qqplot &cont_Total_GLH_Per_Learner. /normal(mu=est sigma=est);
	title 'EFFECTS OF INSTITUTE TYPE ON GLH PER LEARNER';
run;
/*Assuming hypothesis: Consider h0 = mean equal;
ha= mean different */

/*Conducting ttest for checking the significance of Institute*/
proc ttest data = final_Institution_data;
  class &cat_Institute.;
  var &cont_Total_GLH_Per_Learner.;
run;
/*Check the equality of variance: Here after examining the output 
FE College and Sixth Form College  have a 369.6 units of GHL per learners 
difference */
/*the test for equal variance  p value less than 0.05 i.e.; 
0.0001  indicating that both groups does not have equal variance so you can 
use the data for Satterwaite variance.*/

/* From Satterwaite variance :The p value is 0.0001 which indicates 
that the two groups are indeed significantly different.*/
/*Also Satterwaite,the CI doesnt contain 0 value indicating equality not satisfied*/

/*Wilcoxon-test*/ 
/*We are conducting Wilcoxon-test for confirmation since data is not normally distributed*/
/*Wilcoxon-test*/
ods graphics on; 
proc npar1way
	data = final_Institution_data
	wilcoxon
	median
	plots=(wilcoxonboxplot medianplot);
	class &cat_Institute.;
	var &cont_Total_GLH_Per_Learner.;

run;
ods graphics off;
/*From plot we can understand the mean is different*/

/*Result:We can conclude that the Institute_Type on GLH_per_learner*/


/*******************************************************************/
/*******  Does the region have an impact on GLH per learner ? ********/
/************************* ONE-WAY ANOVA ***************************/
/*******************************************************************/

/*From mean we can understand: Highest mean is for North West-Region*/
proc means data = final_Institution_data n mean std fw=5 maxdec=1 nonobs;
	class &cat_Region.;
	var &cont_Total_GLH_Per_Learner.;
	title 'EFFECTS OF REGION ON GLH PER LEARNER';
run;

/*Means plotted- This shows that North West have the highest mean and South West the lowest*/
proc sgplot
	data = final_Institution_data;
	vbox &cont_Total_GLH_Per_Learner. / group=&cat_Region.;
	xaxis min=0 max=4;
	yaxis min=0;
run;
*/Assuming the hypothesis ho:mean same, ha:mean different;
/*Finding the significance of Region on dependent variable using ANOVA*/
/*proc anova
	data = final_Institution_data;
	class &cat_Region.;
	model &cont_Total_GLH_Per_Learner.=&cat_Region.;
	means &cat_Region. /lsd;
run;*/


*To shows significance of the region on glh per learner using proc glm;
proc glm
	data = final_Institution_data;
	class &cat_Region.;
	model &cont_Total_GLH_Per_Learner.=&cat_Region.;
	*means Region /lsd hovtest=bartlett;
	means &cat_Region. /Tukey REGWQ hovtest;
	output out=final_Institution_data_region p=predicted r=residual;
run;
/*Here Pr>F= <0.0001 where it is significantly lower than 0.05 therefore h0 rejected 
region have different means.*/

/*plotting scatterplot of the region vs glh per learner*/
proc sgplot 
	data=final_Institution_data_region;
	scatter x=predicted y=residual;
	refline 0 / axis=y;
run;
/*Result : Since the p-value is less than the 
significance level (typically 0.05).We can conclude that there 
is a significant effect of region on GLH per learner.*/


/*******************************************************************/
/******* What is the trend of GLH per learner over time ? **********/
/************************* REGRESSION ******************************/
/*******************************************************************/
/*For viewing the scatter plot for GLH per learner across the (X)years.*/
proc sgplot
	data = final_Institution_data;
	scatter y=&cont_Total_GLH_Per_Learner. x=Year;
run;

/*The tests : P-parameter estimates (P), 
Pearson and Spearman correlation coefficients (R), 
confidence limits for parameter estimates (CLI), and 
confidence limits for mean response (CLM)*/

/*Trend analysis done using proc reg*/
/*Here the hypothesis is that ho: slope = 0 ; ha:slope ? 0*/
proc reg data=final_Institution_data;
  model &cont_Total_GLH_Per_Learner. = Year / p r cli clm;
  output out=trend_pred p=yhat r=resid;
  	title 'TRENDS OF GLH PER LEARNER OVER TIME';
run;

/*We check Pr>F: here it is 0.0248<0.05. 
Therefore year has significance on glh per learner*/

/* GLH =Intercept + slope*year */
proc univariate 
	data = trend_pred;
	var resid;
	qqplot /normal(mu=est sigma=est);
	histogram /normal;
run;

/*plotting graph of prediction regression line also included CI of parameter estimate and Mean response*/
proc sgplot data=trend_pred;   
  reg x=Year y=&cont_Total_GLH_Per_Learner./clm cli; 
run;                               

/*plotting histogram of residuals with normal density*/
proc sgplot data=trend_pred;
  histogram resid;            
  density resid / type=normal;  
run;
/* shows bell curve-the distribution looks okay.*/

/*plot showing prediction regression line and loess fit w.r.t GLH per learners*/
proc sgplot data=trend_pred;   *  Y vs. X plot with;
  reg x=Year y=&cont_Total_GLH_Per_Learner.;  
  loess x=Year y=&cont_Total_GLH_Per_Learner.; 
run;

/*plotting histogram of residuals vs. predicted. 
The reference line taken as e=0*/
proc sgplot data=trend_pred;
  loess x=yhat y=resid;      
  refline 0 / axis=y;        
run;

/*Results:From the regression model, we can understand that year 
has significance on glh per learner*/


/*********************************************************************/
/** What is the significance of Institute Size on GLH per learner ? **/
/************************* ONE-WAY ANOVA *****************************/
/*********************************************************************/

/*Why One-way ANOVA test:
To determine whether there is difference in mean b/w 2 or more groups.Here
we  multiple institute types (groups) and since want to determine if there
is a significant difference in the mean GLH per learner.

One-way ANOVA -  institute type would be the independent variable and 
GLH per learner would be the dependent variable.
*/

proc means data = final_Institution_data n mean std fw=5 maxdec=1 nonobs;
	class &cat_Institute_Size.;
	var &cont_Total_GLH_Per_Learner.;
	title 'EFFECTS OF INSTITUTE SIZE ON GLH PER LEARNER';
run;


/*Can use proc Anova for determining but I would focus on glm proc in ANOVA for now*/
/*proc anova
	data = final_Institution_data;
	class &cat_Institute_Size.;
	model &cont_Total_GLH_Per_Learner.=&cat_Institute_Size.;
	means &cat_Institute_Size. /lsd;
run;*/

/*From plot we can view the mean difference value*/
proc sgplot
	data = final_Institution_data;
	vbox &cont_Total_GLH_Per_Learner. / group=&cat_Institute_Size.;
	xaxis min=0 max=4;
	yaxis min=0;
		title 'EFFECTS OF INSTITUTE SIZE ON GLH PER LEARNER';
run;

/*proc glm in ANOVA used to shows significance of the Institute_Size on glh per learner */
proc glm
	data = final_Institution_data;
	class &cat_Institute_Size.;
	model &cont_Total_GLH_Per_Learner.=&cat_Institute_Size.;
	*means Institute_Size /lsd hovtest=bartlett;
	means &cat_Institute_Size. /Tukey REGWQ hovtest;
	output out=final_Institution_data_s p=predicted r=residual;

run;
/*Here considering hypothesis: H0= mean same;HA- mean different*/
/*Here using Tukey& REGWQ- since presentmultiple comparison is required.*/
/*The p-value associated with the F-test considered. Here Pr>F = .0001;
i.e p<0.05,null hypothesis is reject and alternate hypothesis accepted.*/

/*Results:There is a significant effect of institute size on GLH per learner.*/


/********************************************************************/
/****** Is Institution_Type & Institution_Size Interrelated ? *******/
/************ Does it together affect GLH Per Learner ? *************/
/************************* TWO-WAY ANOVA ****************************/
/********************************************************************/

/*Why Two-way ANOVA test?
For understanding the main effects of each independent variable as well 
as their interaction effect on the dependent variable-GLH per learner*/

/* calculating the data needed for the profile plot */
proc sort data=final_Institution_data;
	by  Institute_Type Institute_Size;  
run;
proc print data=final_Institution_data;
title 'EFFECTS OF INSTITUTION_TYPE & INSTITUTION_SIZE ON GLH PER LEARNER';
run;

/*obtaining the mean of Institute_Type and Institute_Size*/
proc means 
	data= final_Institution_data;
	var &cont_Total_GLH_Per_Learner.;
	by &cat_Institute. &cat_Institute_Size.;
	output out=means mean=mean;
run;

/* create a profile plot */
/*The overlapping produced from graph shows that the 2 independent variables have interaction*/
proc gplot 
	data=means;
	plot mean*&cat_Institute.=&cat_Institute_Size./ vaxis = 100 to 550 by 50;
    title 'THE INTERACTION OF INSTITUTE TYPE & ';
    symbol i=join;
run;

/*Proc glm using interaction term*/
proc glm 
	data= final_Institution_data;
	class &cat_Institute. &cat_Institute_Size.;
	model &cont_Total_GLH_Per_Learner.=&cat_Institute. &cat_Institute_Size. Institute_Type*Institute_Size; /* includes interaction */
	lsmeans &cat_Institute. &cat_Institute_Size. &cat_Institute.*&cat_Institute_Size.;
	output out=interact p=predicted r=residual;
run;
/*For checking the interaction term:
Look for Institute_type*Institute_size which is 0.0028 which is <0.05- Therefore concluded 
There is interaction.*/

/* if you use proc glm alone, the program does not terminate so use 'quit;' here */
proc univariate 
	data=nointeract;
	var residual;
	histogram /normal;
	qqplot /normal(mu=est sigma=est);
run;

/*Results: From test, we unerstand that the Institute type and size have an interaction
and they together have an effect on the GLH per learner*/

/********************************************************************/
/******************** Is Region & Year Interrelated ? ***************/
/************ Does it together affect GLH Per Learner ? *************/
/************************* TWO-WAY ANOVA ****************************/
/********************************************************************/

proc sort data=final_Institution_data;
	by  Region Year;
run;
proc print data=final_Institution_data;
title 'EFFECTS OF REGION & YEAR ON GLH PER LEARNER';
run;

/*Here will obtain the mean of each region w.r.t year*/
proc means 
	data= final_Institution_data;
	var &cont_Total_GLH_Per_Learner.;
	by &cat_Region. &cat_Year.;
	output out=means mean=mean;
run;

/*Inorder to create a profile plot */
/*From plot, can understand 2nd and 3rd year have interaction*/
/*This validated in regression test for year*/
proc gplot 
	data=means;
	plot mean*&cat_Region.=&cat_Year.
		/ 	vaxis = 100 to 550 by 50
	;
	symbol i=join;
run;

/*proc glm using interaction term*/
proc glm 
	data= final_Institution_data;
	class &cat_Region. &cat_Year.;
	model &cont_Total_GLH_Per_Learner.=&cat_Region. &cat_Year. &cat_Region.*Year;
	lsmeans &cat_Region. &cat_Year. &cat_Region.*&cat_Year.;
	output out=interact p=predicted r=residual;
run;
/*For checking the interaction b/w Region*Year- if p>0.05- there is no interaction*/
/*Since Region*Year p value is 1.000; we need not include the interaction term*/
/*Therefore modelling done without interaction term.*/

/*proc glm without including interaction term*/
proc glm 
	data= final_Institution_data;
	class &cat_Region. &cat_Year.;
	model &cont_Total_GLH_Per_Learner.=&cat_Region. &cat_Year.; 
	means &cat_Region. /lsd;
	means &cat_Year. /lsd;
	output out=nointeract p=predicted r=residual;
run;
/*Now checking the influence of Year and Region seperately*/
/*For Region:p is .0001 which is much less than 0.05 - evidence of strong influence on GLH Per Learner*/
/*For Year: p is 0.0283 which is much higher than Region's p Therefore much lesser influence on GLH PEr Learner*/

/* if you use proc glm alone, the program does not terminate so use 'quit;' here */
proc univariate 
	data=nointeract plot normal;
	var residual;
	histogram /normal;
	qqplot /normal(mu=est sigma=est);
run;
/*Result : We can conclude- The Region and Year together does not affect GLH Per Learner.
The Region has a stronger impact on GLH Per Learner as compared to the year*/