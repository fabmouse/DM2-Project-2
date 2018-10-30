%macro newboot2(NumberOfLoops, DataSet, XVariable, YVariable);

/* Sample the data */
       sasfile &DataSet load;

       proc surveyselect data=&DataSet out=Bootdata3 seed=-23434 noprint
       method=urs samprate=1 outhits rep=&NumberOfLoops;
       run;

       sasfile &DataSet close;

/*Get the parameter estimates*/
       proc reg data=Bootdata3 outest=ParameterEstimates3(drop=_:) noprint;
       model &YVariable=&XVariable;
       by replicate;
       run;

/*Extract just the columns for slope and intercept for storage*/
       data Resultholder3;
       set ParameterEstimates3;
       keep Intercept &XVariable;
       run;

/*Calculate the means of the X variable in each bootstrap*/
       proc univariate data=Bootdata3 noprint;
       var &XVariable;
       by replicate;
       output out=uniOut mean=mean&XVariable;
       run;

/*Calculate 95% confidence intervals for the mean of X variable*/
/*       proc univariate data=uniOut;*/
/*       var mean&XVariable;*/
/*       output out=BootCI pctlpts=2.5, 97.5 pctlpre=CI; */
/*       run;*/

/*Calculate 95% confidence intervals for the mean of intercept parameter*/
       proc univariate data=Resultholder3;
       var Intercept;
       output out=InterceptCI pctlpts=2.5, 97.5 pctlpre=CI; 
       run;
       
/*Calculate 95% confidence intervals for the mean of X variable parameter*/
       proc univariate data=Resultholder3;
       var &XVariable;
       output out=XvarCI pctlpts=2.5, 97.5 pctlpre=CI; 
       run;

/*Produces the means of the parameters from the results of the bootstrap*/
/*       proc means data=Resultholder3;*/
/*       run;*/

/*Produce chart of parameters - commented out for now*/
/*       proc univariate data=Resultholder3;*/
/*       histogram;*/ 
/*       run;*/

/*Create RTF file*/
       ods rtf file="output.rtf" startpage = never;
              title = "SAS Output";
              
              /*Results for intercept parameter*/
              proc print data=InterceptCI;
              run;
              proc gchart data=Resultholder3;
              vbar Intercept;
              run;
              
              /*Results for slope parameter*/
              ods startpage = now; /*Insert a page break for new results*/
              proc print data=XvarCI;
              run;
              proc gchart data=Resultholder3;
              vbar &XVariable;
              run;
       ods rtf close;

%MEND newboot2;

/*Importing the fitness data set*/
proc import out = Asmt2.fitness 
            datafile = "C:\Users\ll99\Desktop\ASMT 2\fitness.csv" 
            dbms = CSV REPLACE;
     getnames = YES;
     datarow = 2; 
run;

/*Timing the macro*/
/*Remember to reference this code*/
%let _sdtm=%sysfunc(datetime());
/*Run the macro*/
%newboot2(NumberOfLoops=, DataSet=, XVariable=, YVariable=);
%let _edtm=%sysfunc(datetime());
%let _runtm=%sysfunc(putn(&_edtm - &_sdtm, 12.4));
%put It took &_runtm seconds to run the program;

ods rtf close;
