# Master-Thesis
This Repository covers the R-code of my Master Thesis "How Machine Learing Enhances the Allocation of ETF Portfolios: A Comperative Performance Analysis across Industries and Sectors in the USA".

Before starting executing the R-Markdown set please read this file first:

1. Packages:\
	a) In order for the chunks to run properly all packages need to be installed first with the command "install.packages("packagename")\
	b) If you want to work (adjust the code with) with "h2o": The packages h2o and modeltime.h2o need to be installed manually (see "https://h2o-release.s3.amazonaws.com/h2o/rel-3.46.0/5/index.html" for details) and require java to be installed on the machine.\
		--> Furthermore, for the command "h2o.init(...)" to run properly, one needs to execute two commands in the console first:\
			(i) "cd C:\Users\Berger\Downloads\h2o-3.46.0.5" (path where your h2o package is installed)\
			(ii) "java -jar .\h2o.jar"\
		--> Also note that, since h2o is run by java it uses ram to compute the models. Hence, it is more efficient when the computer has more memory available. Also, the package "modeltime.h2o" is used to efficiently retrain (update) the ML-model.\

2. File-paths:\
	Please note that there are various parts of the code where file paths need to be referenced. E.g. when importing or exporting data sets or when custom functions are imported\
	--> Make sure to adjust them so they fit to your file paths!\
	--> Even though there are notes within the Rmd-file regarding this matter, keep this this in mind\

3. HTML-View:\
	a) In case you only look at the HTML-file, consider that you only might see a fraction of the full table/data sets or you it isn't displayed at all\
	--> This is, because especially if the tables are large, the HTML view would become quite messy since HTML can't Display a table with many rows and columns clearly\
	--> Hence, I strongly reccomend to also execute the Rmd-set\
	b) The "style.css"-file exists in the folder for the correct formatting of the Rmd.-file when it is knitted (transformed) into a html-file\

4. Input Parameters:\
	If you want to run the code with different Parameters, they can be adjusted at the start of chapter III

5. Comments:\
	Consider that it could be the case that in some rows information regarding the size of the (Macro) data sets might be wrong since it was adjusted some times during the process
