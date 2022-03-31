# populationsim

Population simulator for Australian regions

The population simulator 'generate region ids.R' generates realistic synthetic populations of Australian regions. The simulator assigns a region's individuals to households, schools, universities, and workplaces in a way that satisfies a range of statistical constraints, such as preserving the the region's person age distribution and household type distribution. 

The method involves first specifying a region (or set of regions) for which to generate the synthetic population. This region 
can be set at any level from Statistical Area 2 (SA2) (roughly a suburb or town [2]) to the whole-of-Australia. The method then 
generates a population for each SA2 in the specified region via the following steps. 

1. Generate the SA2's inhabitants and their ages. 

2. Generate households of various types and sizes (e.g. 'one parent household, three people') based on their empirical 
frequencies for the SA2. 

3. Assign the inhabitants to the households such that each household has a realistic age structure (e.g. a sufficient age gap 
between parents and children) and the SA2's overall age distribution is preserved. 

4. Assign staff and school-aged children to nearby schools. 

5. Assign a subset of adults to universities as students. 

6. Assign a subset of working-aged individuals (the active labour force) to workplaces in the corresponding SA4, as SA4s 
represent labour markets according to the Australian Bureau of Statistics [2]. 

These steps rely on the data sources listed below. Steps 1-3 mainly rely on Census data [1]; step 3 also uses data on marriages and births to estimate couples' age gaps and the age gaps between parents and children [6,7]; step 4 uses ACARA schools data [3]; step 5 uses data on university student counts [4]; and step 5 uses Census and business counts data [5]. 

Data sources:

[1] Australian Census of Population and Housing, 2016, Australian Bureau of Statistics. 

[2] Australian Statistical Geography Standard, Australian Bureau of Statisitcs. 

[3] School Profile and School Location 2019 data sets, Australian Curriculum, Assessment and Reporting Authority. 

[4] 2016 all student load data set, Department of Education, Skills and Employment. 

[5] Counts of Australian Businesses, including Entries and Exits, June 2015 to June 2019, Australian Bureau of Statistics. 

[6] Marriages and Divorces, Australia, 2018, Australian Bureau of Statistics. 

[7] Births, 2018, Australian Bureau of Statistics. 
