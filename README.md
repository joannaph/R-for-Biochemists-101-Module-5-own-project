Here is my own project as part of Module 5: R for Biochemists 101 - Biochemical Society Programming Skills Summer Vacation Studentship

I have uploaded data from a wet lab practical that my cohort completed as part of BIOC0036: Practical and Computational Biomolecular Structure and Function at UCL. Here, we were tasked with determining the optimum temperature for the enzyme beta-galactosidase, taking into account time. An increase in temperature causes an increase in turnover rate, however, can simultaneously increase the rate of enzyme denaturation, which in turn, reducing the rate of reaction over time. This is most evident when each reusable enzyme is utilised for the catalysis of multiple identical reactions, thus the maintenance of 3˚ structure is essential for function. Therefore, the purpose of this investigation was to determine the optimum kinetic temperature beta-galactosidase, avoiding irreversible enzyme denaturation exhibited at high temperatures, providing evidence that the each molecule of enzyme catalyses multiple reactions. 

In the experiment, the enzyme activity of beta-galactosidase was measured  through the hydrolysis of onitrophenolyl-beta-D-galactopyranoside (ONPG) to produce o-nitrophenolate (ONP, absorbing visible light at 420nm). The absorbance of produced ONP was determined at 1, 2, 5, and 10 minutes at varying temperatures.
The rate of reaction was determined through a reaction progress chart (Figure 1) and the calculation of the activation energy of reaction by an Arrhenius plot (Figure 2).
![image](https://github.com/user-attachments/assets/e3ad4d8e-beaf-46a5-ad24-9211507a47a0) **Figure 1** - Reaction Progress Chart. Linear relationship between time and absorbance at 22, 35, 45 & 55 ˚C. No linearity observed at 65 & 75˚C

![image](https://github.com/user-attachments/assets/18b69717-a1ff-4c2b-815b-162a5da5d964) **Figure 2** - Arrhenius Plot, where Ea = 27.32 kJ/mol

Our results showed that the rate of beta-galactosidase activity increases between temperatures 22-55 ̊C, showing a linear relationship between absorbance (amount of ONP) and time. This relationship is not seen at temperatures 65 ̊C & 75 ̊C, thus suggesting that the optimum enzyme temperature is in the range 45-55 ̊C. This is accurate as known data of bacterial beta- galactosidase suggests an optimum temperature of 40-65 ̊C, species dependent[1]. This demonstrates the significance of the compromise between temperature and time, a lower temperature allowing for greater product accumulation over time, due to the maintenance of beta-galactosidase function. 
Furthermore, the activation energy calculated from the Arrhenius plot is almost consistent with the known activation energy of beta-galactosidase hydrolysis of ONPG from literature: 27.32 kJ/mol in comparison to the actual 32.88 kJ/mol [2]. My calculation of Ea is likely to be an underestimate, as I include data from 22˚C in the calulation, despite its considerable distance away from the actual optimum temperature. My calculation of the Ea of ONPG hydrolysis by beta-galactosidase suggests accuracy of my estimate of optimum temperature.   


When I initially submitted the analysis for this practical, I used Excel to generate figures, calculate rates of reaction and the Ea. Upon completion of R for Biochemists 101, I re-analysed the data using R, and below are the reasons as to why R proved more useful than Excel:

1) **I was given a large volume of raw data.** The results of every experiment undertaken by every student in the cohort was added to one large spreadsheet, and we were tasked with analysis of _all_ of the data. This proved tedious and fiddly within the Excel interface, but much easier using R. Albeit, I did need to manually create many objects and data frames (I tried to create a loop to avoid this -- it did not work, due to the nature of the formatting of the raw data by the lab technitions), but I found it to be much easier to handle the data in R compared to Excel.
2) **R allowed me to plot more than Excel.** By using R, I was able to include the Standard Error of Mean (SEM) as error bars on my Reaction Progress Chart. I did this by creating a formula to calculate SEM, and adding it to the ggplot, using geom_errorbar(). This proved difficult to do in Excel, as I was not able to include the SEM on the plot itself, rather, in a table to the side.
3) **R gave me confidence in my results.** When I calculated linear regression of absorbance vs. time for temperatures 22, 35, 45, 55, 65, & 75˚C, R was able to tell me the statistical significance of these automatically. Although I could see by eye that there was no linear relationship at 65 & 75 ˚C, it was useful to use the summary() function to show for certain that a linear model was not valid at the highest temperatures. Potentially, this is possible to do in Excel, but it would not have been as straightforward as in R.

In the attached files is my code and the raw data as an Excel spreadsheet. 



**References:**
1. Saqib, S., Akram, A., Halim, S. A., & Tassaduq, R. (2017). Sources of β-galactosidase and its applications in food industry. 3 Biotech, 7(1), 79. https://doi.org/10.1007/s13205-017-0645-5
2. Raol, G. G., Raol, B. V., Prajapati, V. S., & Patel, K. C. (2015). Kinetic and thermodynamic characterization of a halotolerant β-galactosidase produced by halotolerant Aspergillus tubingensis GR1. Journal of basic microbiology, 55(7), 879–889. https://doi.org/10.1002/jobm.201400747
