This is a stat summary of FAER_DP_DATA_COMPLETE, a study of education outcomes comparing deliberate practice training versus more traditional technical traing in Ultrasound-Guided peripheral nerve blocks.

PI: Ankeet Udani, MD

Intervals: Pre, Post, Retension
Primary outcome: Skill test scores (AssessmentTool[0-18], Global[1-5])

####Basic Stat Description

	                   	Control            	Intervention			test p
	n                    	14                 	14 	
	Age                  	29.21 [26-35]       	28.71 [27-31]			0.48
	Gender (M/F)         	0.5                 	0.36 	        			0.7
	AnesExperience       	8.54 [4-20]         	8.07 [3-19]   			0.82
	PreviousUSLines	    	100%               	100%            		1
	PreviousUSLinesn    	21.93 [ 5-100]      	25.07 [ 1-100]			0.78
	PreviousPNB          	50%                	36%           			0.7
	PreviousPNBn         	1.64 [0-5]	          	0.79 [0-5]   			0.22
	PreviousObsPNB      	93%                 	71%          			0.32
	PreviousObsPNBn     	6.71 [0-20]         	4.14 [0-15]  			0.17
	PreviousSimPNB       	21%                	14%             		1
	PreviousDidacPNB     	21%                 	29%          			0.68
	TimeSpentinSimPrac.s.	565.02 [0-1507]     	2823.69 [1607-4032]		<0.001
	TimeuntilRetention.d.	85.57 [42-116]      	100 [52-133] 			0.08

Confidence Over Time Periods

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/2.5%20confidsegments.png)

Testing Confidence over [Pre,Post,Ret] and across [i,c]. I chose to evauluate the PNBConfid scores as interval data [1-4] and compare their proportions by chi square test. This  indicated no difference in confidence statements in any of the 3 survey points between intervention and control. The confidence level can be said to be largely the same between the control and DP intervension. (p-value=NA basically means p-value=1)

        Pearson's Chi-squared test: Pre
	X-squared = NaN, df = 3, p-value = NA

        Pearson's Chi-squared test: Post
	X-squared = 2.0294, df = 3, p-value = 0.5663

        Pearson's Chi-squared test: Ret
	X-squared = NaN, df = 3, p-value = NA

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/1%20videoscoreitems.png)

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/2%20videoscoresegments.png)

Below is the disposition of the rentention hump, which is seen in all the scoring metrics in this study. Scores decrease over time following the post-training maximum.
![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/3%20videoscoresubjects.png)

Unpaired t-tests between groups i and c across the 3 time points test for a difference in mean checklist score. Curiously, the Pre-tests positive for a difference, meaning that even before training, the control and intervention subpops were scoring differently. By the Post and Ret periods, the group scores were much more similar, and although not testing significant, the p-value indicates a possible differentiation.

        Welch Two Sample t-test: Pre
	t = -2.0912, df = 53.224, p-value = 0.04129
	alternative hypothesis: true difference in means is not equal to 0 
	95 percent confidence interval:
	 -3.63818254 -0.07610318 
	sample estimates:
	mean of x mean of y 
 	4.821429  6.678571 

        Welch Two Sample t-test: Post
	t = -1.2552, df = 51.754, p-value = 0.215
	alternative hypothesis: true difference in means is not equal to 0 
	95 percent confidence interval:
	 -2.0419244  0.4704958 
	sample estimates:
	mean of x mean of y 
	 14.96429  15.75000

        Welch Two Sample t-test: Ret
	t = -1.4856, df = 43.407, p-value = 0.1446
	alternative hypothesis: true difference in means is not equal to 0 
	95 percent confidence interval:
	 -3.0305375  0.4591089 
	sample estimates:
	mean of x mean of y 
	 11.96429  13.25000 

Paired t-tests for change scores between groups i and c across the 3 time points test for a difference in improvement. All 3 tests did not show difference in that improvement.

		Paired t-test: Difference between Post and Pre
	t = 1.2384, df = 27, p-value = 0.2262
	alternative hypothesis: true difference in means is not equal to 0
	95 percent confidence interval:
	 -0.7037171  2.8465742
	sample estimates:
	mean of the differences 
               	1.071429 

		Paired t-test: Difference between Ret and Post
	t = -0.5857, df = 27, p-value = 0.563
	alternative hypothesis: true difference in means is not equal to 0
	95 percent confidence interval:
	 -2.251687  1.251687
	sample estimates:
	mean of the differences 
                   	-0.5 

		Paired t-test: Difference between Ret and Pre
	t = 0.5144, df = 27, p-value = 0.6111
	alternative hypothesis: true difference in means is not equal to 0
	95 percent confidence interval:
	 -1.707801  2.850659
	sample estimates:
	mean of the differences 
              	0.5714286 
The 2-group tests using the global score produced a similar result. I chose to evauluate the Global scores as interval data [1-5] and compare their proportions using a 5x2 chi square test. The results of this test using the global score are the same as the video checklist results.

        Pearson's Chi-squared test: Pre 
	X-squared = 6.0171, df = 2, p-value = 0.04936

        Pearson's Chi-squared test: Post
	X-squared = 3.2976, df = 3, p-value = 0.348

        Pearson's Chi-squared test: Ret
	X-squared = 5.5603, df = 4, p-value = 0.2345

Scattermatrix visuals describe the general situation found in the data. We can visually scan for inferences and test for them later.

Take a look below at the relationship between Post confidence and the 3 test scores. The pre-test score plot shows a cresent relationship. Although there's a lot of noise, this suggests that the students with the lowest confidence could score very high or very low, the extremes of the dimension. Is this a signature of a prevalent psychological trick? It's possible. Nervious students concerned with taking a blind pre-test aligned performance and personal confidence much better after training, and even better by retention.

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/7%20scattermatrix-scoresvsconf.png)

The next scattermatrix depicts how the prior experience affects the student's test results. The following relationships are observed:

The higher your score on any test, the higher your score on any other test.

The more PNB procedures you've performed in the past, the less time you chose to take during instruction.

The more PNB procedures you've observed in the past, the less time you chose to take during instruction.

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/5%20scattermatrix-timevsexp.png)

Instead of measuring the test scores themselves, what about the difference in score from test to test? How do the cofactors affect the differences in scores? 

First, The more you improved at the Post test, the less you improved by the Ret test. The majority of residents lost points at the Ret test. Residents that had a high Pre score experienced the highest decrement.

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/6%20scattermatrix-obsvsconf.png)


####The importance of the confidence score at t=Post.

This study asks the student how confident they felt at 3 ocassions[Pre, Post, Ret]. Can this measure of student's intuition predict his/her own performance or improvement in performance?

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/4%20scattermatrix-confid.png)

ICC (intra-class correlation) is a measure of interrater reliability. An ICC of 1 basically indicates perfect agreement and 0 means no agreement. The following features apply to using ICC in this situation. 

	1) A fixed set of 2 judges makes the variation created by reviewers a fixed-effect.
	2) Both judges visited every subject exactly 3 times (but was blinded). 
	3) We are specifically interested in the level of agreement between the judges and don't really care about judgement on this 18-point scale in general.

Thus the version of ICC we are dealing with is the "Two-way mixed single measures (Consistency/Absolute agreement)" (SPSS) or the "ICC(3,1)" (Shrout and Fleiss)
scores the similarity of the 2 raters based on how similarly they score on a case-by-case basis.


Which ICC type should we use?
	Documentation on the R ICC function says,

    "Shrout and Fleiss (1979) consider six cases of reliability of ratings done by k raters on n targets.
    ICC1: Each target is rated by a different judge and the judges are selected at random. (This is a one-way ANOVA fixed effects model and is found by (MSB- MSW)/(MSB+ (nr-1)*MSW))
    ICC2: A random sample of k judges rate each target. The measure is one of absolute agreement in the ratings. Found as (MSB- MSE)/(MSB + (nr-1)*MSE + nr*(MSJ-MSE)/nc)
    ICC3: A fixed set of k judges rate each target. There is no generalization to a larger population of judges. (MSB - MSE)/(MSB+ (nr-1)*MSE)
    Then, for each of these cases, is reliability to be estimated for a single rating or for the average of k ratings? (The 1 rating case is equivalent to the average intercorrelation, the k rating case to the Spearman Brown adjusted reliability.)
    ICC1 is sensitive to differences in means between raters and is a measure of absolute agreement.
    ICC2 and ICC3 remove mean differences between judges, but are sensitive to interactions of raters by judges. The difference between ICC2 and ICC3 is whether raters are seen as fixed or random effects.
    ICC1k, ICC2k, ICC3K reflect the means of k raters."

	All Video Scoring
	type  ICC  F df1 df2 p lower bound upper bound
	Single_fixed_raters      ICC3 0.90 19  83  83 0        0.85        0.93

	Pre Scores
	type  ICC F df1 df2       p lower bound upper bound
	Single_fixed_raters      ICC3 0.78 8  27  27 3.5e-07        0.57        0.89

	Post Scores
	type  ICC   F df1 df2       p lower bound upper bound
	Single_fixed_raters      ICC3 0.70 5.7  27  27 1.1e-05        0.45        0.85

	Ret Scores
	type  ICC   F df1 df2       p lower bound upper bound
	Single_fixed_raters      ICC3 0.71 6.0  27  27 7.3e-06        0.47        0.86

	Global Scores
	type  ICC   F df1 df2 p lower bound upper bound
	Single_fixed_raters      ICC3 0.78 8.0  83  83 0        0.68        0.85

### Formal tests on Confidence Score groups

Tukey's Range test on confidence

What about evaulating checklistscore by both Trial[Pre,Post,Ret] and Group[i,c]? 2-way anova detects the clear difference seen earlier in Trial, but not Group. Within the Trials, Pre is distinguished from the other evaluations at Post and Ret. There is a plausible (p=0.14) difference between Post and Ret, but not a stat significant one.

	> summary(fm1)
             	Df Sum Sq Mean Sq F value  Pr(>F)    
	survey$Group  1  0.298   0.298   0.933   0.337    
	survey$Trial  2 13.738   6.869  21.530 3.3e-08 ***
	Residuals    80 25.524   0.319                    
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

  	Tukey multiple comparisons of means
    	95% family-wise confidence level
    	factor levels have been ordered
	
	Fit: aov(formula = dh ~ survey$Group + survey$Trial)
	
	$`survey$Group`
         diff        lwr       upr     p adj
	i-c 0.1190476 -0.1262453 0.3643405 0.3370378
	
	$`survey$Trial`
	              diff         lwr      upr     p adj
	Ret-Pre  0.6785714  0.31806173 1.039081 0.0000684
	Post-Pre 0.9642857  0.60377602 1.324795 0.0000000
	Post-Ret 0.2857143 -0.07479541 0.646224 0.1474283


RMANOVA does a triplet comparison of the confidence levels at the 3 time points [Pre,Post,Ret], segregating subject level variance.

	> summary(aov(dh~ (survey$Trial+survey$Group)+Error(survey$Participant)))

	Error: survey$Participant
	             Df Sum Sq Mean Sq F value Pr(>F)
	survey$Group  1  0.298  0.2976   0.466  0.501
	Residuals    26 16.595  0.6383               
	
	Error: Within
	             Df Sum Sq Mean Sq F value   Pr(>F)    
	survey$Trial  2 13.738   6.869   41.54 1.19e-11 ***
	Residuals    54  8.929   0.165                     
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

The friedman rank test also tests for group differences [Pre,Post,Ret] and gives the same result.

	> friedman.test(survey$PNBConfid,survey$Trial,survey$Participant)
	
        	Friedman rank sum test

	data:  survey$PNBConfid, survey$Trial and survey$Participant 
	Friedman chi-squared = 35.0909, df = 2, p-value = 2.399e-08

Synopsis of Confidence Outcome: Confidence was measured on all 28 subjects on a 1-4 ordinal scale of [SD,D,A,SA]. Chi-squared tests comparing the proportions of these answers found no substantial difference between intervention and control. TukeyHSD (Tukey's range test) and RMANOVA for paired data both determined there was a difference in PNBConfid between the assessment times [Pre, Post, Ret]. Tukey's test indicates that although there is a possible difference (p-value=0.14), the disparity is not significant. These findings are corroborated visually in the graph "2.5 confidsegments.png".

###Comparing checklist score and global score

Comparing 2 scales can be done by converting data to ranks, rescaling, or using the correlation coeff. Spearman rank correlation is appropriate to use here, as the data is best described here as ordinal data without meaningfull intervals (not interval data).

	> cor(video$TotalChecklistScore, video$Globalscore, method="pearson")
	[1] 0.8733169
	
	> cor(video$TotalChecklistScore, video$Globalscore, method="spearman")
	[1] 0.8837453