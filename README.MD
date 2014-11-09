This is a stat summary of FAER_DP_DATA_COMPLETE, a study of education outcomes comparing deliberate practice training versus more traditional technical traing in Ultrasound-Guided peripheral nerve blocks.

PI: Ankeet Udani, MD

Intervals: Pre, Post, Retension
Primary outcome: Skill test scores (AssessmentTool[0-18], Global[1-5])

####Basic Stat Description

                      Control Intervension
	n                          14           14
	Age                     29.21        28.71
	Gender.Ratio (M/F)       0.50         0.36
	AnesExperience           8.54         8.07
	%PreviousUSLines         100%         100%
	PreviousUSLinesn        21.93        25.07
	%PreviousPNB              50%          36%
	PreviousPNBn             1.64         0.79
	%PreviousObsPNB           93%          71%
	PreviousObsPNBn          6.71         4.14
	%PreviousSimPNB           21%          14%
	%PreviousDidacPNB         21%          29%
	TimeSpentinSimPrac.s.  565.02      2823.69
	TimeuntilRetention.d.   85.57       100.00

Confidence Over Time Periods

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/2.5%20confidsegments.png)

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/1%20videoscoreitems.png)

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/1%20videoscoreitems.png)

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/2%20videoscoresegments.png)

The disposition of the rentention hump. Scores decrease over time following the post-training maximum.
![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/3%20videoscoresubjects.png)


Paired t-tests between groups i and c across the 3 time points test for a difference in mean group score. Curiously, the Pre-tests positive for a difference, meaning that even before training, the control and intervention subpops were scoring differently. By the Post and Ret periods, the group scores were much more similar.

	Paired t-test data:  Ret
	t = -1.3383, df = 27, p-value = 0.1919
	alternative hypothesis: true difference in means is not equal to 0 
	95 percent confidence interval:
	 -3.2568630  0.6854344 
	sample estimates:
	mean of the differences 
	              -1.285714 

	Paired t-test data:  Post
	t = -1.3228, df = 27, p-value = 0.197
	alternative hypothesis: true difference in means is not equal to 0 
	95 percent confidence interval:
	 -2.0044337  0.4330051 
	sample estimates:
	mean of the differences 
	             -0.7857143 

	Paired t-test data:  Pre
	t = -2.2941, df = 27, p-value = 0.02979
	alternative hypothesis: true difference in means is not equal to 0 	
	95 percent confidence interval:
	 -3.5181485 -0.1961372 
	sample estimates:
	mean of the differences 
	              -1.857143 

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

