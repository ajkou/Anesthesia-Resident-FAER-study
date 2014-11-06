This is a stat summary of FAER_DP_DATA_COMPLETE, a study of education outcomes comparing deliberate practice training versus more traditional technical traing in Ultrasound-Guided peripheral nerve blocks.
PI: Ankeet Udani

Intervals: Pre, Post, Retension
Primary outcome: Skill test scores (AssessmentTool[0-18], Global[1-5])

####Basic Stat Description

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/1%20videoscoreitems.png)

![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/2%20videoscoresegments.png)

The disposition of the rentention hump. Scores decrease over time following the post-training maximum.
![alt tag](https://raw.githubusercontent.com/ajkou/Anesthesia-Resident-FAER-Study/master/3%20videoscoresubjects.png)


Paired t-tests between groups i and c across the 3 time points test for a difference in mean group score. Curiously, the Pre-tests positive for a difference, meaning that even before training, the control and intervention subpops were scoring differently. By the Post and Ret periods, the group scores were much more similar.

	Paired t-test data:  Ret
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
