# Covid exposure mixtures

This project aims to investigate the combined effects of multiple pandemic-related policy exposures on mental health outcomes using exposure mixture modeling techniques. **The main objectives are:**

    To estimate the joint effects of COVID-19 policy exposures on depression and anxiety outcomes.
    To identify which components of the containment policies had the greatest impact on mental health outcomes.
    To compare and contrast different mixture modeling approaches, including quantile g-computation, targeted learning mixture analysis, and          
    cross-validated targeted maximum likelihood estimation.

Mixture modeling is particularly relevant to this project as it allows for the analysis of multiple exposures that are simultaneously intervened upon, better capturing real-world scenarios where correlated exposure variables generate combined health impacts. This approach will help uncover the cumulative effects of policy exposures on mental health outcomes, even when individual policies may have small effects when evaluated independently.

_Key research questions include:_

    What is the combined effect of multiple pandemic-related policy exposures on the incidence of depression and anxiety?
    Which specific policy exposures, or combinations of exposures, have the greatest impact on mental health outcomes?
    How do different mixture modeling techniques compare in terms of their ability to estimate the joint effects of policy exposures on mental health outcomes?


**Data**

The primary cohort for this analysis is the All of Us Research Program, with secondary cohorts including the MGB Biobank, UK Biobank, and ELSA Brasil. The data structure includes:

    Outcomes: Incident depression and anxiety diagnoses based on ICD-10 codes.
    Exposures: Policy exposures from the Oxford COVID-19 Government Response Tracker (OxCGRT), including school closings, workplace closings, travel restrictions, and others.
    Covariates: Individual-level covariates such as age, sex, race, ethnicity, socioeconomic status, and pre-existing mental health conditions, as well as location-level covariates.

The exposure window for this analysis is proposed to be 5 months (for the _minimum viable product_ analysis so that we can examine the effect of our exposure on a continuous outcome _(anxiety and depression severity, measured using GAD-7 and PHQ-9)_, with the outcome window following the exposure window. To handle the longitudinal nature of the policy exposures, we plan to:

    Binarize each policy exposure into "low" and "high" categories.
    Calculate a "time at high" variable that counts the number of days each policy was in the high category during the exposure window.
    Explore potential strategies for reducing the longitudinal nature of the exposures, such as binning, or taking the mean over the 
    exposure window.

By transforming the longitudinal exposures into continuous variables, we can then apply mixture modeling techniques to estimate the joint effects of these exposures on mental health outcomes.

