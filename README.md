# Estimating the effect of Krasner’s tenure as District Attorney on criminal charges

The goal of this project is to estimate the effect Larry Krasner's election as Philadelphia District Attorney has had on criminal justice outcomes 
such as the number of charges for difference offense types. 

* [main.md](main.md) documents a causal inference approach to estimating the effect of Krasner's tenure as District Attorney on charges.
We use publicly available data from the Philadelphia District Attorney's Office (DAO) on charges and arrests for various offense types by date. We implement
propensity score matching to balance distributions of arrest counts between the pre and post Krasner data and fit linear models to estimate the effect 
of treatment (being charged by Krasner's DAO as compared to being charged by his predecessor). We find that the effect of Krasner's DAO was a statistically
significant decrease in charges in five of six categories of offenses, and an increase in charges for firearms offenses. We also investigate the sensitivity
of our estimates to unobserved confounders.

* In [matching.md](matching.md) we document more about our matching methodology and investigate other matching methods.

* [cleaning.md](cleaning.md) documents how we obtained and processed our data to prepare it for analysis.

In the future, I would like to extend this work to study the effect Krasner's DAO has had on other aspects of the criminal justice process such as
sentencing, diversion, and bail.
