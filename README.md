# Corruption-Profiler
This is a project for my Master's Degree. The objective is to profile congressman expenses and detect anomalies that could lead to a case of corruption.

The scripts used for this project are based on [Serenata de Amor](https://github.com/datasciencebr/serenata-de-amor), a project developed by [Datascience Brigade](https://datasciencebr.com/) with a similar goal.

### Upcomimg graph changes

1. ~~Update colors to better represent the data~~
1. Run analysis over histogram and proportions of each subgroup
  * Evaluate different input data results (subquota, proportion, pdf)
  * Evaluate othe forms of presentation (boxplot, histogram)
1. Use the JS distance on the calculation instead of pearson/spearman
  * Consider using the robust distance (evaluate what is best)
  * Tinker with the K size to see the different groups
1. Use KNN clicks to better detect subgroups relation