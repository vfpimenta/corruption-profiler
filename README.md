# Corruption-Profiler
This is a project for my Master's Degree. The objective is to profile congressman expenses and detect anomalies that could lead to a case of corruption.

The scripts used for this project are based on [Serenata de Amor](https://github.com/datasciencebr/serenata-de-amor), a project developed by [Datascience Brigade](https://datasciencebr.com/) with a similar goal.

### Upcomimg changes

1. ~~Cross reference presence and expenses~~
1. ~~Calculate silhouete of each cluster~~
1. ~~Generate dendogram of clusters~~
1. Apply different expenses on the same cluster as general
1. Normalize and apply to JS distance

1. Proportional expense of each congressman as input
1. Use other forms of cluster to divide (kmeans and such) and compute sihlluette

1. Remove congressman that don't have data (presences+expenses) for long periods (see cluster 2)
1. Analyse presences in 2014-dec vs. expenses (it seems that there is not enough presences for the expenses)
1. Check if `num_matricula` is the same for the two congressman that occupy the same seat