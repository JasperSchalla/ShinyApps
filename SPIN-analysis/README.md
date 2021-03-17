# SPIN-Analysis

The SPIN database cotains data about the quantity of chemical production in Denmark, Finland, Norway and Sweden in tonnes from 2000 on. The only exception is the data from Finland which begins in the year 2001. The data can be used to monitor the development of the production of different chemicals. This is especially important for chemicals which are classified or candidates for the classification as substances of very high concern (SVHC). This classification is based on the framework *REACH* which is a EU regulation for chemicals. With this app the overall production of chemicals as well as the production of SHVCs and other problematic chemicals can be monitored. 

To apply this application there are **three** different input data sets necessary (in the *example_data* folder there is example data to test the application):
1. The newest data from the SPIN database (*spin_data.txt.gz*)
2. The newest candidate-list of SVHCs to indetify the SVHCs in the spin data (*candidate-list-of-svhc-for-authorisation-export.xlsx*)
3. The authorisation-list which contains SVHCs and their trigger dates to get information on the trigger dates for the SVHCs in the spin data (*spin_data.txt.gz*)

Moreover, it can be monitored how the production has changed since these substances have been included in one of the lists. This can give indications about the effectiveness of the REACH framework. There is a list with SVHCs implemented which are considered as intermediate substances at the same time. For these substances there are different regulations since they are not existent in the final product and hence, the production of these substances is not affected. This default list can be extended as well as demagnified. To monitor other harmful substances there is a list that contains other substances which are known to be harmful implemented in the app. These substances are classified by their dangerous properties, e.g. if substances are carcinogenic. This default list can be extended as well as demagnified. The limit of the SPIN database is adressed by the calculated fraction of SVHCs included in this database since not every substance can be monitored there.

So, the main function of the app are:
- monitor the trend of the overall chemical, SVHC and other harmful chemical production
- monitor the effectiveness of the REACH framework by observing the trends after trigger dates
- provide cleaned data for further analysis

Since there is a lot of data in the database it can take some times to generate the tables and plots. When the default format of the candidate- or authorisation list or the SPIN database is changed this can cause errors. When errors occur you can contact me via email (schallajasper@gmail.com).

Here are some examples for possible plots which can be generated:

![](/SPIN-analysis/example_plots/spin_trend.png)
![](/SPIN-analysis/example_plots/spin_rel_trend.png)
![](/SPIN-analysis/example_plots/spin_prob_trend.png)
![](/SPIN-analysis/example_plots/spin_rel_prob_trend.png)
![](/SPIN-analysis/example_plots/spin_timeline.png)


