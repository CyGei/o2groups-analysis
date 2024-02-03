
**Sorting out assortativity: when can we assess the contributions of different population groups to epidemic transmission?**

## Structure

```
📦script
 ┣ 📂modelling
 ┃ ┣ 📜master.R
 ┃ ┗ 📜modelling_helpers.R
 ┣ 📂simulations
 ┃ ┣ 📜generate_scenarios.R
 ┃ ┣ 📜master.R
 ┃ ┣ 📜process_helpers.R
 ┃ ┗ 📜test.R
 ┗ 📂visualisations
 ┃ ┣ 📜1_AR_last_date.R
 ┃ ┣ 📜2_scenario_histograms.R
 ┃ ┣ 📜3_violin_ROC.R
 ┃ ┣ 📜4_hexbin.R
 ┃ ┣ 📜5_hexbin_all.R
 ┃ ┣ 📜7_delta_gamma_peak_coeff.R
 ┃ ┣ 📜8_delta_saturation.R
 ┃ ┣ 📜master.R
 ┃ ┗ 📜plot_helpers.R

```
## Order of execution
The code was tested on R version 4.1.3 (2022-03-10). \
*Requires the [o2groups](https://github.com/CyGei/o2groups) R package.*

1. Source `master.R` in `simulations`, it will store the results in a new `data` folder.
2. Source `master.R` in `visualisations`, it will store the figures in a new `plots` folder.
3. Source `master.R` in `modelling`, it will print the results in the console.


## License
Creative Commons Attribution (CC BY) 4.0 International License