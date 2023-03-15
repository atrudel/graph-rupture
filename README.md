# graph-rupture

This project explores change-point detection techniques as introduced in the paper 
[gfpop: an R package for Univariate Graph-Constrained Change-Point Detection](https://arxiv.org/abs/2002.03646).  
The code is separated in two parts :
- **Python** code to explore data and format it
- **R** code to analyze the time series with graph constraints using the R package `gfpop`.

## Python
To see the ECG data visualization:
```shell
cd Python
jupyter notebook ECG_exploration.ipynb
```

## R

### ECG
#### Gfpop Dataset
To see the reproduction of the results given in the gfpop paper on ECG signals, 
you can run the script ``R/ECG/gfpop_ECG.R``.  
You can also visualize the plot in the file
``R/ECG/gfpop_ECG.pdf``  
**Done:**
- [x] Reproduce the code from the gfpop repository
- [x] Adapt the code to remove the comparison with the PanTomkins model

####  Kaggle Dataset
[Work in progress]  
We can now display the signal in R.  
**Todo:**
- [x] Extract one signal of the dataset and display it
- [x] Reproduce the constraint graph and adapt deltas
- [ ] Run the analysis on many time series
- [ ] Evaluate qualitatively a few examples

#### QRS dataset
TODO: it would be good to find an annotated QRS dataset for ECG.