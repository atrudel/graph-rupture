# graph-rupture

This project explores change-point detection techniques as introduced in the paper 
[gfpop: an R package for Univariate Graph-Constrained Change-Point Detection](https://arxiv.org/abs/2002.03646).  
The code is separated in two parts :
- **Python** code to explore data and format it
- **R** code to analyze the time series with graph constraints using the R package `gfpop`.

## Python
To see the sensor data visualization:
```shell
cd Python
jupyter notebook intel_exploration.ipynb
```

## R
### ECG
Every plots obtained within the scripts are available in `R/ECG/figures`.
#### Gfpop Dataset
To see the reproduction of the results given in the gfpop paper on ECG signals, 
you can run the script `R/ECG/gfpop_ECG.R`.  
You can also visualize the plot in the file
`R/ECG/figures/gfpop_ECG.pdf`.

#### Other dataset
We used the dataset *ECG Five Days* and pre-processed the data via Python notebook. The data post-processed is available in a csv file `data\ECG_5days\ptbdb_normal.csv` and the script that gave the figures of our report is `R\ECG\fivedays_ECG.R`.

### Sensor
Plot obtained within the script is available in `R/Sensors/figures`.
To see the production of the result, you can run the script `R\Sensors\Intel_sensors.R`