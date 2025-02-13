Code to download and format data from French National Forest Inventories from 2005 to 2024 in mediterranean forests, and examine the temporal trends of ecosystem services during this time period. 

Before running the script, make sure to install R package [targets](https://books.ropensci.org/targets/), and python packages ```requests```, ```bs4```, ```pandas```, ```time```, ```argparse```. 

To run the script, just launch R and run ```targets::tar_make()```. The script should download the remaining packages, download the data and launch the analyses. 
