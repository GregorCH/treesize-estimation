#! /bin/bash

#
# computes, combines, and visualizes estimation errors for various window sizes.
#
rm windowsizes/summary.csv
./save_windowsize_errors.R
./combine_windowsize_errors.R
./makeplots_windowsize.R
