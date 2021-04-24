# R Scripts for Training and Comparing B&B Tree Size Estimates

This directory contains scripts to train ensemble estimation methods and compare them.
In addition, we provide the data used in the article.

# Usage

The central script is `create_report_table.sh`. It takes as input a directory
with SCIP log files and an (optional) output directory name (defaulting to `output`
if not specified). The output directory will be created if it does not exist.

A sample call on the heterogeneous MIPLIB and Cor@l data that we provide would look as follows:


    ./create_report_table.sh Data/heterogeneous/ output_test

The script parses the log files and the periodic estimation reports therein using awk, which it converts into a
CSV table used as input for further training and evaluation in the main R script
`process_data.R`.


The behavior of process_data.R can be further configured by providing a config.R file
with options for process_data.R.
For example, in order to produce the results from the article,
you should first create a file output_test/config.R as follows:

    mkdir -p output_test
    echo "split.stage <- TRUE" > output_test/config.R
    ./create_report_table.sh Data/heterogeneous/ output_test

Please refer to the section on Options below for further information about available options.

## Data

The log file data that was used to produce the tables and plots in the article is included under `Data/`.
The `Data` subdirectory is split into SCIP log files on heteregeneous (MIPLIB and Cor@l benchmark sets)
and homogeneous instance sets available from the MIPLIB 2017 submissions repository.
The data for the article was produced with a modified version of SCIP 6.0.

In order to produce such log file data with SCIP 7.0, it is necessary to enable periodic estimation output
and to enable the profile estimation, which is disabled by default for performance considerations.
Additionally, we disabled restarts altogether.

The necessary parameters can be passed to SCIP as a settings file with the following content:

    estimation/reportfreq = 100
    estimation/treeprofile/enabled = TRUE
    presolving/maxrestarts = 0


## Options for process_data.R

As mentioned above, process_data.R accepts a few configuration options to configure the training procedure.
They must be specified in a file `config.R` in the output directory.

The available options are as follows.


| Option          | Range                        | Default                | Description                                                                                                                                                                      |
|-----------------|------------------------------|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Random.Seed     | Integers >= 0                | 123                    | Seed for randomization                                                                                                                                                           |
| split.method    | c('prob', 'record', 'seeds') | 'prob'                 | train/test split by 'prob'lem, 'record', or nondefault 'seeds' (test on default seed)                                                                                            |
| split.stage     | c(TRUE, FALSE)               | FALSE                  | if TRUE, use three stages early/intermediate/late                                                                                                                                |
| comparison name | string                       | "random forest MIPLIB" | if the output directory contains an RDS file "other_rf_model.rds", this model is read and evaluated in addition to the trained models and appears under this name in the tables. |
