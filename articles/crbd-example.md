# Constant-rate Birth Death example

``` r

library(treepplr)
```

``` r

exe_path <- tp_compile("crbd", "smc-apf", particles = 5000)
data_path <- tp_data("crbd")

output_list <- tp_run(exe_path, data_path, sweeps = 4)
```

``` r

output <- tp_parse_smc(output_list)

tp_smc_convergence(output)
```
