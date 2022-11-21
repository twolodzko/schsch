
The code example comes from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine -m 100 --warmup 1 'scheme --quiet < run-all.scm' '../../main run-all.scm'  
Benchmark 1: scheme --quiet < run-all.scm
  Time (mean ± σ):     292.6 ms ±   9.1 ms    [User: 230.4 ms, System: 61.8 ms]
  Range (min … max):   281.3 ms … 333.8 ms    100 runs
 
Benchmark 2: ../../main run-all.scm
  Time (mean ± σ):     441.9 ms ±  27.2 ms    [User: 365.6 ms, System: 76.1 ms]
  Range (min … max):   398.5 ms … 563.0 ms    100 runs
 
Summary
  'scheme --quiet < run-all.scm' ran
    1.51 ± 0.10 times faster than '../../main run-all.scm'
```
