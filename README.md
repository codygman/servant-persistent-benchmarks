Servant Persistent Query Benchmarks
==================

Just benchmarking servant with persistent using wrk:


```
~/source/unsorted/wrk $ ./wrk -t4 -c400 -d10s http://localhost:8080/person/
Running 10s test @ http://localhost:8080/person/
  4 threads and 400 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    69.78ms  120.21ms   2.00s    98.03%
    Req/Sec     1.40k   687.69     3.00k    65.13%
  48290 requests in 10.02s, 24.96MB read
  Socket errors: connect 0, read 0, write 0, timeout 209
Requests/sec:   4817.96
Transfer/sec:      2.49MB
```