---
title: Benchmarking Hortonworks Data Platform 2.3
---
<pre class="lang:sh decode:true">hadoop jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-examples.jar teragen 100000000 /home/hdfs/teragen-10g-t4

hadoop jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-examples.jar terasort /home/hdfs/teragen-10g-t4 /home/hdfs/teragen-10g-t4-out-2

yarn jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-examples.jar pi 16 100000

yarn jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-client-jobclient-2.7.1.2.3.2.0-2950-tests.jar TestDFSIO -write -nrFiles 10 -fileSize 1000

yarn jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-client-jobclient-2.7.1.2.3.2.0-2950-tests.jar TestDFSIO -read -nrFiles 10 -fileSize 1000

yarn jar /usr/hdp/2.3.2.0-2950/hadoop-mapreduce/hadoop-mapreduce-client-jobclient-2.7.1.2.3.2.0-2950-tests.jar TestDFSIO -clean

</pre>

*   http://docs.hortonworks.com/HDPDocuments/HDP2/HDP-2.1.3/bk_using-apache-hadoop/content/running_mapreduce_examples_on_yarn.html
*   http://www.michael-noll.com/blog/2011/04/09/benchmarking-and-stress-testing-an-hadoop-cluster-with-terasort-testdfsio-nnbench-mrbench/
*   https://support.pivotal.io/hc/en-us/articles/200864057-Running-DFSIO-MapReduce-benchmark-test
*   https://support.pivotal.io/hc/en-us/articles/200927666-Running-TeraSort-MapReduce-Benchmark


