# authentect_spectroscopic
demo work for potential show and tell with authentect.

to run:

* First run cmake to setup the C++ code to generate sample data.
* make
* run *generate_spectroscopic_data* in src/cpp
* walk through command line args to generate a bunch of data in the `output` folder.
* you can run the clojure spectroscopic matching by
   `lein run ${projectpath}/output 0 100` or whatever range you want to run.
