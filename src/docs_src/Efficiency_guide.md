To run selection methods which try to produce both correct and time-optimized solutions use the following parameters:
main gp loop is: gp-efficiency
error-function:
    import propeller.tools.efficieny-error-functions
        :error-function1 used for problems with 1 input per case
        :error-function2 used for problems with 2 inputs per case
parent-selection:
        :tournament-efficiency , tournament selection which sums correctness and total runtime
        :lexicase2 , lexicase selection which randomly considers runtime as an "extra testcase"
        :lexicase-parallel , for each test case take both the most correct and the fastest

All problems are in and can be run from session.cljc

Sample parameters:
  (gp/gp-efficiency
   (merge
    {:instructions            instructions
     :error-function          error/error-function2
     :training-data           (:train train-and-test-data)
     :testing-data            (:test train-and-test-data)
     :max-generations         500
     :population-size         500
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :lexicase2
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 0.5 :crossover 0.5}
     :elitism                 false}

    