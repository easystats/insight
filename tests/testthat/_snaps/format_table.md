# formatting ROPE CI

    Code
      print(parameters::equivalence_test(m10))
    Output
      # TOST-test for Practical Equivalence
      
        ROPE: [-0.83 0.83]
      
      Parameter            |         90% CI |   SGPV | Equivalence |      p
      ---------------------------------------------------------------------
      (Intercept)          | [16.39, 28.63] | < .001 |    Rejected | > .999
      Sepal Width          | [ 6.28,  9.80] | < .001 |    Rejected | > .999
      Species [versicolor] | [12.73, 16.44] | < .001 |    Rejected | > .999
      Species [virginica]  | [17.81, 21.12] | < .001 |    Rejected | > .999

