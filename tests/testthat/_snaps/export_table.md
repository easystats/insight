# export_table

    Code
      export_table(d)
    Output
           a |     b
      --------------
        1.30 |    ab
        2.00 |    cd
      543.00 | abcde

---

    Code
      export_table(d, sep = " ", header = "*", digits = 1)
    Output
          a     b
      ***********
        1.3    ab
        2.0    cd
      543.0 abcde

# export_table, table_width, remove duplicated empty lines

    Code
      print(export_table(out, table_width = 60, remove_duplicates = FALSE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
         |          |                                         
      2  |   e16sex |                           elder's gender
         |          |                                         
         |          |                                         
      3  |   e42dep |                       elder's dependency
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
      4  | c172code |               carer's level of education
         |          |                                         
         |          |                                         
         |          |                                         
      5  |  neg_c_7 |             Negative impact with 7 items
         |          |                                         
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
         |             |            |         
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
         |             |            |         
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
         |             |            |         
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |         
      5  |     numeric |   3 (3.0%) |  [7, 28]
         |             |            |         
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
         |                                 |    |      
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
         |                                 |    |      
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
         |                                 |    |      
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
         |                                 |    |      
      5  |                                 | 97 |      
         |                                 |    |      

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", remove_duplicates = FALSE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
      --------------------------------------------------------
      2  |   e16sex |                           elder's gender
                                                              
      --------------------------------------------------------
      3  |   e42dep |                       elder's dependency
                                                              
                                                              
                                                              
      --------------------------------------------------------
      4  | c172code |               carer's level of education
                                                              
                                                              
      --------------------------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ----------------------------------------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ----------------------------------------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ----------------------------------------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ----------------------------------------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
      -------------------------------------------------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      -------------------------------------------------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      -------------------------------------------------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      -------------------------------------------------
      5  |                                 | 97 |      
      -------------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", sep = " | ",
        remove_duplicates = FALSE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
      --------------------------------------------------------
      2  |   e16sex |                           elder's gender
                                                              
      --------------------------------------------------------
      3  |   e42dep |                       elder's dependency
                                                              
                                                              
                                                              
      --------------------------------------------------------
      4  | c172code |               carer's level of education
                                                              
                                                              
      --------------------------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ----------------------------------------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ----------------------------------------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ----------------------------------------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ----------------------------------------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
      -------------------------------------------------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      -------------------------------------------------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      -------------------------------------------------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      -------------------------------------------------
      5  |                                 | 97 |      
      -------------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", cross = "+",
        remove_duplicates = FALSE))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  |   e16sex |                           elder's gender
         |          |                                         
      ---+----------+-----------------------------------------
      3  |   e42dep |                       elder's dependency
         |          |                                         
         |          |                                         
         |          |                                         
      ---+----------+-----------------------------------------
      4  | c172code |               carer's level of education
         |          |                                         
         |          |                                         
      ---+----------+-----------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ---+-------------+------------+---------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ---+-------------+------------+---------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ---+-------------+------------+---------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ---+-------------+------------+---------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ---+-------------+------------+---------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      ---+---------------------------------+----+------
      1  |                                 | 98 |      
      ---+---------------------------------+----+------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      ---+---------------------------------+----+------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      ---+---------------------------------+----+------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      ---+---------------------------------+----+------
      5  |                                 | 97 |      
      -------------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, remove_duplicates = TRUE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
         |          |                                         
      2  |   e16sex |                           elder's gender
         |          |                                         
      3  |   e42dep |                       elder's dependency
         |          |                                         
      4  | c172code |               carer's level of education
         |          |                                         
      5  |  neg_c_7 |             Negative impact with 7 items
         |          |                                         
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
         |             |            |         
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
         |             |            |         
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
         |             |            |         
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |         
      5  |     numeric |   3 (3.0%) |  [7, 28]
         |             |            |         
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
         |                                 |    |      
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
         |                                 |    |      
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
         |                                 |    |      
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
         |                                 |    |      
      5  |                                 | 97 |      
         |                                 |    |      

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", remove_duplicates = TRUE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
      --------------------------------------------------------
      2  |   e16sex |                           elder's gender
      --------------------------------------------------------
      3  |   e42dep |                       elder's dependency
      --------------------------------------------------------
      4  | c172code |               carer's level of education
      --------------------------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ----------------------------------------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ----------------------------------------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ----------------------------------------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ----------------------------------------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
      -------------------------------------------------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      -------------------------------------------------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      -------------------------------------------------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      -------------------------------------------------
      5  |                                 | 97 |      
      -------------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", sep = " | ",
        remove_duplicates = TRUE))
    Output
      ID |     Name |                                    Label
      --------------------------------------------------------
      1  |  c12hour | average number of hours of care per week
      --------------------------------------------------------
      2  |   e16sex |                           elder's gender
      --------------------------------------------------------
      3  |   e42dep |                       elder's dependency
      --------------------------------------------------------
      4  | c172code |               carer's level of education
      --------------------------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ----------------------------------------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ----------------------------------------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ----------------------------------------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ----------------------------------------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ----------------------------------------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      -------------------------------------------------
      1  |                                 | 98 |      
      -------------------------------------------------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      -------------------------------------------------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      -------------------------------------------------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      -------------------------------------------------
      5  |                                 | 97 |      
      -------------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, empty_line = "-", cross = "+",
        remove_duplicates = TRUE))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  |   e16sex |                           elder's gender
      ---+----------+-----------------------------------------
      3  |   e42dep |                       elder's dependency
      ---+----------+-----------------------------------------
      4  | c172code |               carer's level of education
      ---+----------+-----------------------------------------
      5  |  neg_c_7 |             Negative impact with 7 items
      --------------------------------------------------------
      
      ID |        Type |   Missings |   Values
      ---+-------------+------------+---------
      1  |     numeric |   2 (2.0%) | [5, 168]
      ---+-------------+------------+---------
      2  |     numeric |   0 (0.0%) |        1
         |             |            |        2
      ---+-------------+------------+---------
      3  | categorical |   3 (3.0%) |        1
         |             |            |        2
         |             |            |        3
         |             |            |        4
      ---+-------------+------------+---------
      4  |     numeric | 10 (10.0%) |        1
         |             |            |        2
         |             |            |        3
      ---+-------------+------------+---------
      5  |     numeric |   3 (3.0%) |  [7, 28]
      ----------------------------------------
      
      ID |                    Value Labels |  N |  Prop
      ---+---------------------------------+----+------
      1  |                                 | 98 |      
      ---+---------------------------------+----+------
      2  |                            male | 46 | 46.0%
         |                          female | 54 | 54.0%
      ---+---------------------------------+----+------
      3  |                     independent |  2 |  2.1%
         |              slightly dependent |  4 |  4.1%
         |            moderately dependent | 28 | 28.9%
         |              severely dependent | 63 | 64.9%
      ---+---------------------------------+----+------
      4  |          low level of education |  8 |  8.9%
         | intermediate level of education | 66 | 73.3%
         |         high level of education | 16 | 17.8%
      ---+---------------------------------+----+------
      5  |                                 | 97 |      
      -------------------------------------------------

