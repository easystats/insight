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

---

    Code
      print(export_table(out, table_width = 60, remove_duplicates = TRUE, empty_line = "-",
        cross = "+"))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  | e15relat |                    relationship to elder
      ---+----------+-----------------------------------------
      3  |   e16sex |                           elder's gender
      ---+----------+-----------------------------------------
      4  |   e17age |                               elder' age
      --------------------------------------------------------
      
      ID |    Type |  Missings |    Values
      ---+---------+-----------+----------
      1  | numeric |  6 (0.7%) |  [4, 168]
      ---+---------+-----------+----------
      2  | numeric |  7 (0.8%) |         1
         |         |           |         2
         |         |           |         3
         |         |           |         4
         |         |           |         5
         |         |           |         6
         |         |           |         7
         |         |           |         8
      ---+---------+-----------+----------
      3  | numeric |  7 (0.8%) |         1
         |         |           |         2
      ---+---------+-----------+----------
      4  | numeric | 17 (1.9%) | [65, 103]
      ------------------------------------
      
      ID |            Value Labels |   N |  Prop
      ---+-------------------------+-----+------
      1  |                         | 902 |      
      ---+-------------------------+-----+------
      2  |          spouse/partner | 171 | 19.0%
         |                   child | 473 | 52.5%
         |                 sibling |  29 |  3.2%
         | daughter or son -in-law |  85 |  9.4%
         |              ancle/aunt |  23 |  2.6%
         |            nephew/niece |  22 |  2.4%
         |                  cousin |   6 |  0.7%
         |          other, specify |  92 | 10.2%
      ---+-------------------------+-----+------
      3  |                    male | 296 | 32.9%
         |                  female | 605 | 67.1%
      ---+-------------------------+-----+------
      4  |                         | 891 |      
      ------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, remove_duplicates = FALSE,
        empty_line = "-", cross = "+"))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  | e15relat |                    relationship to elder
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
      ---+----------+-----------------------------------------
      3  |   e16sex |                           elder's gender
         |          |                                         
      ---+----------+-----------------------------------------
      4  |   e17age |                               elder' age
      --------------------------------------------------------
      
      ID |    Type |  Missings |    Values
      ---+---------+-----------+----------
      1  | numeric |  6 (0.7%) |  [4, 168]
      ---+---------+-----------+----------
      2  | numeric |  7 (0.8%) |         1
         |         |           |         2
         |         |           |         3
         |         |           |         4
         |         |           |         5
         |         |           |         6
         |         |           |         7
         |         |           |         8
      ---+---------+-----------+----------
      3  | numeric |  7 (0.8%) |         1
         |         |           |         2
      ---+---------+-----------+----------
      4  | numeric | 17 (1.9%) | [65, 103]
      ------------------------------------
      
      ID |            Value Labels |   N |  Prop
      ---+-------------------------+-----+------
      1  |                         | 902 |      
      ---+-------------------------+-----+------
      2  |          spouse/partner | 171 | 19.0%
         |                   child | 473 | 52.5%
         |                 sibling |  29 |  3.2%
         | daughter or son -in-law |  85 |  9.4%
         |              ancle/aunt |  23 |  2.6%
         |            nephew/niece |  22 |  2.4%
         |                  cousin |   6 |  0.7%
         |          other, specify |  92 | 10.2%
      ---+-------------------------+-----+------
      3  |                    male | 296 | 32.9%
         |                  female | 605 | 67.1%
      ---+-------------------------+-----+------
      4  |                         | 891 |      
      ------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, remove_duplicates = TRUE, empty_line = "-",
        cross = "+"))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  | e15relat |                    relationship to elder
      ---+----------+-----------------------------------------
      3  |   e16sex |                           elder's gender
      --------------------------------------------------------
      
      ID |    Type | Missings |   Values
      ---+---------+----------+---------
      1  | numeric | 6 (0.7%) | [4, 168]
      ---+---------+----------+---------
      2  | numeric | 7 (0.8%) |        1
         |         |          |        2
         |         |          |        3
         |         |          |        4
         |         |          |        5
         |         |          |        6
         |         |          |        7
         |         |          |        8
      ---+---------+----------+---------
      3  | numeric | 7 (0.8%) |        1
         |         |          |        2
      ----------------------------------
      
      ID |            Value Labels |   N |  Prop
      ---+-------------------------+-----+------
      1  |                         | 902 |      
      ---+-------------------------+-----+------
      2  |          spouse/partner | 171 | 19.0%
         |                   child | 473 | 52.5%
         |                 sibling |  29 |  3.2%
         | daughter or son -in-law |  85 |  9.4%
         |              ancle/aunt |  23 |  2.6%
         |            nephew/niece |  22 |  2.4%
         |                  cousin |   6 |  0.7%
         |          other, specify |  92 | 10.2%
      ---+-------------------------+-----+------
      3  |                    male | 296 | 32.9%
         |                  female | 605 | 67.1%
      ------------------------------------------

---

    Code
      print(export_table(out, table_width = 60, remove_duplicates = FALSE,
        empty_line = "-", cross = "+"))
    Output
      ID |     Name |                                    Label
      ---+----------+-----------------------------------------
      1  |  c12hour | average number of hours of care per week
      ---+----------+-----------------------------------------
      2  | e15relat |                    relationship to elder
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
         |          |                                         
      ---+----------+-----------------------------------------
      3  |   e16sex |                           elder's gender
         |          |                                         
      --------------------------------------------------------
      
      ID |    Type | Missings |   Values
      ---+---------+----------+---------
      1  | numeric | 6 (0.7%) | [4, 168]
      ---+---------+----------+---------
      2  | numeric | 7 (0.8%) |        1
         |         |          |        2
         |         |          |        3
         |         |          |        4
         |         |          |        5
         |         |          |        6
         |         |          |        7
         |         |          |        8
      ---+---------+----------+---------
      3  | numeric | 7 (0.8%) |        1
         |         |          |        2
      ----------------------------------
      
      ID |            Value Labels |   N |  Prop
      ---+-------------------------+-----+------
      1  |                         | 902 |      
      ---+-------------------------+-----+------
      2  |          spouse/partner | 171 | 19.0%
         |                   child | 473 | 52.5%
         |                 sibling |  29 |  3.2%
         | daughter or son -in-law |  85 |  9.4%
         |              ancle/aunt |  23 |  2.6%
         |            nephew/niece |  22 |  2.4%
         |                  cousin |   6 |  0.7%
         |          other, specify |  92 | 10.2%
      ---+-------------------------+-----+------
      3  |                    male | 296 | 32.9%
         |                  female | 605 | 67.1%
      ------------------------------------------

# export_table, overlengthy lines

    Code
      print(export_table(d[1:10, ], verbose = FALSE))
    Output
      ano1_bno1_cno1_dno1_eno1_fno1_gno1_hno1_ino1_jno1_kno1_lno1_mno1_nno1_ono1_pno1_qno1_rno1_sno1_tno1_uno1_vno1_wno1_xno1_yno1_zno1
      ---------------------------------------------------------------------------------------------------------------------------------
                                                                                                                                   5.10
                                                                                                                                   4.90
                                                                                                                                   4.70
                                                                                                                                   4.60
                                                                                                                                   5.00
                                                                                                                                   5.40
                                                                                                                                   4.60
                                                                                                                                   5.00
                                                                                                                                   4.40
                                                                                                                                   4.90
      
      ano2_bno2_cno2_dno2_eno2_fno2_gno2_hno2_ino2_jno2_kno2_lno2_mno2_nno2_ono2_pno2_qno2_rno2_sno2_tno2_uno2_vno2_wno2_xno2_yno2_zno2
      ---------------------------------------------------------------------------------------------------------------------------------
                                                                                                                                   3.50
                                                                                                                                   3.00
                                                                                                                                   3.20
                                                                                                                                   3.10
                                                                                                                                   3.60
                                                                                                                                   3.90
                                                                                                                                   3.40
                                                                                                                                   3.40
                                                                                                                                   2.90
                                                                                                                                   3.10
      
      Petal.Length | Petal.Width | Species
      ------------------------------------
              1.40 |        0.20 |  setosa
              1.40 |        0.20 |  setosa
              1.30 |        0.20 |  setosa
              1.50 |        0.20 |  setosa
              1.40 |        0.20 |  setosa
              1.70 |        0.40 |  setosa
              1.40 |        0.30 |  setosa
              1.50 |        0.20 |  setosa
              1.40 |        0.20 |  setosa
              1.50 |        0.10 |  setosa

---

    Code
      print(export_table(d[1:10, ], verbose = FALSE))
    Output
      Sepal.Length
      ------------
              5.10
              4.90
              4.70
              4.60
              5.00
              5.40
              4.60
              5.00
              4.40
              4.90
      
      ano1_bno1_cno1_dno1_eno1_fno1_gno1_hno1_ino1_jno1_kno1_lno1_mno1_nno1_ono1_pno1_qno1_rno1_sno1_tno1_uno1_vno1_wno1_xno1_yno1_zno1
      ---------------------------------------------------------------------------------------------------------------------------------
                                                                                                                                   3.50
                                                                                                                                   3.00
                                                                                                                                   3.20
                                                                                                                                   3.10
                                                                                                                                   3.60
                                                                                                                                   3.90
                                                                                                                                   3.40
                                                                                                                                   3.40
                                                                                                                                   2.90
                                                                                                                                   3.10
      
      Petal.Length | Petal.Width | ano2_bno2_cno2_dno2_eno2_fno2_gno2_hno2_ino2_jno2_kno2_lno2_mno2_nno2_ono2_pno2_qno2_rno2_sno2_tno2_uno2_vno2_wno2_xno2_yno2_zno2
      --------------------------------------------------------------------------------------------------------------------------------------------------------------
              1.40 |        0.20 |                                                                                                                            setosa
              1.40 |        0.20 |                                                                                                                            setosa
              1.30 |        0.20 |                                                                                                                            setosa
              1.50 |        0.20 |                                                                                                                            setosa
              1.40 |        0.20 |                                                                                                                            setosa
              1.70 |        0.40 |                                                                                                                            setosa
              1.40 |        0.30 |                                                                                                                            setosa
              1.50 |        0.20 |                                                                                                                            setosa
              1.40 |        0.20 |                                                                                                                            setosa
              1.50 |        0.10 |                                                                                                                            setosa

