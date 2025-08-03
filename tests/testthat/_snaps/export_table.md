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

# export_table, gt, simple

    Code
      as.character(out)
    Output
      [1] "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_heading\" style=\"border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\" align=\"center\">\n      <td colspan=\"2\" class=\"gt_heading gt_title gt_font_normal gt_bottom_border\" style=\"border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;\" bgcolor=\"#FFFFFF\" align=\"center\">Table Title</td>\n    </tr>\n    \n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">1.30</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">2.00</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">cd</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">543.00</td>\n<td headers=\"b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">abcde</td></tr>\n  </tbody>\n  <tfoot class=\"gt_sourcenotes\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n    <tr style=\"border-style: none;\">\n      <td class=\"gt_sourcenote\" colspan=\"2\" style=\"border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;\"></td>\n    </tr>\n  </tfoot>\n  \n</table>\n</div>"

---

    Code
      as.character(out)
    Output
      [1] "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_heading\" style=\"border-style: none; background-color: #FFFFFF; text-align: center; border-bottom-color: #FFFFFF; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\" align=\"center\">\n      <td colspan=\"2\" class=\"gt_heading gt_title gt_font_normal gt_bottom_border\" style=\"border-style: none; color: #333333; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; background-color: #FFFFFF; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; font-weight: normal;\" bgcolor=\"#FFFFFF\" align=\"center\">Table Title</td>\n    </tr>\n    \n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_right\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"right\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">1.30</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">2.00</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">cd</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">543.00</td>\n<td headers=\"b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">abcde</td></tr>\n  </tbody>\n  <tfoot class=\"gt_sourcenotes\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n    <tr style=\"border-style: none;\">\n      <td class=\"gt_sourcenote\" colspan=\"2\" style=\"border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;\"></td>\n    </tr>\n  </tfoot>\n  \n</table>\n</div>"

---

    Code
      as.character(out)
    Output
      [1] "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g1\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g1</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">1.30</td>\n<td headers=\"g1  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; border-top-width: 2px;\" valign=\"middle\" align=\"center\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">2.00</td>\n<td headers=\"g1  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">cd</td></tr>\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g2\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g2</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">543.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; border-top-width: 2px;\" valign=\"middle\" align=\"center\">abcde</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">78.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">hj</td></tr>\n  </tbody>\n  <tfoot class=\"gt_sourcenotes\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n    <tr style=\"border-style: none;\">\n      <td class=\"gt_sourcenote\" colspan=\"2\" style=\"border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;\"></td>\n    </tr>\n  </tfoot>\n  \n</table>\n</div>"

---

    Code
      as.character(out)
    Output
      [1] "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_right\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"right\">a</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"b\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">b</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g1\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g1</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;\" valign=\"middle\" align=\"right\">1.30</td>\n<td headers=\"g1  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">ab</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g1  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">2.00</td>\n<td headers=\"g1  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">cd</td></tr>\n    <tr class=\"gt_group_heading_row\" style=\"border-style: none;\">\n      <th colspan=\"2\" class=\"gt_group_heading\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: initial; text-transform: inherit; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; text-align: left; font-style: oblique;\" scope=\"colgroup\" id=\"g2\" bgcolor=\"#FFFFFF\" valign=\"middle\" align=\"left\">g2</th>\n    </tr>\n    <tr class=\"gt_row_group_first\" style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; border-top-width: 2px;\" valign=\"middle\" align=\"right\">543.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-top-width: 2px;\" valign=\"middle\" align=\"left\">abcde</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"g2  a\" class=\"gt_row gt_right\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;\" valign=\"middle\" align=\"right\">78.00</td>\n<td headers=\"g2  b\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;\" valign=\"middle\" align=\"left\">hj</td></tr>\n  </tbody>\n  <tfoot class=\"gt_sourcenotes\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n    <tr style=\"border-style: none;\">\n      <td class=\"gt_sourcenote\" colspan=\"2\" style=\"border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;\"></td>\n    </tr>\n  </tfoot>\n  \n</table>\n</div>"

# export_table, gt, complex with group indention

    Code
      as.character(out)
    Output
      [1] "<div id=\"osncjrvket\" style=\"padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;\">\n  \n  <table class=\"gt_table\" data-quarto-disable-processing=\"false\" data-quarto-bootstrap=\"false\" style=\"-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 100%; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n  <thead style=\"border-style: none;\">\n    <tr class=\"gt_col_headings gt_spanner_row\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; border-bottom-style: hidden;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_left\" rowspan=\"2\" colspan=\"1\" scope=\"col\" id=\"Parameter\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"left\">Parameter</th>\n      <th class=\"gt_center gt_columns_top_border gt_column_spanner_outer\" rowspan=\"1\" colspan=\"2\" scope=\"colgroup\" id=\"lm1\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; padding-top: 0; padding-bottom: 0; padding-left: 4px; padding-right: 4px; text-align: center;\" bgcolor=\"#FFFFFF\" align=\"center\">\n        <div class=\"gt_column_spanner\" style=\"border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden; display: inline-block; width: 100%;\">lm1</div>\n      </th>\n      <th class=\"gt_center gt_columns_top_border gt_column_spanner_outer\" rowspan=\"1\" colspan=\"2\" scope=\"colgroup\" id=\"lm2\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; padding-top: 0; padding-bottom: 0; padding-left: 4px; text-align: center; padding-right: 0;\" bgcolor=\"#FFFFFF\" align=\"center\">\n        <div class=\"gt_column_spanner\" style=\"border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 5px; overflow-x: hidden; display: inline-block; width: 100%;\">lm2</div>\n      </th>\n    </tr>\n    <tr class=\"gt_col_headings\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;\">\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"Coefficient-(lm1)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">Coefficient</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a(SE)-(lm1)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">(SE)</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"Coefficient-(lm2)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">Coefficient</th>\n      <th class=\"gt_col_heading gt_columns_bottom_border gt_center\" rowspan=\"1\" colspan=\"1\" scope=\"col\" id=\"a(SE)-(lm2)\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;\" bgcolor=\"#FFFFFF\" valign=\"bottom\" align=\"center\">(SE)</th>\n    </tr>\n  </thead>\n  <tbody class=\"gt_table_body\" style=\"border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;\">\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; font-style: oblique; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                              Species</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">       </td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                 Species (versicolor)</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">-1.60***</td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.19)</td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">-1.69**</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.56)</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                  Species (virginica)</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">-2.12***</td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.27)</td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">-1.19 *</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.60)</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; font-style: oblique; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                         Interactions</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">       </td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">  Species (versicolor) × Petal Length</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">  -0.01</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.28)</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">   Species (virginica) × Petal Length</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">  -0.15</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.27)</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; font-style: oblique; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                             Controls</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">       </td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                         Petal Length</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\"> 0.90***</td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.06)</td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">   0.39</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">(0.26)</td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; font-style: oblique; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                                     </td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">        </td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">       </td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td></tr>\n    <tr style=\"border-style: none;\"><td headers=\"Parameter\" class=\"gt_row gt_left\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left; font-style: oblique; border-right-width: 1px; border-right-style: solid; border-right-color: #d3d3d3;\" valign=\"middle\" align=\"left\">                         Observations</td>\n<td headers=\"Coefficient (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">     150</td>\n<td headers=\"(SE) (lm1)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td>\n<td headers=\"Coefficient (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">    150</td>\n<td headers=\"(SE) (lm2)\" class=\"gt_row gt_center\" style=\"border-style: none; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center;\" valign=\"middle\" align=\"center\">      </td></tr>\n  </tbody>\n  <tfoot class=\"gt_sourcenotes\" style=\"border-style: none; color: #333333; background-color: #FFFFFF; border-bottom-style: none; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3;\" bgcolor=\"#FFFFFF\">\n    <tr style=\"border-style: none;\">\n      <td class=\"gt_sourcenote\" colspan=\"5\" style=\"border-style: none; font-size: 90%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px;\"></td>\n    </tr>\n  </tfoot>\n  \n</table>\n</div>"

# export_table, by in text format

    Code
      export_table(mtcars, by = c("cyl", "gear"))
    Output
      Group: cyl=6, gear=4
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      21.00 | 160.00 | 110 | 3.90 | 2.62 | 16.46 |  0 |  1 |    4
      21.00 | 160.00 | 110 | 3.90 | 2.88 | 17.02 |  0 |  1 |    4
      19.20 | 167.60 | 123 | 3.92 | 3.44 | 18.30 |  1 |  0 |    4
      17.80 | 167.60 | 123 | 3.92 | 3.44 | 18.90 |  1 |  0 |    4
      
      Group: cyl=4, gear=4
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      22.80 | 108.00 |  93 | 3.85 | 2.32 | 18.61 |  1 |  1 |    1
      24.40 | 146.70 |  62 | 3.69 | 3.19 | 20.00 |  1 |  0 |    2
      22.80 | 140.80 |  95 | 3.92 | 3.15 | 22.90 |  1 |  0 |    2
      32.40 |  78.70 |  66 | 4.08 | 2.20 | 19.47 |  1 |  1 |    1
      30.40 |  75.70 |  52 | 4.93 | 1.61 | 18.52 |  1 |  1 |    2
      33.90 |  71.10 |  65 | 4.22 | 1.83 | 19.90 |  1 |  1 |    1
      27.30 |  79.00 |  66 | 4.08 | 1.94 | 18.90 |  1 |  1 |    1
      21.40 | 121.00 | 109 | 4.11 | 2.78 | 18.60 |  1 |  1 |    2
      
      Group: cyl=8, gear=4
      
      mpg | disp | hp | drat | wt | qsec | vs | am | carb
      ---------------------------------------------------
      
      Group: cyl=6, gear=3
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      21.40 |  258 | 110 | 3.08 | 3.21 | 19.44 |  1 |  0 |    1
      18.10 |  225 | 105 | 2.76 | 3.46 | 20.22 |  1 |  0 |    1
      
      Group: cyl=4, gear=3
      
        mpg |   disp | hp | drat |   wt |  qsec | vs | am | carb
      ----------------------------------------------------------
      21.50 | 120.10 | 97 | 3.70 | 2.46 | 20.01 |  1 |  0 |    1
      
      Group: cyl=8, gear=3
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      18.70 | 360.00 | 175 | 3.15 | 3.44 | 17.02 |  0 |  0 |    2
      14.30 | 360.00 | 245 | 3.21 | 3.57 | 15.84 |  0 |  0 |    4
      16.40 | 275.80 | 180 | 3.07 | 4.07 | 17.40 |  0 |  0 |    3
      17.30 | 275.80 | 180 | 3.07 | 3.73 | 17.60 |  0 |  0 |    3
      15.20 | 275.80 | 180 | 3.07 | 3.78 | 18.00 |  0 |  0 |    3
      10.40 | 472.00 | 205 | 2.93 | 5.25 | 17.98 |  0 |  0 |    4
      10.40 | 460.00 | 215 | 3.00 | 5.42 | 17.82 |  0 |  0 |    4
      14.70 | 440.00 | 230 | 3.23 | 5.34 | 17.42 |  0 |  0 |    4
      15.50 | 318.00 | 150 | 2.76 | 3.52 | 16.87 |  0 |  0 |    2
      15.20 | 304.00 | 150 | 3.15 | 3.44 | 17.30 |  0 |  0 |    2
      13.30 | 350.00 | 245 | 3.73 | 3.84 | 15.41 |  0 |  0 |    4
      19.20 | 400.00 | 175 | 3.08 | 3.85 | 17.05 |  0 |  0 |    2
      
      Group: cyl=6, gear=5
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      19.70 |  145 | 175 | 3.62 | 2.77 | 15.50 |  0 |  1 |    6
      
      Group: cyl=4, gear=5
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      26.00 | 120.30 |  91 | 4.43 | 2.14 | 16.70 |  0 |  1 |    2
      30.40 |  95.10 | 113 | 3.77 | 1.51 | 16.90 |  1 |  1 |    2
      
      Group: cyl=8, gear=5
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      15.80 |  351 | 264 | 4.22 | 3.17 | 14.50 |  0 |  1 |    4
      15.00 |  301 | 335 | 3.54 | 3.57 | 14.60 |  0 |  1 |    8

---

    Code
      export_table(iris, by = "Species")
    Output
      Group: Species=setosa
      
      Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
      -------------------------------------------------------
              5.10 |        3.50 |         1.40 |        0.20
              4.90 |        3.00 |         1.40 |        0.20
              4.70 |        3.20 |         1.30 |        0.20
              4.60 |        3.10 |         1.50 |        0.20
              5.00 |        3.60 |         1.40 |        0.20
              5.40 |        3.90 |         1.70 |        0.40
              4.60 |        3.40 |         1.40 |        0.30
              5.00 |        3.40 |         1.50 |        0.20
              4.40 |        2.90 |         1.40 |        0.20
              4.90 |        3.10 |         1.50 |        0.10
              5.40 |        3.70 |         1.50 |        0.20
              4.80 |        3.40 |         1.60 |        0.20
              4.80 |        3.00 |         1.40 |        0.10
              4.30 |        3.00 |         1.10 |        0.10
              5.80 |        4.00 |         1.20 |        0.20
              5.70 |        4.40 |         1.50 |        0.40
              5.40 |        3.90 |         1.30 |        0.40
              5.10 |        3.50 |         1.40 |        0.30
              5.70 |        3.80 |         1.70 |        0.30
              5.10 |        3.80 |         1.50 |        0.30
              5.40 |        3.40 |         1.70 |        0.20
              5.10 |        3.70 |         1.50 |        0.40
              4.60 |        3.60 |         1.00 |        0.20
              5.10 |        3.30 |         1.70 |        0.50
              4.80 |        3.40 |         1.90 |        0.20
              5.00 |        3.00 |         1.60 |        0.20
              5.00 |        3.40 |         1.60 |        0.40
              5.20 |        3.50 |         1.50 |        0.20
              5.20 |        3.40 |         1.40 |        0.20
              4.70 |        3.20 |         1.60 |        0.20
              4.80 |        3.10 |         1.60 |        0.20
              5.40 |        3.40 |         1.50 |        0.40
              5.20 |        4.10 |         1.50 |        0.10
              5.50 |        4.20 |         1.40 |        0.20
              4.90 |        3.10 |         1.50 |        0.20
              5.00 |        3.20 |         1.20 |        0.20
              5.50 |        3.50 |         1.30 |        0.20
              4.90 |        3.60 |         1.40 |        0.10
              4.40 |        3.00 |         1.30 |        0.20
              5.10 |        3.40 |         1.50 |        0.20
              5.00 |        3.50 |         1.30 |        0.30
              4.50 |        2.30 |         1.30 |        0.30
              4.40 |        3.20 |         1.30 |        0.20
              5.00 |        3.50 |         1.60 |        0.60
              5.10 |        3.80 |         1.90 |        0.40
              4.80 |        3.00 |         1.40 |        0.30
              5.10 |        3.80 |         1.60 |        0.20
              4.60 |        3.20 |         1.40 |        0.20
              5.30 |        3.70 |         1.50 |        0.20
              5.00 |        3.30 |         1.40 |        0.20
      
      Group: Species=versicolor
      
      Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
      -------------------------------------------------------
              7.00 |        3.20 |         4.70 |        1.40
              6.40 |        3.20 |         4.50 |        1.50
              6.90 |        3.10 |         4.90 |        1.50
              5.50 |        2.30 |         4.00 |        1.30
              6.50 |        2.80 |         4.60 |        1.50
              5.70 |        2.80 |         4.50 |        1.30
              6.30 |        3.30 |         4.70 |        1.60
              4.90 |        2.40 |         3.30 |        1.00
              6.60 |        2.90 |         4.60 |        1.30
              5.20 |        2.70 |         3.90 |        1.40
              5.00 |        2.00 |         3.50 |        1.00
              5.90 |        3.00 |         4.20 |        1.50
              6.00 |        2.20 |         4.00 |        1.00
              6.10 |        2.90 |         4.70 |        1.40
              5.60 |        2.90 |         3.60 |        1.30
              6.70 |        3.10 |         4.40 |        1.40
              5.60 |        3.00 |         4.50 |        1.50
              5.80 |        2.70 |         4.10 |        1.00
              6.20 |        2.20 |         4.50 |        1.50
              5.60 |        2.50 |         3.90 |        1.10
              5.90 |        3.20 |         4.80 |        1.80
              6.10 |        2.80 |         4.00 |        1.30
              6.30 |        2.50 |         4.90 |        1.50
              6.10 |        2.80 |         4.70 |        1.20
              6.40 |        2.90 |         4.30 |        1.30
              6.60 |        3.00 |         4.40 |        1.40
              6.80 |        2.80 |         4.80 |        1.40
              6.70 |        3.00 |         5.00 |        1.70
              6.00 |        2.90 |         4.50 |        1.50
              5.70 |        2.60 |         3.50 |        1.00
              5.50 |        2.40 |         3.80 |        1.10
              5.50 |        2.40 |         3.70 |        1.00
              5.80 |        2.70 |         3.90 |        1.20
              6.00 |        2.70 |         5.10 |        1.60
              5.40 |        3.00 |         4.50 |        1.50
              6.00 |        3.40 |         4.50 |        1.60
              6.70 |        3.10 |         4.70 |        1.50
              6.30 |        2.30 |         4.40 |        1.30
              5.60 |        3.00 |         4.10 |        1.30
              5.50 |        2.50 |         4.00 |        1.30
              5.50 |        2.60 |         4.40 |        1.20
              6.10 |        3.00 |         4.60 |        1.40
              5.80 |        2.60 |         4.00 |        1.20
              5.00 |        2.30 |         3.30 |        1.00
              5.60 |        2.70 |         4.20 |        1.30
              5.70 |        3.00 |         4.20 |        1.20
              5.70 |        2.90 |         4.20 |        1.30
              6.20 |        2.90 |         4.30 |        1.30
              5.10 |        2.50 |         3.00 |        1.10
              5.70 |        2.80 |         4.10 |        1.30
      
      Group: Species=virginica
      
      Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
      -------------------------------------------------------
              6.30 |        3.30 |         6.00 |        2.50
              5.80 |        2.70 |         5.10 |        1.90
              7.10 |        3.00 |         5.90 |        2.10
              6.30 |        2.90 |         5.60 |        1.80
              6.50 |        3.00 |         5.80 |        2.20
              7.60 |        3.00 |         6.60 |        2.10
              4.90 |        2.50 |         4.50 |        1.70
              7.30 |        2.90 |         6.30 |        1.80
              6.70 |        2.50 |         5.80 |        1.80
              7.20 |        3.60 |         6.10 |        2.50
              6.50 |        3.20 |         5.10 |        2.00
              6.40 |        2.70 |         5.30 |        1.90
              6.80 |        3.00 |         5.50 |        2.10
              5.70 |        2.50 |         5.00 |        2.00
              5.80 |        2.80 |         5.10 |        2.40
              6.40 |        3.20 |         5.30 |        2.30
              6.50 |        3.00 |         5.50 |        1.80
              7.70 |        3.80 |         6.70 |        2.20
              7.70 |        2.60 |         6.90 |        2.30
              6.00 |        2.20 |         5.00 |        1.50
              6.90 |        3.20 |         5.70 |        2.30
              5.60 |        2.80 |         4.90 |        2.00
              7.70 |        2.80 |         6.70 |        2.00
              6.30 |        2.70 |         4.90 |        1.80
              6.70 |        3.30 |         5.70 |        2.10
              7.20 |        3.20 |         6.00 |        1.80
              6.20 |        2.80 |         4.80 |        1.80
              6.10 |        3.00 |         4.90 |        1.80
              6.40 |        2.80 |         5.60 |        2.10
              7.20 |        3.00 |         5.80 |        1.60
              7.40 |        2.80 |         6.10 |        1.90
              7.90 |        3.80 |         6.40 |        2.00
              6.40 |        2.80 |         5.60 |        2.20
              6.30 |        2.80 |         5.10 |        1.50
              6.10 |        2.60 |         5.60 |        1.40
              7.70 |        3.00 |         6.10 |        2.30
              6.30 |        3.40 |         5.60 |        2.40
              6.40 |        3.10 |         5.50 |        1.80
              6.00 |        3.00 |         4.80 |        1.80
              6.90 |        3.10 |         5.40 |        2.10
              6.70 |        3.10 |         5.60 |        2.40
              6.90 |        3.10 |         5.10 |        2.30
              5.80 |        2.70 |         5.10 |        1.90
              6.80 |        3.20 |         5.90 |        2.30
              6.70 |        3.30 |         5.70 |        2.50
              6.70 |        3.00 |         5.20 |        2.30
              6.30 |        2.50 |         5.00 |        1.90
              6.50 |        3.00 |         5.20 |        2.00
              6.20 |        3.40 |         5.40 |        2.30
              5.90 |        3.00 |         5.10 |        1.80

---

    Code
      export_table(mtcars, by = ~ cyl + gear)
    Output
      Group: cyl=6, gear=4
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      21.00 | 160.00 | 110 | 3.90 | 2.62 | 16.46 |  0 |  1 |    4
      21.00 | 160.00 | 110 | 3.90 | 2.88 | 17.02 |  0 |  1 |    4
      19.20 | 167.60 | 123 | 3.92 | 3.44 | 18.30 |  1 |  0 |    4
      17.80 | 167.60 | 123 | 3.92 | 3.44 | 18.90 |  1 |  0 |    4
      
      Group: cyl=4, gear=4
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      22.80 | 108.00 |  93 | 3.85 | 2.32 | 18.61 |  1 |  1 |    1
      24.40 | 146.70 |  62 | 3.69 | 3.19 | 20.00 |  1 |  0 |    2
      22.80 | 140.80 |  95 | 3.92 | 3.15 | 22.90 |  1 |  0 |    2
      32.40 |  78.70 |  66 | 4.08 | 2.20 | 19.47 |  1 |  1 |    1
      30.40 |  75.70 |  52 | 4.93 | 1.61 | 18.52 |  1 |  1 |    2
      33.90 |  71.10 |  65 | 4.22 | 1.83 | 19.90 |  1 |  1 |    1
      27.30 |  79.00 |  66 | 4.08 | 1.94 | 18.90 |  1 |  1 |    1
      21.40 | 121.00 | 109 | 4.11 | 2.78 | 18.60 |  1 |  1 |    2
      
      Group: cyl=8, gear=4
      
      mpg | disp | hp | drat | wt | qsec | vs | am | carb
      ---------------------------------------------------
      
      Group: cyl=6, gear=3
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      21.40 |  258 | 110 | 3.08 | 3.21 | 19.44 |  1 |  0 |    1
      18.10 |  225 | 105 | 2.76 | 3.46 | 20.22 |  1 |  0 |    1
      
      Group: cyl=4, gear=3
      
        mpg |   disp | hp | drat |   wt |  qsec | vs | am | carb
      ----------------------------------------------------------
      21.50 | 120.10 | 97 | 3.70 | 2.46 | 20.01 |  1 |  0 |    1
      
      Group: cyl=8, gear=3
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      18.70 | 360.00 | 175 | 3.15 | 3.44 | 17.02 |  0 |  0 |    2
      14.30 | 360.00 | 245 | 3.21 | 3.57 | 15.84 |  0 |  0 |    4
      16.40 | 275.80 | 180 | 3.07 | 4.07 | 17.40 |  0 |  0 |    3
      17.30 | 275.80 | 180 | 3.07 | 3.73 | 17.60 |  0 |  0 |    3
      15.20 | 275.80 | 180 | 3.07 | 3.78 | 18.00 |  0 |  0 |    3
      10.40 | 472.00 | 205 | 2.93 | 5.25 | 17.98 |  0 |  0 |    4
      10.40 | 460.00 | 215 | 3.00 | 5.42 | 17.82 |  0 |  0 |    4
      14.70 | 440.00 | 230 | 3.23 | 5.34 | 17.42 |  0 |  0 |    4
      15.50 | 318.00 | 150 | 2.76 | 3.52 | 16.87 |  0 |  0 |    2
      15.20 | 304.00 | 150 | 3.15 | 3.44 | 17.30 |  0 |  0 |    2
      13.30 | 350.00 | 245 | 3.73 | 3.84 | 15.41 |  0 |  0 |    4
      19.20 | 400.00 | 175 | 3.08 | 3.85 | 17.05 |  0 |  0 |    2
      
      Group: cyl=6, gear=5
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      19.70 |  145 | 175 | 3.62 | 2.77 | 15.50 |  0 |  1 |    6
      
      Group: cyl=4, gear=5
      
        mpg |   disp |  hp | drat |   wt |  qsec | vs | am | carb
      -----------------------------------------------------------
      26.00 | 120.30 |  91 | 4.43 | 2.14 | 16.70 |  0 |  1 |    2
      30.40 |  95.10 | 113 | 3.77 | 1.51 | 16.90 |  1 |  1 |    2
      
      Group: cyl=8, gear=5
      
        mpg | disp |  hp | drat |   wt |  qsec | vs | am | carb
      ---------------------------------------------------------
      15.80 |  351 | 264 | 4.22 | 3.17 | 14.50 |  0 |  1 |    4
      15.00 |  301 | 335 | 3.54 | 3.57 | 14.60 |  0 |  1 |    8

# export_table, tinytable with indented rows

    Code
      export_table(mp, format = "tt", row_groups = groups, table_width = Inf)
    Output
      +---------------+-------------+------+--------------+-------+-------+
      | Parameter     | Coefficient | SE   | 95% CI       | t(22) | p     |
      +===============+=============+======+==============+=======+=======+
      | Engine                                                            |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [6]       | -2.47       | 2.21 | -7.05, 2.12  | -1.12 | 0.276 |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [8]       | 1.97        | 5.11 | -8.63, 12.58 | 0.39  | 0.703 |
      +---------------+-------------+------+--------------+-------+-------+
      | vs            | 3.18        | 3.79 | -4.68, 11.04 | 0.84  | 0.410 |
      +---------------+-------------+------+--------------+-------+-------+
      | hp            | -0.06       | 0.02 | -0.11, -0.02 | -2.91 | 0.008 |
      +---------------+-------------+------+--------------+-------+-------+
      | Interactions                                                      |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4] × vs | -2.90       | 4.67 | -12.57, 6.78 | -0.62 | 0.541 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5] × vs | 2.59        | 4.54 | -6.82, 12.00 | 0.57  | 0.574 |
      +---------------+-------------+------+--------------+-------+-------+
      | Controls                                                          |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4]      | 3.10        | 4.34 | -5.90, 12.10 | 0.71  | 0.482 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5]      | 4.80        | 3.48 | -2.42, 12.01 | 1.38  | 0.182 |
      +---------------+-------------+------+--------------+-------+-------+
      | drat          | 2.70        | 2.03 | -1.52, 6.91  | 1.33  | 0.198 |
      +---------------+-------------+------+--------------+-------+-------+ 

---

    Code
      export_table(mp, format = "text", row_groups = groups, table_width = Inf)
    Output
      Parameter       | Coefficient |   SE |       95% CI | t(22) |     p
      -------------------------------------------------------------------
      Engine          |             |      |              |       |      
        cyl [6]       |       -2.47 | 2.21 |  -7.05, 2.12 | -1.12 | 0.276
        cyl [8]       |        1.97 | 5.11 | -8.63, 12.58 |  0.39 | 0.703
        vs            |        3.18 | 3.79 | -4.68, 11.04 |  0.84 | 0.410
        hp            |       -0.06 | 0.02 | -0.11, -0.02 | -2.91 | 0.008
      Interactions    |             |      |              |       |      
        gear [4] × vs |       -2.90 | 4.67 | -12.57, 6.78 | -0.62 | 0.541
        gear [5] × vs |        2.59 | 4.54 | -6.82, 12.00 |  0.57 | 0.574
      Controls        |             |      |              |       |      
        gear [4]      |        3.10 | 4.34 | -5.90, 12.10 |  0.71 | 0.482
        gear [5]      |        4.80 | 3.48 | -2.42, 12.01 |  1.38 | 0.182
        drat          |        2.70 | 2.03 |  -1.52, 6.91 |  1.33 | 0.198

---

    Code
      export_table(mp, format = "markdown", row_groups = groups, table_width = Inf)
    Output
      
      
      |Parameter     | Coefficient|   SE|       95% CI| t(22)|     p|
      |:-------------|-----------:|----:|------------:|-----:|-----:|
      |Engine        |            |     |             |      |      |
      |cyl [6]       |       -2.47| 2.21|  -7.05, 2.12| -1.12| 0.276|
      |cyl [8]       |        1.97| 5.11| -8.63, 12.58|  0.39| 0.703|
      |vs            |        3.18| 3.79| -4.68, 11.04|  0.84| 0.410|
      |hp            |       -0.06| 0.02| -0.11, -0.02| -2.91| 0.008|
      |Interactions  |            |     |             |      |      |
      |gear [4] × vs |       -2.90| 4.67| -12.57, 6.78| -0.62| 0.541|
      |gear [5] × vs |        2.59| 4.54| -6.82, 12.00|  0.57| 0.574|
      |Controls      |            |     |             |      |      |
      |gear [4]      |        3.10| 4.34| -5.90, 12.10|  0.71| 0.482|
      |gear [5]      |        4.80| 3.48| -2.42, 12.01|  1.38| 0.182|
      |drat          |        2.70| 2.03|  -1.52, 6.91|  1.33| 0.198|

---

    Code
      export_table(mp, format = "text", row_groups = groups, table_width = Inf,
        align = "llrrlr")
    Output
      Parameter       | Coefficient |   SE |       95% CI | t(22) |     p
      -------------------------------------------------------------------
      Engine          |             |      |              |       |      
        cyl [6]       | -2.47       | 2.21 |  -7.05, 2.12 | -1.12 | 0.276
        cyl [8]       | 1.97        | 5.11 | -8.63, 12.58 | 0.39  | 0.703
        vs            | 3.18        | 3.79 | -4.68, 11.04 | 0.84  | 0.410
        hp            | -0.06       | 0.02 | -0.11, -0.02 | -2.91 | 0.008
      Interactions    |             |      |              |       |      
        gear [4] × vs | -2.90       | 4.67 | -12.57, 6.78 | -0.62 | 0.541
        gear [5] × vs | 2.59        | 4.54 | -6.82, 12.00 | 0.57  | 0.574
      Controls        |             |      |              |       |      
        gear [4]      | 3.10        | 4.34 | -5.90, 12.10 | 0.71  | 0.482
        gear [5]      | 4.80        | 3.48 | -2.42, 12.01 | 1.38  | 0.182
        drat          | 2.70        | 2.03 |  -1.52, 6.91 | 1.33  | 0.198

---

    Code
      export_table(mp, format = "markdown", row_groups = groups, table_width = Inf,
        align = "llrrlr")
    Output
      
      
      |Parameter     |Coefficient |   SE|       95% CI|t(22) |     p|
      |:-------------|:-----------|----:|------------:|:-----|-----:|
      |Engine        |            |     |             |      |      |
      |cyl [6]       |      -2.47 | 2.21|  -7.05, 2.12|-1.12 | 0.276|
      |cyl [8]       |       1.97 | 5.11| -8.63, 12.58| 0.39 | 0.703|
      |vs            |       3.18 | 3.79| -4.68, 11.04| 0.84 | 0.410|
      |hp            |      -0.06 | 0.02| -0.11, -0.02|-2.91 | 0.008|
      |Interactions  |            |     |             |      |      |
      |gear [4] × vs |      -2.90 | 4.67| -12.57, 6.78|-0.62 | 0.541|
      |gear [5] × vs |       2.59 | 4.54| -6.82, 12.00| 0.57 | 0.574|
      |Controls      |            |     |             |      |      |
      |gear [4]      |       3.10 | 4.34| -5.90, 12.10| 0.71 | 0.482|
      |gear [5]      |       4.80 | 3.48| -2.42, 12.01| 1.38 | 0.182|
      |drat          |       2.70 | 2.03|  -1.52, 6.91| 1.33 | 0.198|

---

    Code
      export_table(mp, format = "tt", table_width = Inf)
    Output
      +---------------+-------------+------+--------------+-------+-------+
      | Parameter     | Coefficient | SE   | 95% CI       | t(22) | p     |
      +===============+=============+======+==============+=======+=======+
      | Engine                                                            |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [6]       | -2.47       | 2.21 | -7.05, 2.12  | -1.12 | 0.276 |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [8]       | 1.97        | 5.11 | -8.63, 12.58 | 0.39  | 0.703 |
      +---------------+-------------+------+--------------+-------+-------+
      | vs            | 3.18        | 3.79 | -4.68, 11.04 | 0.84  | 0.410 |
      +---------------+-------------+------+--------------+-------+-------+
      | hp            | -0.06       | 0.02 | -0.11, -0.02 | -2.91 | 0.008 |
      +---------------+-------------+------+--------------+-------+-------+
      | Interactions                                                      |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4] × vs | -2.90       | 4.67 | -12.57, 6.78 | -0.62 | 0.541 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5] × vs | 2.59        | 4.54 | -6.82, 12.00 | 0.57  | 0.574 |
      +---------------+-------------+------+--------------+-------+-------+
      | Controls                                                          |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4]      | 3.10        | 4.34 | -5.90, 12.10 | 0.71  | 0.482 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5]      | 4.80        | 3.48 | -2.42, 12.01 | 1.38  | 0.182 |
      +---------------+-------------+------+--------------+-------+-------+
      | drat          | 2.70        | 2.03 | -1.52, 6.91  | 1.33  | 0.198 |
      +---------------+-------------+------+--------------+-------+-------+ 

---

    Code
      export_table(mp, format = "tt", by = "groups", table_width = Inf)
    Output
      +---------------+-------------+------+--------------+-------+-------+
      | Parameter     | Coefficient | SE   | 95% CI       | t(22) | p     |
      +===============+=============+======+==============+=======+=======+
      | Engine                                                            |
      +---------------+-------------+------+--------------+-------+-------+
      | hp            | -0.06       | 0.02 | -0.11, -0.02 | -2.91 | 0.008 |
      +---------------+-------------+------+--------------+-------+-------+
      | vs            | 3.18        | 3.79 | -4.68, 11.04 | 0.84  | 0.410 |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [6]       | -2.47       | 2.21 | -7.05, 2.12  | -1.12 | 0.276 |
      +---------------+-------------+------+--------------+-------+-------+
      | cyl [8]       | 1.97        | 5.11 | -8.63, 12.58 | 0.39  | 0.703 |
      +---------------+-------------+------+--------------+-------+-------+
      | Controls                                                          |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4]      | 3.10        | 4.34 | -5.90, 12.10 | 0.71  | 0.482 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5]      | 4.80        | 3.48 | -2.42, 12.01 | 1.38  | 0.182 |
      +---------------+-------------+------+--------------+-------+-------+
      | drat          | 2.70        | 2.03 | -1.52, 6.91  | 1.33  | 0.198 |
      +---------------+-------------+------+--------------+-------+-------+
      | Interactions                                                      |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [4] × vs | -2.90       | 4.67 | -12.57, 6.78 | -0.62 | 0.541 |
      +---------------+-------------+------+--------------+-------+-------+
      | gear [5] × vs | 2.59        | 4.54 | -6.82, 12.00 | 0.57  | 0.574 |
      +---------------+-------------+------+--------------+-------+-------+ 

