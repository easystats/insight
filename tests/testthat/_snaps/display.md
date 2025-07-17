# display, matrix

    Code
      as.character(display(mdat))
    Output
      [1] "|     | C.1| C.2| C.3|" "|:----|---:|---:|---:|" "|row1 |   1|   2|   3|"
      [4] "|row2 |  11|  12|  13|"

---

    Code
      as.character(print_md(mdat))
    Output
      [1] "|     | C.1| C.2| C.3|" "|:----|---:|---:|---:|" "|row1 |   1|   2|   3|"
      [4] "|row2 |  11|  12|  13|"

# display, table

    Code
      as.character(display(res))
    Output
      [1] "|    | hiphop| salsa|" "|:---|------:|-----:|" "|no  |  -0.99|  0.96|"
      [4] "|yes |   0.91| -0.89|"

---

    Code
      as.character(print_md(res))
    Output
      [1] "|    | hiphop| salsa|" "|:---|------:|-----:|" "|no  |  -0.99|  0.96|"
      [4] "|yes |   0.91| -0.89|"

