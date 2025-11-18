*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0065........................................*
DATA:  BEGIN OF STATUS_ZPMT0065                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0065                      .
CONTROLS: TCTRL_ZPMT0065
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0065                      .
TABLES: ZPMT0065                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
