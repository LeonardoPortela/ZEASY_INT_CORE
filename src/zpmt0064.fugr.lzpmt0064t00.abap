*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0064........................................*
DATA:  BEGIN OF STATUS_ZPMT0064                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0064                      .
CONTROLS: TCTRL_ZPMT0064
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0064                      .
TABLES: ZPMT0064                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
