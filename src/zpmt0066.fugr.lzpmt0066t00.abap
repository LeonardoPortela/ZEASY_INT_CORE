*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0066........................................*
DATA:  BEGIN OF STATUS_ZPMT0066                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0066                      .
CONTROLS: TCTRL_ZPMT0066
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0066                      .
TABLES: ZPMT0066                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
