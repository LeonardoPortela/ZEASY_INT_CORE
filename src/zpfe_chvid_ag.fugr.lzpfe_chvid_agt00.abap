*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPFE_CHVID_AG...................................*
DATA:  BEGIN OF STATUS_ZPFE_CHVID_AG                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPFE_CHVID_AG                 .
CONTROLS: TCTRL_ZPFE_CHVID_AG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPFE_CHVID_AG                 .
TABLES: ZPFE_CHVID_AG                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
