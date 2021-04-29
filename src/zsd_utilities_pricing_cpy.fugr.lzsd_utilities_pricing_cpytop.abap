FUNCTION-POOL ZSD_UTILITIES_PRICING_CPY.    "MESSAGE-ID ..

* INCLUDE LZSD_UTILITIES_PRICING_CPYD...     " Local class definition

*Constants
CONSTANTS: gc_komg(6)        TYPE c   VALUE 'E1KOMG',
           gc_konh(6)        TYPE c   VALUE 'E1KONH',
           gc_konp(6)        TYPE c   VALUE 'E1KONP',
           gc_conda(6)       TYPE c   VALUE 'COND_A',
           gc_sap(3)         TYPE c   VALUE 'SAP',
           gc_2(1)           TYPE c   VALUE '2',
           gc_6(1)           TYPE c   VALUE '6',
           gc_conda04(8)     TYPE c   VALUE 'COND_A04',
           gc_ls(2)          TYPE c   VALUE 'LS',
           gc_cond(4)        TYPE c   VALUE 'COND',
           gc_msgtyp_infrec  TYPE edi_mestyp VALUE 'INFREC',
           gc_error          TYPE c VALUE 'E',
           gc_success        TYPE c VALUE 'S',
           gc_warning        TYPE c VALUE 'W',
           gc_idoctyp_infrec TYPE char9 VALUE 'INFREC02',
           gc_idocext_infrec TYPE char10 VALUE 'ZINFREC02',
           gc_code           TYPE char4 VALUE 'INFR',
           gc_e1einam        TYPE char7 VALUE 'E1EINAM',
           gc_e1einem        TYPE char7 VALUE 'E1EINEM',
           gc_auts           TYPE char9 VALUE 'Z1MM_AUTS'.
