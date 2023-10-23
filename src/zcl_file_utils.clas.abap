*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature
*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
class ZCL_FILE_UTILS definition
  public
  final
  create public .

*"* public components of class ZCL_FILE_UTILS
*"* do not include other source files here!!!
*"* protected components of class ZCL_UTILIDADES_ARCHIVOS
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

    class-methods:
        add_slash_at_end
            importing
                iv_path type csequence
                iv_local type abap_bool
            returning
                value(rv_result) type string.

  class-methods GET_XLS_LINE_FROM_TABLE
    importing
      !IT_DATOS type TABLE
    returning
      value(EV_LINEA) type STRING .
  class-methods GET_XLS_LINE
    importing
      !IS_ESTRUCTURA type DATA
    returning
      value(EV_LINEA) type STRING .
  class-methods ADD_XLS_LINE_TO_TABLE
    importing
      !IV_LINEA type STRING
    changing
      !ET_DATOS type TABLE .
  class-methods DISPLAY_FILE_BROWSER
    importing
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE
    changing
      !EV_FILE type CSEQUENCE .
  class-methods DISPLAY_DIRECTORY_BROWSER
    importing
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE
    changing
      !CV_DIRECTORY type CSEQUENCE .
  class-methods VALIDAR_ARCHIVO
    importing
      !IV_ARCHIVO type C
      !IV_TIPO_MENSAJE_BATCH type BAPI_MTYPE default 'S'
    returning
      value(EV_EXISTE) type ABAP_BOOL .
  class-methods VALIDATE_PATH
    importing
      !IV_DIRECTORY type C
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE .
  class-methods GET_FILE_PATH
    importing
      !IV_FILE type C
    changing
      !CV_DIRECTORY type C .
  class-methods SAVE_FILE
    importing
      !IT_DATA type TABLE
      !IV_FILE type CSEQUENCE
      !IV_REMOVE_LINE_WHITE_SPACES type ABAP_BOOL default ABAP_TRUE
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE .
  class-methods VALIDATE_FILE_PATH
    importing
      !IV_FILE type C
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE .
  class-methods GET_DATOS_ARCHIVO_BINARIO
    importing
      !IV_ARCHIVO type C
    changing
      !ET_DATOS type TABLE
      !EV_TAMANIO type I .
  class-methods GET_DATOS_ARCHIVO_TEXTO
    importing
      !IV_ARCHIVO type CSEQUENCE
    changing
      !ET_DATOS type TABLE .
  class-methods GET_DATOS_ARCHIVO_XLS
    importing
      !IV_ARCHIVO type C
    changing
      !ET_DATOS type TABLE .
  class-methods GET_ARCHIVOS_DIRECTORIO
    importing
      !IV_DIRECTORIO type CSEQUENCE
      !IV_CONCATENAR_RUTA type ABAP_BOOL default ABAP_TRUE
    changing
      !ET_ARCHIVOS type STRINGTAB .
  class-methods SAVE_TAB_SEPARATED_FILE
    importing
      !IT_DATA type TABLE
      !IV_FILE type CSEQUENCE
      !IT_HEADER type STRINGTAB optional
      !IV_LOCAL_FILE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_FILE_ERROR .
  class-methods SAVE_BINARY_FILE
    importing
      !IT_DATA type TABLE
      !IV_FILE type CSEQUENCE
      !IV_LENGTH type I .
  class-methods MOVE
    importing
      !IV_ARCHIVO type CSEQUENCE
      !IV_RUTA_DESTINO type CSEQUENCE .
  class-methods BORRAR_ARCHIVO
    importing
      !IV_ARCHIVO type CSEQUENCE .
  class-methods GET_NOMBRE_ARCHIVO
    importing
      !IV_ARCHIVO type CSEQUENCE
    changing
      !EV_NOMBRE_ARCHIVO type CSEQUENCE .
  class-methods COMPRIMIR_ARCHIVO
    importing
      !IV_ARCHIVO type CSEQUENCE .
  class-methods GET_DATOS_TEXTO_FROM_BINARIO
    importing
      !IV_ARCHIVO type CSEQUENCE
    changing
      !ET_DATOS type TABLE .
protected section.
*"* private components of class ZCL_FILE_UTILS
*"* do not include other source files here!!!
private section.

ENDCLASS.



CLASS ZCL_FILE_UTILS IMPLEMENTATION.


    method add_slash_at_end.

        if iv_path is not INITIAL.

          DATA(lv_slash) = cond #( WHEN iv_local = 'X' then '\' else '/' ).
          DATA(lv_dir_last_char_offset) = strlen( iv_path ) - 1.
          DATA(lv_dir_has_slash) = cond #( when iv_path+lv_dir_last_char_offset(1) eq lv_slash then abap_true else abap_false ).
          rv_result = |{ iv_path }{ cond #( when lv_dir_has_slash eq abap_true then '' else LV_SLASH ) }|.

        ENDIF.

    endmethod.


    METHOD ADD_XLS_LINE_TO_TABLE.

      FIELD-SYMBOLS:
        <LV_VALOR_CAMPO> TYPE STRING,
        <LS_LINEA> TYPE DATA,
        <LV_CAMPO> TYPE DATA.

      DATA:
        LT_VALORES_CAMPOS TYPE STRINGTAB.

*     Se obtienen los valores de los campos a partir de la línea XLS.
      SPLIT IV_LINEA AT CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB INTO TABLE
        LT_VALORES_CAMPOS.

*     Se agrega una línea a la tabla.
      APPEND INITIAL LINE TO ET_DATOS ASSIGNING <LS_LINEA>.

*     Se itera sobre los valores de los campos.
      LOOP AT LT_VALORES_CAMPOS ASSIGNING <LV_VALOR_CAMPO>.

        CHECK <LV_VALOR_CAMPO> IS NOT INITIAL.

*     Se obtiene el campo correspondiente de la línea agregada a la tabla.
        ASSIGN COMPONENT SY-TABIX OF STRUCTURE <LS_LINEA> TO
          <LV_CAMPO>.

*     Se asigna el valor a dicho campo.
        <LV_CAMPO> = <LV_VALOR_CAMPO>.

      ENDLOOP.


    ENDMETHOD.


    method BORRAR_ARCHIVO.

      DATA:
        LV_ARCHIVO TYPE STRING,
        LV_RC TYPE I.

      IF SY-BATCH IS INITIAL.

        LV_ARCHIVO = IV_ARCHIVO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
          EXPORTING
            FILENAME             = LV_ARCHIVO
          CHANGING
            RC                   = LV_RC
          EXCEPTIONS
            FILE_DELETE_FAILED   = 1
            CNTL_ERROR           = 2
            ERROR_NO_GUI         = 3
            FILE_NOT_FOUND       = 4
            ACCESS_DENIED        = 5
            UNKNOWN_ERROR        = 6
            NOT_SUPPORTED_BY_GUI = 7
            WRONG_PARAMETER      = 8
            others               = 9
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


      ELSE.

        DELETE DATASET IV_ARCHIVO.

      ENDIF.

    endmethod.


    method COMPRIMIR_ARCHIVO.

      DATA:
        LV_COMANDO(1024) TYPE C.

      CALL 'C_SAPGPARAM'
        ID 'NAME'  FIELD 'DIR_BINARY'
        ID 'VALUE' FIELD LV_COMANDO.

      CONCATENATE
        LV_COMANDO
        '\MKSZIP.EXE -f'
      INTO
        LV_COMANDO.

      CONCATENATE
        LV_COMANDO
        IV_ARCHIVO
      INTO
        LV_COMANDO
      SEPARATED BY
        SPACE.

      CALL 'SYSTEM'
        ID 'COMMAND' FIELD LV_COMANDO.

      IF SY-SUBRC NE 0.
        MESSAGE E073(ZBC00) WITH IV_ARCHIVO.
      ENDIF.

    endmethod.


    method DISPLAY_DIRECTORY_BROWSER.

      DATA:
        LV_DIRECTORIO TYPE STRING.

      IF IV_LOCAL_FILE EQ ABAP_TRUE.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
*          EXPORTING
*            WINDOW_TITLE         =
*            INITIAL_FOLDER       =
          CHANGING
            SELECTED_FOLDER      = LV_DIRECTORIO
*          EXCEPTIONS
*            CNTL_ERROR           = 1
*            ERROR_NO_GUI         = 2
*            NOT_SUPPORTED_BY_GUI = 3
*            others               = 4
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        CV_DIRECTORY = LV_DIRECTORIO.

      ELSE.

        ZCL_FILE_UTILS=>DISPLAY_FILE_BROWSER(
          EXPORTING
            IV_LOCAL_FILE = IV_LOCAL_FILE
          CHANGING
            EV_FILE = CV_DIRECTORY
          ).


      ENDIF.

    endmethod.


    method DISPLAY_FILE_BROWSER.

      DATA:
        LT_ARCHIVO TYPE FILETABLE,
        LS_ARCHIVO TYPE FILE_TABLE,
        LV_RESULTADO TYPE I.

      IF IV_LOCAL_FILE EQ ABAP_TRUE.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
          EXPORTING
*            WINDOW_TITLE            =
*            DEFAULT_EXTENSION       =
*            DEFAULT_FILENAME        =
*            FILE_FILTER             =
*            WITH_ENCODING           =
*            INITIAL_DIRECTORY       =
            MULTISELECTION          = ABAP_FALSE
          CHANGING
            FILE_TABLE              = LT_ARCHIVO
            RC                      = LV_RESULTADO
*            USER_ACTION             =
*            FILE_ENCODING           =
*          EXCEPTIONS
*            FILE_OPEN_DIALOG_FAILED = 1
*            CNTL_ERROR              = 2
*            ERROR_NO_GUI            = 3
*            NOT_SUPPORTED_BY_GUI    = 4
*            others                  = 5
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSEIF LV_RESULTADO EQ 1.
          READ TABLE LT_ARCHIVO INTO LS_ARCHIVO INDEX 1.
          EV_FILE = LS_ARCHIVO-FILENAME.
        ENDIF.

      ELSE.

        CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
*         EXPORTING
*           DIRECTORY              = ' '
*           FILEMASK               = ' '
          IMPORTING
            SERVERFILE             = EV_FILE
          EXCEPTIONS
            CANCELED_BY_USER       = 1
            OTHERS                 = 2
                  .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


      ENDIF.

    endmethod.


    method GET_ARCHIVOS_DIRECTORIO.

      DATA:
        LV_DIRECTORIO TYPE STRING,
        LV_CANTIDAD_ARCHIVOS TYPE I,
        LT_FILE_INFO TYPE STANDARD TABLE OF FILE_INFO,
        LS_FILE_INFO TYPE FILE_INFO,
        LV_ARCHIVO TYPE STRING,
        LV_NAME TYPE PATHEXTERN,
        LV_TYPE(10) TYPE C.

      IF SY-BATCH EQ SPACE.

        LV_DIRECTORIO = IV_DIRECTORIO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
          EXPORTING
            DIRECTORY                   = LV_DIRECTORIO
*            FILTER                      = '*.*'
            FILES_ONLY                  = ABAP_TRUE
*            DIRECTORIES_ONLY            =
          CHANGING
            FILE_TABLE                  = LT_FILE_INFO
            COUNT                       = LV_CANTIDAD_ARCHIVOS
          EXCEPTIONS
            CNTL_ERROR                  = 1
            DIRECTORY_LIST_FILES_FAILED = 2
            WRONG_PARAMETER             = 3
            ERROR_NO_GUI                = 4
            NOT_SUPPORTED_BY_GUI        = 5
            others                      = 6
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.

          LOOP AT LT_FILE_INFO INTO LS_FILE_INFO.

            IF IV_CONCATENAR_RUTA EQ ABAP_TRUE.
              CONCATENATE
                LV_DIRECTORIO
                '\'
                LS_FILE_INFO-FILENAME
              INTO
                LV_ARCHIVO.
            ELSE.
              LV_ARCHIVO = LS_FILE_INFO-FILENAME.
            ENDIF.

            APPEND LV_ARCHIVO TO ET_ARCHIVOS.

          ENDLOOP.

        ENDIF.

      ELSE.

        CALL 'C_DIR_READ_START'
          ID 'DIR' FIELD IV_DIRECTORIO
          ID 'FILE' FIELD SPACE.

        DO.

          CALL 'C_DIR_READ_NEXT'
            ID 'TYPE'   FIELD LV_TYPE
            ID 'NAME'   FIELD LV_NAME.

          IF SY-SUBRC EQ 1.
            EXIT.
          ENDIF.

          CHECK LV_TYPE EQ 'file, regu'.

          IF IV_CONCATENAR_RUTA EQ ABAP_TRUE.
            CONCATENATE
              IV_DIRECTORIO
              '\'
              LV_NAME
            INTO
              LV_ARCHIVO.
          ELSE.
            LV_ARCHIVO = LV_NAME.
          ENDIF.

          APPEND LV_ARCHIVO TO ET_ARCHIVOS.

        ENDDO.

        CALL 'C_DIR_READ_FINISH'.

      ENDIF.

    endmethod.


    method GET_DATOS_ARCHIVO_BINARIO .

      FIELD-SYMBOLS:
        <LS_LINEA_TABLA> TYPE DATA.

      DATA:
        LV_BYTES_LEIDOS TYPE I,
        LV_ARCHIVO TYPE STRING.

*     En caso de estar corriendo de fondo...
      IF SY-BATCH EQ 'X'.

*     Se abre el archivo.
        OPEN DATASET IV_ARCHIVO FOR INPUT IN BINARY MODE.

*     Se verifica se haya abierto el archivo.
        CHECK SY-SUBRC EQ 0.

        EV_TAMANIO = 0.

        DO.

*     Se agrega una linea a la linea que tendrá los datos.
          APPEND INITIAL LINE TO ET_DATOS ASSIGNING <LS_LINEA_TABLA>.

*     Se lee una linea del archivo.
          READ DATASET IV_ARCHIVO INTO <LS_LINEA_TABLA> LENGTH LV_BYTES_LEIDOS.

*     En caso de no haber tenido exito...
          IF SY-SUBRC NE 0.

*     En caso de haber leído algo...
            IF LV_BYTES_LEIDOS GT 0.

*     ...se incrementa la cantidad de bytes.
              EV_TAMANIO = EV_TAMANIO + LV_BYTES_LEIDOS.
            ELSE.

*     Sino se elimina la última linea del archivo.
              DELETE ET_DATOS INDEX SY-INDEX.
            ENDIF.

            EXIT.
          ENDIF.

*     Se incrementa la cantidad de bytes.
          EV_TAMANIO = EV_TAMANIO + LV_BYTES_LEIDOS.

        ENDDO.

*     Se cierra el archivo.
        CLOSE DATASET IV_ARCHIVO.

      ELSE.

        LV_ARCHIVO = IV_ARCHIVO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
          EXPORTING
            FILENAME                = LV_ARCHIVO
            FILETYPE                = 'BIN'
*            HAS_FIELD_SEPARATOR     = SPACE
*            HEADER_LENGTH           = 0
*            READ_BY_LINE            = 'X'
*            DAT_MODE                = SPACE
*            CODEPAGE                = SPACE
*            IGNORE_CERR             = ABAP_TRUE
*            REPLACEMENT             = '#'
*            VIRUS_SCAN_PROFILE      =
          IMPORTING
            FILELENGTH              = EV_TAMANIO
*            HEADER                  =
          CHANGING
            DATA_TAB                = ET_DATOS
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            NOT_SUPPORTED_BY_GUI    = 17
            ERROR_NO_GUI            = 18
            others                  = 19
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    endmethod.


    method GET_DATOS_ARCHIVO_TEXTO.


      FIELD-SYMBOLS:
        <LS_LINEA_TABLA> TYPE DATA.

      DATA:
        LV_BYTES_LEIDOS TYPE I,
        LV_ARCHIVO TYPE STRING.

*     Se verifica si el proceso es de fondo
      IF SY-BATCH EQ 'X'.

*     Apertura de archivo.
        OPEN DATASET IV_ARCHIVO FOR INPUT IN LEGACY TEXT MODE CODE PAGE '1160'.

*     Verifica algún error con la apertura del archivo.
        CHECK SY-SUBRC EQ 0.

        DO.

*     Se agrega una linea a la linea que tendrá los datos.
          APPEND INITIAL LINE TO ET_DATOS ASSIGNING <LS_LINEA_TABLA>.

*     Se lee una linea del archivo.
          READ DATASET IV_ARCHIVO INTO <LS_LINEA_TABLA> LENGTH LV_BYTES_LEIDOS.

*     En caso de no haber tenido exito...
          IF SY-SUBRC NE 0.

*     En caso de no haber leído nada
            IF LV_BYTES_LEIDOS EQ 0.
*     Sino se elimina la última linea del archivo.
              DELETE ET_DATOS INDEX SY-INDEX.
            ENDIF.

            EXIT.
          ENDIF.

*     Se incrementa la cantidad de bytes.
        ENDDO.

*     Se cierra el archivo.
        CLOSE DATASET IV_ARCHIVO.

      ELSE.

        LV_ARCHIVO = IV_ARCHIVO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
          EXPORTING
            FILENAME                = LV_ARCHIVO
            FILETYPE                = 'ASC'
*            HAS_FIELD_SEPARATOR     = SPACE
*            HEADER_LENGTH           = 0
*            READ_BY_LINE            = 'X'
*            DAT_MODE                = SPACE
*            CODEPAGE                = SPACE
*            IGNORE_CERR             = ABAP_TRUE
*            REPLACEMENT             = '#'
*            VIRUS_SCAN_PROFILE      =
*         IMPORTING
*            FILELENGTH              = EV_TAMANIO
*            HEADER                  =
          CHANGING
            DATA_TAB                = ET_DATOS
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            NOT_SUPPORTED_BY_GUI    = 17
            ERROR_NO_GUI            = 18
            others                  = 19
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.


    endmethod.


    method GET_DATOS_ARCHIVO_XLS .

      FIELD-SYMBOLS:
        <LS_LINEA_TABLA> TYPE DATA.

      DATA:
        LV_BYTES_LEIDOS TYPE I,
        LV_ARCHIVO TYPE STRING,
        LV_LINEA TYPE STRING.

*     Se verifica si el proceso es de fondo
      IF SY-BATCH EQ 'X'.

*     Apertura de archivo.
        OPEN DATASET IV_ARCHIVO FOR INPUT IN LEGACY TEXT MODE CODE PAGE '1160'.

*     Verifica algún error con la apertura del archivo.
        CHECK SY-SUBRC EQ 0.

        DO.

*     Se lee una linea del archivo.
          READ DATASET IV_ARCHIVO INTO LV_LINEA LENGTH LV_BYTES_LEIDOS.

*     En caso de no haber tenido exito...
          IF SY-SUBRC NE 0.

*     En caso de haber leído.
            IF LV_BYTES_LEIDOS NE 0.

              CALL METHOD ZCL_FILE_UTILS=>ADD_XLS_LINE_TO_TABLE
                EXPORTING
                  IV_LINEA = LV_LINEA
                CHANGING
                  ET_DATOS = ET_DATOS.

            ENDIF.

            EXIT.
          ENDIF.

        CALL METHOD ZCL_FILE_UTILS=>ADD_XLS_LINE_TO_TABLE
          EXPORTING
            IV_LINEA = LV_LINEA
          CHANGING
            ET_DATOS = ET_DATOS.

        ENDDO.

*     Se cierra el archivo.
        CLOSE DATASET IV_ARCHIVO.

      ELSE.

        LV_ARCHIVO = IV_ARCHIVO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
          EXPORTING
            FILENAME                = LV_ARCHIVO
            FILETYPE                = 'ASC'
            HAS_FIELD_SEPARATOR     = 'X'
*            HEADER_LENGTH           = 0
*            READ_BY_LINE            = 'X'
*            DAT_MODE                = SPACE
*            CODEPAGE                = SPACE
*            IGNORE_CERR             = ABAP_TRUE
*            REPLACEMENT             = '#'
*            VIRUS_SCAN_PROFILE      =
*         IMPORTING
*            FILELENGTH              = EV_TAMANIO
*            HEADER                  =
          CHANGING
            DATA_TAB                = ET_DATOS
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            NOT_SUPPORTED_BY_GUI    = 17
            ERROR_NO_GUI            = 18
            others                  = 19
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    endmethod.


    METHOD GET_DATOS_TEXTO_FROM_BINARIO.

      DATA:
        LV_XLINEA TYPE XSTRING,
        LV_LINEA TYPE STRING,
        LV_CONVERSOR TYPE REF TO CL_ABAP_CONV_IN_CE,
        LV_ARCHIVO TYPE STRING.

      IF SY-BATCH EQ 'X'.

        OPEN DATASET IV_ARCHIVO FOR INPUT IN BINARY MODE.

        READ DATASET IV_ARCHIVO INTO LV_XLINEA.

        CLOSE DATASET IV_ARCHIVO.

        LV_CONVERSOR = CL_ABAP_CONV_IN_CE=>CREATE( INPUT = LV_XLINEA ).

        LV_CONVERSOR->READ( IMPORTING DATA = LV_LINEA ).

        SPLIT LV_LINEA
        AT CL_ABAP_CHAR_UTILITIES=>CR_LF
        INTO TABLE ET_DATOS
        IN CHARACTER MODE.

      ELSE.

        LV_ARCHIVO = IV_ARCHIVO.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
          EXPORTING
            FILENAME                = LV_ARCHIVO
            FILETYPE                = 'ASC'
*            HAS_FIELD_SEPARATOR     = SPACE
*            HEADER_LENGTH           = 0
*            READ_BY_LINE            = 'X'
*            DAT_MODE                = SPACE
*            CODEPAGE                = SPACE
*            IGNORE_CERR             = ABAP_TRUE
*            REPLACEMENT             = '#'
*            VIRUS_SCAN_PROFILE      =
*         IMPORTING
*            FILELENGTH              = EV_TAMANIO
*            HEADER                  =
          CHANGING
            DATA_TAB                = ET_DATOS
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            NOT_SUPPORTED_BY_GUI    = 17
            ERROR_NO_GUI            = 18
            OTHERS                  = 19
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    ENDMETHOD.


    method GET_FILE_PATH.

      DATA:
        LV_MATCH_OFFSET TYPE I.

      LV_MATCH_OFFSET = 0.

      DO.

*     Se busca el caracter '\' a partir de la posición LV_MATCH_OFFSET en
*     IV_ARCHIVO
        FIND
          '\'
        IN SECTION
          OFFSET
            LV_MATCH_OFFSET
        OF
          IV_FILE
        IN
          CHARACTER MODE
        MATCH OFFSET
          LV_MATCH_OFFSET.

        IF SY-SUBRC NE 0.
          EXIT.
        ENDIF.

*     Se agrega uno a LV_MATCH_OFFSET de forma de no seguir apuntando a el
*     caracter '\'.
        LV_MATCH_OFFSET = LV_MATCH_OFFSET + 1.

      ENDDO.

      CHECK LV_MATCH_OFFSET GT 0.

*     Se obtiene la ruta del archivo.
      CV_DIRECTORY = IV_FILE(LV_MATCH_OFFSET).

    endmethod.


    method GET_NOMBRE_ARCHIVO.

      DATA:
        LT_PARTES_ARCHIVO TYPE STRINGTAB,
        LV_LINEAS TYPE I.

      SPLIT IV_ARCHIVO AT '\' INTO TABLE LT_PARTES_ARCHIVO
        IN CHARACTER MODE.

      DESCRIBE TABLE LT_PARTES_ARCHIVO LINES LV_LINEAS.

      READ TABLE LT_PARTES_ARCHIVO
      INTO EV_NOMBRE_ARCHIVO
      INDEX LV_LINEAS.

    endmethod.


    method GET_XLS_LINE.

      FIELD-SYMBOLS:
        <LV_VALOR_CAMPO> TYPE STRING,
        <LS_LINEA> TYPE DATA,
        <LV_CAMPO> TYPE DATA.

      DATA:
        LV_TIPO(1) TYPE C,
        LV_CANTIDAD_CAMPOS TYPE I,
        LV_VALOR_CAMPO TYPE STRING.

*     Se obtiene la cantidad de campos.
      DESCRIBE FIELD IS_ESTRUCTURA TYPE LV_TIPO COMPONENTS LV_CANTIDAD_CAMPOS.

*     En caso de no ser una estructura se asigna directamente el valor a la línea.
      IF LV_CANTIDAD_CAMPOS EQ 0.
        EV_LINEA = IS_ESTRUCTURA.
      ENDIF.

*     Se itera sobre los campos de la estructura
      DO LV_CANTIDAD_CAMPOS TIMES.

        ASSIGN COMPONENT SY-INDEX OF STRUCTURE IS_ESTRUCTURA TO
          <LV_CAMPO>.

*     En caso de ser el primero se asigna al principio de la línea.
        IF SY-INDEX EQ 1.
          EV_LINEA = <LV_CAMPO>.

*     Sino se concatena separado por un tabulador.
        ELSE.

          LV_VALOR_CAMPO = <LV_CAMPO>.

          CONCATENATE
            EV_LINEA
            LV_VALOR_CAMPO
          INTO
            EV_LINEA
          SEPARATED BY
            CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

        ENDIF.

      ENDDO.

    endmethod.


    method GET_XLS_LINE_FROM_TABLE.

      FIELD-SYMBOLS:
        <LV_CAMPO> TYPE DATA.

      DATA:
        LV_VALOR_CAMPO TYPE STRING.

*     Se itera sobre los campos de la estructura
      LOOP AT IT_DATOS ASSIGNING <LV_CAMPO>.

*     En caso de ser el primero se asigna al principio de la línea.
        IF SY-TABIX EQ 1.

          EV_LINEA = <LV_CAMPO>.

*     Sino se concatena separado por un tabulador.
        ELSE.

          LV_VALOR_CAMPO = <LV_CAMPO>.

          CONCATENATE
            EV_LINEA
            <LV_CAMPO>
          INTO
            EV_LINEA
          SEPARATED BY
            CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

        ENDIF.

      ENDLOOP.

    endmethod.


    METHOD MOVE.

      DATA:
        LV_ARCHIVO TYPE STRING,
        LV_RUTA TYPE STRING,
        LV_COMANDO(1024) TYPE C.

      CONCATENATE
        '"'
        IV_ARCHIVO
        '"'
      INTO
        LV_ARCHIVO.

      CONCATENATE
        '"'
        IV_RUTA_DESTINO
        '"'
      INTO
        LV_RUTA.

      CONCATENATE
        'MOVE /Y'
        LV_ARCHIVO
        LV_RUTA
      INTO
        LV_COMANDO
      SEPARATED BY
        SPACE.

      CALL 'SYSTEM'
        ID 'COMMAND' FIELD LV_COMANDO.

    ENDMETHOD.


    METHOD SAVE_BINARY_FILE .

      FIELD-SYMBOLS:
        <LF_LINEA> TYPE DATA.

      DATA:
        LV_ARCHIVO TYPE STRING,
        LV_LINEAS TYPE I,
        LV_LONGITUD_LINEA TYPE I,
        LV_BYTES_ULTIMA_LINEA TYPE I.


      IF SY-BATCH IS INITIAL.
        MOVE IV_FILE TO LV_ARCHIVO.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            BIN_FILESIZE                    = IV_LENGTH
            FILENAME                        = LV_ARCHIVO
            FILETYPE                        = 'BIN'
*           APPEND                          = ' '
*           WRITE_FIELD_SEPARATOR           = ' '
*           HEADER                          = '00'
*           TRUNC_TRAILING_BLANKS           = ' '
*           WRITE_LF                        = 'X'
*           COL_SELECT                      = ' '
*           COL_SELECT_MASK                 = ' '
*           DAT_MODE                        = ' '
*           CONFIRM_OVERWRITE               = ' '
*           NO_AUTH_CHECK                   = ' '
*           CODEPAGE                        = ' '
*           IGNORE_CERR                     = ABAP_TRUE
*           REPLACEMENT                     = '#'
*           WRITE_BOM                       = ' '
*           TRUNC_TRAILING_BLANKS_EOL       = 'X'
*           WK1_N_FORMAT                    = ' '
*           WK1_N_SIZE                      = ' '
*           WK1_T_FORMAT                    = ' '
*           WK1_T_SIZE                      = ' '
*         IMPORTING
*           FILELENGTH                      =
          TABLES
            DATA_TAB                        = IT_DATA
*           FIELDNAMES                      =
          EXCEPTIONS
            FILE_WRITE_ERROR                = 1
            NO_BATCH                        = 2
            GUI_REFUSE_FILETRANSFER         = 3
            INVALID_TYPE                    = 4
            NO_AUTHORITY                    = 5
            UNKNOWN_ERROR                   = 6
            HEADER_NOT_ALLOWED              = 7
            SEPARATOR_NOT_ALLOWED           = 8
            FILESIZE_NOT_ALLOWED            = 9
            HEADER_TOO_LONG                 = 10
            DP_ERROR_CREATE                 = 11
            DP_ERROR_SEND                   = 12
            DP_ERROR_WRITE                  = 13
            UNKNOWN_DP_ERROR                = 14
            ACCESS_DENIED                   = 15
            DP_OUT_OF_MEMORY                = 16
            DISK_FULL                       = 17
            DP_TIMEOUT                      = 18
            FILE_NOT_FOUND                  = 19
            DATAPROVIDER_EXCEPTION          = 20
            CONTROL_FLUSH_ERROR             = 21
            OTHERS                          = 22
                  .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ELSE.

        OPEN DATASET IV_FILE FOR OUTPUT IN BINARY MODE.

        IF SY-SUBRC NE 0.
          MESSAGE 'No se pudo abrir el archivo ' TYPE 'E'.
        ENDIF.

*     Se guardan todas las líneas de la tabla pasada menos la última.
        LOOP AT IT_DATA ASSIGNING <LF_LINEA>.
          AT LAST.
            EXIT.
          ENDAT.
          TRANSFER <LF_LINEA> TO IV_FILE.
        ENDLOOP.

*     Se obtiene la cantidad de líneas de la tabla.
        DESCRIBE TABLE IT_DATA LINES LV_LINEAS.
        DESCRIBE FIELD <LF_LINEA> LENGTH LV_LONGITUD_LINEA IN BYTE MODE.

*     Se obtiene la cantidad de bytes a transferir de la última línea.
        LV_BYTES_ULTIMA_LINEA = IV_LENGTH -
          ( ( LV_LINEAS - 1 ) * LV_LONGITUD_LINEA ).

        TRANSFER <LF_LINEA> TO IV_FILE LENGTH LV_BYTES_ULTIMA_LINEA.

        CLOSE DATASET IV_FILE.

      ENDIF.

    ENDMETHOD.


    METHOD SAVE_FILE.

      DATA:
        LV_ARCHIVO TYPE STRING,
        LV_PRIMERA_LINEA TYPE ABAP_BOOL,
        LV_LONGITUD_LINEA TYPE I.

      FIELD-SYMBOLS:
        <LF_LINEA> TYPE DATA.


      IF IV_LOCAL_FILE EQ ABAP_TRUE AND SY-BATCH IS INITIAL.

        MOVE IV_FILE TO LV_ARCHIVO.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
*           BIN_FILESIZE                    =
            FILENAME                        = LV_ARCHIVO
*           FILETYPE                        = 'ASC'
*           APPEND                          = ' '
*           WRITE_FIELD_SEPARATOR           = ' '
*           HEADER                          = '00'
*           TRUNC_TRAILING_BLANKS           = ' '
*           WRITE_LF                        = 'X'
*           COL_SELECT                      = ' '
*           COL_SELECT_MASK                 = ' '
*           DAT_MODE                        = ' '
*           CONFIRM_OVERWRITE               = ' '
*           NO_AUTH_CHECK                   = ' '
*           CODEPAGE                        = ' '
*           IGNORE_CERR                     = ABAP_TRUE
*           REPLACEMENT                     = '#'
*           WRITE_BOM                       = ' '
            TRUNC_TRAILING_BLANKS_EOL       = IV_REMOVE_LINE_WHITE_SPACES
*           WK1_N_FORMAT                    = ' '
*           WK1_N_SIZE                      = ' '
*           WK1_T_FORMAT                    = ' '
*           WK1_T_SIZE                      = ' '
*         IMPORTING
*           FILELENGTH                      =
          TABLES
            DATA_TAB                        = IT_DATA
*           FIELDNAMES                      =
          EXCEPTIONS
            FILE_WRITE_ERROR                = 1
            NO_BATCH                        = 2
            GUI_REFUSE_FILETRANSFER         = 3
            INVALID_TYPE                    = 4
            NO_AUTHORITY                    = 5
            UNKNOWN_ERROR                   = 6
            HEADER_NOT_ALLOWED              = 7
            SEPARATOR_NOT_ALLOWED           = 8
            FILESIZE_NOT_ALLOWED            = 9
            HEADER_TOO_LONG                 = 10
            DP_ERROR_CREATE                 = 11
            DP_ERROR_SEND                   = 12
            DP_ERROR_WRITE                  = 13
            UNKNOWN_DP_ERROR                = 14
            ACCESS_DENIED                   = 15
            DP_OUT_OF_MEMORY                = 16
            DISK_FULL                       = 17
            DP_TIMEOUT                      = 18
            FILE_NOT_FOUND                  = 19
            DATAPROVIDER_EXCEPTION          = 20
            CONTROL_FLUSH_ERROR             = 21
            OTHERS                          = 22
                  .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ELSE.

        OPEN DATASET IV_FILE FOR OUTPUT ENCODING DEFAULT IN TEXT MODE.

        IF SY-SUBRC NE 0.
          MESSAGE 'No se pudo abrir el archivo' TYPE 'E'.
        ENDIF.

        IF IV_REMOVE_LINE_WHITE_SPACES EQ ABAP_TRUE.

          LOOP AT IT_DATA ASSIGNING <LF_LINEA>.
            TRANSFER <LF_LINEA> TO IV_FILE.
          ENDLOOP.

        ELSE.

          LV_PRIMERA_LINEA = ABAP_TRUE.

          LOOP AT IT_DATA ASSIGNING <LF_LINEA>.

            IF LV_PRIMERA_LINEA EQ ABAP_TRUE.
              DESCRIBE FIELD <LF_LINEA> LENGTH LV_LONGITUD_LINEA IN CHARACTER MODE.
            ENDIF.

            TRANSFER <LF_LINEA> TO IV_FILE LENGTH LV_LONGITUD_LINEA.

            AT FIRST.
              LV_PRIMERA_LINEA = ABAP_FALSE.
            ENDAT.

          ENDLOOP.

        ENDIF.

        CLOSE DATASET IV_FILE.

      ENDIF.

    ENDMETHOD.


    method SAVE_TAB_SEPARATED_FILE.

      DATA:
        lv_archivo TYPE string,
        lv_linea TYPE string,
        LV_HEADER TYPE STRING,
        LV_COLUMN_DESCRIPTION TYPE STRING,
        LT_HEADER TYPE STRINGTAB,
        LV_APPEND TYPE ABAP_BOOL.

      FIELD-SYMBOLS:
        <lf_linea> TYPE data.


      IF IV_LOCAL_FILE eq ABAP_TRUE AND SY-BATCH IS INITIAL.

        MOVE IV_file TO lv_archivo.

        IF IT_HEADER IS NOT INITIAL.

          LOOP AT IT_HEADER INTO LV_COLUMN_DESCRIPTION.

            IF LV_HEADER IS INITIAL.

              LV_HEADER = LV_COLUMN_DESCRIPTION.

            ELSE.

              CONCATENATE
                LV_HEADER
                LV_COLUMN_DESCRIPTION
              INTO
                LV_HEADER
              SEPARATED BY
                CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

            ENDIF.

          ENDLOOP.

          APPEND LV_HEADER TO LT_HEADER.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
*             BIN_FILESIZE                    =
              filename                        = lv_archivo
*             FILETYPE                        = 'ASC'
*             APPEND                          = ' '
              WRITE_FIELD_SEPARATOR           = SPACE
*             HEADER                          = '00'
*             TRUNC_TRAILING_BLANKS           = ' '
*             WRITE_LF                        = 'X'
*             COL_SELECT                      = ' '
*             COL_SELECT_MASK                 = ' '
*             DAT_MODE                        = ' '
*             CONFIRM_OVERWRITE               = ' '
*             NO_AUTH_CHECK                   = ' '
*             CODEPAGE                        = ' '
*             IGNORE_CERR                     = ABAP_TRUE
*             REPLACEMENT                     = '#'
*             WRITE_BOM                       = ' '
*             TRUNC_TRAILING_BLANKS_EOL       = 'X'
*             WK1_N_FORMAT                    = ' '
*             WK1_N_SIZE                      = ' '
*             WK1_T_FORMAT                    = ' '
*             WK1_T_SIZE                      = ' '
*           IMPORTING
*             FILELENGTH                      =
            TABLES
              data_tab                        = LT_HEADER
*              FIELDNAMES                      =
            EXCEPTIONS
              FILE_write_error                = 1
              no_batch                        = 2
              gui_refuse_filetransfer         = 3
              invalid_type                    = 4
              no_authority                    = 5
              unknown_error                   = 6
              header_not_allowed              = 7
              separator_not_allowed           = 8
              filesize_not_allowed            = 9
              header_too_long                 = 10
              dp_error_create                 = 11
              dp_error_send                   = 12
              dp_error_write                  = 13
              unknown_dp_error                = 14
              access_denied                   = 15
              dp_out_of_memory                = 16
              disk_full                       = 17
              dp_timeout                      = 18
              FILE_not_found                  = 19
              dataprovider_exception          = 20
              control_flush_error             = 21
              OTHERS                          = 22
                    .
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.



          ENDIF.

          LV_APPEND = ABAP_TRUE.

        ELSE.

          LV_APPEND = ABAP_FALSE.

        ENDIF.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
*           BIN_FILESIZE                    =
            filename                        = lv_archivo
*           FILETYPE                        = 'ASC'
            APPEND                          = LV_APPEND
            WRITE_FIELD_SEPARATOR           = 'X'
*           HEADER                          = '00'
*           TRUNC_TRAILING_BLANKS           = ' '
*           WRITE_LF                        = 'X'
*           COL_SELECT                      = ' '
*           COL_SELECT_MASK                 = ' '
*           DAT_MODE                        = ' '
*           CONFIRM_OVERWRITE               = ' '
*           NO_AUTH_CHECK                   = ' '
*           CODEPAGE                        = ' '
*           IGNORE_CERR                     = ABAP_TRUE
*           REPLACEMENT                     = '#'
*           WRITE_BOM                       = ' '
*           TRUNC_TRAILING_BLANKS_EOL       = 'X'
*           WK1_N_FORMAT                    = ' '
*           WK1_N_SIZE                      = ' '
*           WK1_T_FORMAT                    = ' '
*           WK1_T_SIZE                      = ' '
*         IMPORTING
*           FILELENGTH                      =
          TABLES
            data_tab                        = IT_data
*            FIELDNAMES                      =
          EXCEPTIONS
            FILE_write_error                = 1
            no_batch                        = 2
            gui_refuse_filetransfer         = 3
            invalid_type                    = 4
            no_authority                    = 5
            unknown_error                   = 6
            header_not_allowed              = 7
            separator_not_allowed           = 8
            filesize_not_allowed            = 9
            header_too_long                 = 10
            dp_error_create                 = 11
            dp_error_send                   = 12
            dp_error_write                  = 13
            unknown_dp_error                = 14
            access_denied                   = 15
            dp_out_of_memory                = 16
            disk_full                       = 17
            dp_timeout                      = 18
            FILE_not_found                  = 19
            dataprovider_exception          = 20
            control_flush_error             = 21
            OTHERS                          = 22
                  .
        IF sy-subrc <> 0.

            RAISE EXCEPTION TYPE ZCX_FILE_ERROR.

        ENDIF.

      ELSE.

        OPEN DATASET IV_file FOR OUTPUT IN LEGACY TEXT MODE CODE PAGE '1160'.

        IF SY-SUBRC NE 0.
          MESSAGE e125(ZFI00) WITH 'No se pudo abrir el archivo ' IV_file.
        ENDIF.

        lv_linea = get_xls_line_from_table( IT_header ).

        TRANSFER lv_linea TO IV_file.

        LOOP AT IT_data ASSIGNING <lf_linea>.

          lv_linea = get_xls_line( <lf_linea> ).

          TRANSFER lv_linea TO IV_file.

        ENDLOOP.

        CLOSE DATASET IV_file.

      ENDIF.

    endmethod.


    method VALIDAR_ARCHIVO.

      DATA:
        LV_ARCHIVO TYPE STRING.

      IF SY-BATCH IS INITIAL.

        LV_ARCHIVO = IV_ARCHIVO.

        IF IV_ARCHIVO IS INITIAL.
          MESSAGE E078(ZFI00).
        ENDIF.

        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
          EXPORTING
            FILE                 = LV_ARCHIVO
          RECEIVING
            RESULT               = EV_EXISTE
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            WRONG_PARAMETER      = 3
            NOT_SUPPORTED_BY_GUI = 4
            others               = 5
                .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          IF EV_EXISTE EQ ABAP_FALSE.
            MESSAGE ID 'ZFI00' TYPE 'E' NUMBER 078.
          ENDIF.
        ENDIF.

      ELSE.

        OPEN DATASET IV_ARCHIVO FOR INPUT IN TEXT MODE ENCODING DEFAULT.


        IF SY-SUBRC NE 0.
          EV_EXISTE = ABAP_FALSE.
          MESSAGE ID 'ZFI00' TYPE IV_TIPO_MENSAJE_BATCH NUMBER 078.
        ELSE.
          EV_EXISTE = ABAP_TRUE.
          CLOSE DATASET IV_ARCHIVO.
        ENDIF.

      ENDIF.

    endmethod.


    method VALIDATE_FILE_PATH.

      DATA:
        LV_DIRECTORIO TYPE PATHEXTERN.

      CHECK SY-BATCH IS INITIAL AND IV_LOCAL_FILE EQ ABAP_TRUE.

*     Se obtiene la ruta del archivo.
      CALL METHOD ZCL_FILE_UTILS=>GET_FILE_PATH
        EXPORTING
          IV_FILE    = IV_FILE
        CHANGING
          CV_DIRECTORY = LV_DIRECTORIO.

*     Se valida la ruta.
      ZCL_FILE_UTILS=>VALIDATE_PATH(
        LV_DIRECTORIO
        ).

    endmethod.


    method VALIDATE_PATH.

      DATA:
        LV_DIRECTORIO TYPE STRING,
        LV_RESULT TYPE ABAP_BOOL.

      CHECK SY-BATCH IS INITIAL AND IV_LOCAL_FILE EQ ABAP_TRUE.

      IF IV_DIRECTORY IS INITIAL.
        MESSAGE 'La ruta es inexistente' TYPE 'E'.
      ENDIF.

      LV_DIRECTORIO = IV_DIRECTORY.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
        EXPORTING
          DIRECTORY            = LV_DIRECTORIO
        RECEIVING
          RESULT               = LV_RESULT
        EXCEPTIONS
          CNTL_ERROR           = 1
          ERROR_NO_GUI         = 2
          WRONG_PARAMETER      = 3
          NOT_SUPPORTED_BY_GUI = 4
          others               = 5
              .
      IF SY-SUBRC <> 0.

*     Lo que sigue se hace por un error que tiene el mètodo llamado.
        IF NOT ( SY-MSGID IS INITIAL OR SY-MSGTY IS INITIAL OR
           SY-MSGNO IS INITIAL ).

          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

        ENDIF.

      ENDIF.

      CHECK LV_RESULT EQ ABAP_FALSE.

      MESSAGE 'La ruta es inexistente' TYPE 'E'.

    endmethod.
ENDCLASS.
