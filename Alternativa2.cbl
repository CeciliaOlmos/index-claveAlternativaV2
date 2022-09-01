      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS ASSIGN TO "..\socios.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is soc-nro
           ALTERNATE record key is soc-nom WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-nro pic 999.
           03 soc-nom pic x(10).
           03 soc-apell pic x(10).
           03 soc-importe pic 9(8)v99.
           03 soc-tel pic 9(10).
           03 soc-provincia pic x(12).
           03 soc-localidad pic x(12).
           03 soc-calle pic x(12).
           03 soc-nro-calle pic 9(4).
       WORKING-STORAGE SECTION.
       01  lin-soc.
           03 filler pic x(14) value "NUMERO SOCIO:".
           03 l-soc-num pic zzz value spaces.
           03 filler pic x(13) value all ".".
           03 filler pic x(11) value "NOMBRE:".
           03 l-nombre pic x(10) value spaces.
           03 filler pic x(23) value spaces.
       01  lin-resto.
           03 lin-apellido.
               05 filler pic x(30) value all ".".
               05 filler pic x(9) value "APELLIDO:".
               05 filler pic x(2) value spaces.
               05 l-apell pic x(10) value spaces.
               05 filler pic x(21) value spaces.
           03  lin-saldo.
               05 filler pic x(30) value all ".".
               05 filler pic x(9) value "SALDO:".
               05 filler pic x(2) value spaces.
               05 l-saldo pic z.zzz.zzz.zz9,99.
               05 filler pic x(21) value spaces.
           03  lin-tel.
               05 filler pic x(30) value all ".".
               05 filler pic x(9) value "TELEFONO:".
               05 filler pic x(2) value spaces.
               05 l-telef pic zzzzzzzzzz value spaces.
               05 filler pic x(21) value spaces.
           03  lin-provincia.
               05 filler pic x(30) value all ".".
               05 filler pic x(10) value "PROVINCIA:".
               05 filler pic x(1) value spaces.
               05 l-prov pic x(12) value spaces.
           03  lin-dom.
               05 filler pic x(30) value all ".".
               05 filler pic x(10) value "DOMICILIO:".
               05 l-loc pic x(12) value spaces.
               05 filler pic x(6) value "CALLE:".
               05 l-calle pic x(12) value spaces.
               05 filler pic x(4) value "NRO:".
               05 l-nro-calle pic zzz9 value spaces.

       77  sen pic 9.
           88  fin-de-archivo value 1.
       77  nom-ant pic x(10).
       77  soc-ant pic 999.
       77  w-opcion pic 9.
           88 salir value 3.
       77  w-cont pic 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-BUSQUEDA.
           PERFORM with test after UNTIL salir
               PERFORM 200-MENU
               PERFORM 300-PROCESO-MENU
           END-PERFORM.
           PERFORM 400-FIN-BUSQUEDA.
           STOP RUN.

       100-INICIO-BUSQUEDA.
           OPEN INPUT socios.

       200-MENU.
           PERFORM 230-MOSTRAR-OPCIONES.
           ACCEPT w-opcion.

       230-MOSTRAR-OPCIONES.
           display "1 - Buscar por codigo de socio".
           display "2 - Buscar por nombre de socio".
           display "3 - Salir".

       300-PROCESO-MENU.
           EVALUATE w-opcion
               WHEN 1 PERFORM 330-LEER-POR-NUM thru 330-F-LEER-POR-NUM
               WHEN 2 PERFORM 350-LEER-POR-NOM
               WHEN 3 NEXT SENTENCE
           END-EVALUATE.

       330-LEER-POR-NUM.
           PERFORM 360-PIDO-NUM.
           PERFORM 363-BUSCO-SOCIO.
           PERFORM 335-MOSTRAR-DATOS.
       363-BUSCO-SOCIO.
           READ SOCIOS INVALID KEY
                           display "Numero no encontrado"

                           GO TO 330-F-LEER-POR-NUM.
       330-F-LEER-POR-NUM.
           EXIT.

       335-MOSTRAR-DATOS.
           PERFORM 340-ARMAR-LINEA.
           DISPLAY lin-soc.
           PERFORM 345-MOSTRAR-RESTO.

       340-ARMAR-LINEA.
           MOVE soc-nro TO l-soc-num.
           MOVE soc-nom TO l-nombre.
           MOVE soc-apell TO l-apell.
           MOVE soc-importe TO l-saldo.
           MOVE soc-tel TO l-telef.
           MOVE soc-provincia TO l-prov.
           MOVE soc-localidad TO l-loc.
           MOVE soc-calle to l-calle.
           MOVE soc-nro-calle to l-nro-calle.

       345-MOSTRAR-RESTO.
           PERFORM 340-ARMAR-LINEA.
           DISPLAY lin-apellido.
           DISPLAY lin-saldo.
           DISPLAY lin-tel.
           DISPLAY lin-provincia.
           DISPLAY lin-dom.

       350-LEER-POR-NOM.
           move zeros to sen.
           move zero to w-cont.
           PERFORM 355-PIDO-NOMBRE.
           move nom-ant to soc-nom.
           START SOCIOS KEY IS = soc-nom INVALID KEY
                           display "Nombre no encontrado"
                        NOT INVALID KEY
                           PERFORM 359-PROCESAR-SOCIO.

       355-PIDO-NOMBRE.
           display "Ingrese nombre".
           accept nom-ant.

       357-LEER-SOCIOS.
           READ SOCIOS NEXT AT END move 1 to sen.

       359-PROCESAR-SOCIO.
           PERFORM 357-LEER-SOCIOS
           PERFORM UNTIL fin-de-archivo OR soc-nom NOT = nom-ant
                   PERFORM 362-MOSTRAR-NOMBRE
                   PERFORM 357-LEER-SOCIOS
           END-PERFORM.
           PERFORM 370-ELEGIR-CODIGO.

       362-MOSTRAR-NOMBRE.
           DISPLAY "NUMERO SOCIO:",soc-nro,"...........NOMBRE:",soc-nom.
           move soc-nro to soc-ant.
           add 1 to w-cont.

       360-PIDO-NUM.
           display "Ingrese un nro de socio"
           accept soc-nro.

       370-ELEGIR-CODIGO.
           IF w-cont = 1 THEN
                MOVE soc-ant to soc-nro
                PERFORM 363-BUSCO-SOCIO
                PERFORM 345-MOSTRAR-RESTO
           ELSE
              PERFORM 330-LEER-POR-NUM
           END-IF.

       400-FIN-BUSQUEDA.
           CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
