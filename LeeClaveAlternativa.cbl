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
           SELECT socios ASSIGN TO "..\socios.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is SEQUENTIAL
           record KEY is soc-nro
           ALTERNATE record key is soc-nom WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       fd  socios.
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
       01  w-flag pic 9 value ZERO.
       01  lin-soc.
           03 filler pic x(6) value "NUMERO".
       01  lin-soc2.
           03 filler pic x(6) value "SOCIO:".
           03 filler pic x(2) value spaces.
           03 filler pic x(9) value "NOMBRE Y".
           03 filler pic x(9) value "APELLIDO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(9) value " SALDO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(9) value "TELEFONO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(10) value "PROVINCIA:".
           03 filler pic x(4) value spaces.
           03 filler pic x(10) value "DOMICILIO:".
           03 filler pic x(5) value spaces.
           03 filler pic x(8) value "CALLE Y ".
           03 filler pic x(2) value spaces.
           03 filler pic x(4) value "NRO:".
           03 filler pic x(3) value spaces.
       01  lin-guarda.
           03 filler pic x(104) value all "-".
       01  lin-detalle.
           03 l-soc-num pic zzz value spaces.
           03 filler pic x(5) value spaces.
           03 l-nombre pic x(10) value spaces.
           03 l-apell pic x(8) value spaces.
           03 l-saldo pic zzz.zzz.zz9,99.
           03 filler pic x(2) value spaces.
           03 l-telef pic zzzzzzzzzz value spaces.
           03 filler pic x(3) value spaces.
           03 l-prov pic x(12) value spaces.
           03 filler pic x(3) value spaces.
           03 l-loc pic x(12) value spaces.
           03 filler pic x(3) value spaces.
           03 l-calle pic x(12) value spaces.
           03 filler pic x(3) value spaces.
           03 l-nro-calle pic zzz9 value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-SOCIOS.
           PERFORM UNTIL w-flag is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-SOCIOS
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.
           STOP RUN.
       100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           open INPUT SOCIOS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-soc.
           DISPLAY lin-soc2.
           DISPLAY lin-guarda.

       200-LEE-ARCH-SOCIOS.
           READ socios next at end move 1 to w-flag.

       300-PROCESO-LECTURA.
           move soc-nro to l-soc-num.
           move soc-nom to l-nombre.
           move soc-apell to l-apell.
           move soc-importe to l-saldo.
           move soc-tel to l-telef.
           move soc-provincia to l-prov.
           move soc-localidad to l-loc.
           move soc-calle to l-calle.
           move soc-nro-calle to l-nro-calle.
           DISPLAY lin-detalle.



       400-FIN-LECTURA.
           close SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
