      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT socios ASSIGN TO "..\socios.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
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
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-INGRESO.
           PERFORM UNTIL soc-nro = 0
               PERFORM 300-INGRESO-RESTO
               PERFORM 400-GRABO-REGISTRO
              PERFORM 200-INGRESO
           END-PERFORM.
           PERFORM 500-FIN.
            STOP RUN.
        100-INICIO.
           OPEN I-O socios.
       200-INGRESO.
           display "ingrese nro"
           accept soc-nro.
       300-INGRESO-RESTO.
           DISPLAY "INGRESE LOS DATOS DEL SOCIO: "
           display "nombre"
           accept soc-nom
           DISPLAY "apellido"
           ACCEPT soc-apell
           DISPLAY "saldo"
           ACCEPT soc-importe
           DISPLAY "telefono"
           ACCEPT soc-tel
           DISPLAY "provincia"
           ACCEPT soc-provincia
           DISPLAY "localidad"
           ACCEPT soc-localidad
           DISPLAY "calle del domicilio"
           ACCEPT soc-calle
           DISPLAY "numero de calle"
           ACCEPT soc-nro-calle.
       400-GRABO-REGISTRO.
           write soc-reg invalid key display "no pude"
           end-write.
       500-FIN.
           close socios.

       END PROGRAM YOUR-PROGRAM-NAME.
