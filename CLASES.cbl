      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS A-G IS "A" THRU "G", "a" THRU "g", "7" THRU "9".
           CLASS A-G-UPPER IS "A" THRU "G".
           CLASS A-G-LOWER IS "a" THRU "g".
           CLASS BINARIO IS ZERO THRU "1".
           CLASS HEX IS ZERO THRU "9", "A" THRU "F", "a" THRU "f".
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 Valor1 PIC X(4) VALUE "ABCD".
           01 Valor2 PIC X(4) VALUE "A9cD".
           01 Valor3 PIC X(4) VALUE "0101".
           01 Valor4 PIC X(6) VALUE "aC24FF".
       PROCEDURE DIVISION.
       *> Solicita datos al usuario.
       SolicitaValor1.
       *> Solicita Valor 1.
            DISPLAY "--------------------------------------------"
            DISPLAY
            "Introduce cuatro caracteres en mayúscula entre A y G:".
            DISPLAY "--------------------------------------------"
            ACCEPT Valor1.

       CompruebaValor1.
           IF Valor1 IS A-G-UPPER OR Valor1 IS A-G-LOWER THEN
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor1 " y está en el rango"
               DISPLAY "--------------------------------------------"
           ELSE
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor1 " y no está en el rango"
               DISPLAY "--------------------------------------------"
           END-IF.

       SolicitaValor2.
       *> Solicita Valor2.
            DISPLAY
            "Introduce cuatro caracteres:".
            DISPLAY "--------------------------------------------"
            ACCEPT Valor2.

       CompruebaValor2.

           IF Valor2 IS A-G THEN
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor2 " y está en el rango"
               DISPLAY "--------------------------------------------"
           ELSE
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor2 " y no está en el rango"
               DISPLAY "--------------------------------------------"
               DISPLAY "INTÉNTALO DE NUEVO"
               DISPLAY "--------------------------------------------"
               ACCEPT Valor2
                   IF Valor2 IS A-G THEN
                       DISPLAY
               "--------------------------------------------"
                       DISPLAY
                       "Has escrito " Valor2 " y está en el rango"
                       DISPLAY
               "--------------------------------------------"
                   ELSE
                       DISPLAY
               "--------------------------------------------"
                       DISPLAY
               "Has escrito dos veces y fuera de rango"
                       DISPLAY
               "--------------------------------------------"
                       DISPLAY "ÚLTIMO INTENTO"
                       DISPLAY
               "--------------------------------------------"
                       ACCEPT Valor2
                       DISPLAY
               "--------------------------------------------"
                           IF Valor2 IS A-G THEN
                           DISPLAY
               "--------------------------------------------"
                           DISPLAY
                           "Has escrito " Valor2 " y está en el rango"
                           DISPLAY
               "--------------------------------------------"
                           ELSE
                           DISPLAY
               "--------------------------------------------"
                           DISPLAY
               "Has escrito tres veces y fuera de rango"
                           DISPLAY
               "--------------------------------------------"


           END-IF.

       SolicitaValor3.
       *> Solicita Valor3.
            DISPLAY "Introduce un binario de 4 dígitos:".
            DISPLAY "--------------------------------------------"
            ACCEPT Valor3.

       CompruebaValor3.

           IF Valor3 IS BINARIO THEN
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor3 " y está en el rango"
               DISPLAY "--------------------------------------------"
           ELSE
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor3 " y no está en el rango"
               DISPLAY "--------------------------------------------"
           END-IF.

       SolicitaValor4.
       *> Solicita Valor4.
            DISPLAY "Introduce un hexadecimal de 6 dígitos:".
            DISPLAY "--------------------------------------------"
            ACCEPT Valor4.

       CompruebaValor4.

           IF Valor4 IS HEX THEN
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor4 " y está en el rango"
               DISPLAY "--------------------------------------------"
           ELSE
               DISPLAY "--------------------------------------------"
               DISPLAY "Has escrito " Valor4 " y no está en el rango"
               DISPLAY "--------------------------------------------"
           END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
