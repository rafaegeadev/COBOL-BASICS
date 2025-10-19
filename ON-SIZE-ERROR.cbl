      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 NUMERO1                  PIC 9(3)      VALUE 200.
       77 NUMERO2                  PIC 9(3)      VALUE 100.
       77 NUMERO3                  PIC 9(1)      VALUE 3.
       77 NUMERO4                  PIC 9(1)      VALUE 4.
       77 RESULTADO                PIC 9(3)      VALUE ZEROES.
       77 RESULTADO-GRANDE         PIC 9(5)      VALUE ZEROES.

       PROCEDURE DIVISION.

       COMPUTE RESULTADO = NUMERO1 * NUMERO2
            ON SIZE ERROR DISPLAY "NUMERO DEMASIADO GRANDE"
               DISPLAY RESULTADO
       END-COMPUTE.

       COMPUTE RESULTADO = NUMERO1 * NUMERO2
           ON SIZE ERROR COMPUTE RESULTADO-GRANDE = NUMERO1 * NUMERO2
               DISPLAY RESULTADO-GRANDE
       END-COMPUTE.


       COMPUTE RESULTADO = NUMERO1 * NUMERO2
           ON SIZE ERROR
               DISPLAY "EL NUMERO ES MUY GRANDE, NO SE "-
                       "VISUALIZA ENTERO"
               DISPLAY "SE HA ESTABLECIDO EL VALOR POR DEFECTO (200)"
               MOVE 200 TO RESULTADO
               NOT ON SIZE ERROR
               DISPLAY RESULTADO
       END-COMPUTE.

       DISPLAY RESULTADO.

       COMPUTE RESULTADO = NUMERO3 * NUMERO4
           ON SIZE ERROR
               DISPLAY "EL NUMERO ES MUY GRANDE, NO SE "-
                       "VISUALIZA ENTERO"
               DISPLAY "SE HA ESTABLECIDO EL VALOR POR DEFECTO (200)"
               MOVE 200 TO RESULTADO
               NOT ON SIZE ERROR
               DISPLAY RESULTADO
       END-COMPUTE.

       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
