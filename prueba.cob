      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CURSO-COBOL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 NUMERO1 PIC 99 VALUE 25.
           01 NUMERO2 PIC 99 VALUE 15.
           01 NUMERO3 PIC 99 VALUE 10.
           01 RESULTADOSUMA PIC 99 VALUE ZERO.
           01 RESULTADORESTA PIC S99 VALUE ZERO.
           01 RESULTADOSUMAVARIOS PIC 99 VALUE ZERO.
           01 RESULTADOMULTIPLICACION PIC 999 VALUE ZERO.
           01 RESULTADODIVISION PIC 99V9 VALUE ZERO.
           01 RESULTADOCOMPUTE PIC S999 VALUE ZERO.

       PROCEDURE DIVISION.
      *> Este párrafo calcula la suma.
       CALCULA-SUMA.
           ADD NUMERO1 TO NUMERO2 GIVING RESULTADOSUMA.

      *> Este párrafo calcula la resta.
       CALCULA-RESTA.
           SUBTRACT NUMERO1 FROM NUMERO2 GIVING RESULTADORESTA.

      *> Este párrafo calcula la suma múltiple.
       CALCULA-SUMA-MULTIPLE.
           ADD NUMERO1,NUMERO2 TO NUMERO3 GIVING RESULTADOSUMAVARIOS.

      *> Este párrafo calcula la multiplicacion.
       CALCULA-MULTIPLICACION.
           MULTIPLY NUMERO1 BY NUMERO2 GIVING RESULTADOMULTIPLICACION.

      *> Este párrafo calcula la división.
       CALCULA-DIVISION.
           DIVIDE NUMERO1 BY NUMERO2 GIVING RESULTADODIVISION.

      *> Este párrafo calcula una ecuación.
       CALCULA-ECUACION.
           COMPUTE RESULTADOCOMPUTE = (NUMERO2 - NUMERO1) * NUMERO3.

       MUESTRA-RESULTADO.
           DISPLAY "El resultado de sumar "
                   NUMERO1 " + " NUMERO2
                   " es " RESULTADOSUMA.

           DISPLAY "El resultado de restar "
                   NUMERO2 " - " NUMERO1
                   " es " RESULTADORESTA.

           DISPLAY "El resultado de sumar "
                   NUMERO1 " + " NUMERO2" + " NUMERO3
                   " es " RESULTADOSUMAVARIOS.

           DISPLAY "El resultado de multiplicar "
                   NUMERO1 " X " NUMERO2
                   " es " RESULTADOMULTIPLICACION.

           DISPLAY "El resultado de dividir "
                   NUMERO1 " / " NUMERO2
                   " es " RESULTADODIVISION.

           DISPLAY "El resultado de("
                   NUMERO2 " - " NUMERO1 ") x " NUMERO3
                   " es " RESULTADOCOMPUTE.

           STOP RUN.
       END PROGRAM CURSO-COBOL.
