      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMEROS-CALCULOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * 
      ******************************************************************
      *                      CONSTANTES                                *
      ******************************************************************
       01 CTE-CONSTANTE.
          05 CTE-NUMERO1             PIC 9(02)       VALUE 25.
          05 CTE-NUMERO2             PIC 9(02)       VALUE 15.
          05 CTE-NUMERO3             PIC 9(02)       VALUE 10.
      *    
      ******************************************************************
      *                      VARIABLES                                 *
      ******************************************************************
       01 WK-VARIABLES.       
          05 WK-RESULTADOSUMA        PIC 9(02)       VALUE ZEROES.
          05 WK-RESULTADORESTA       PIC S9(02)      VALUE ZEROES.
          05 WK-RESULTADOSUMAVARIOS  PIC 9(02)       VALUE ZEROES.
          05 WK-RESULTADOMULTIPLICACION 
                                     PIC 9(03)       VALUE ZEROES.
          05 WK-RESULTADODIVISION    PIC 9(02)V9(01) VALUE ZEROES.
          05 WK-RESULTADOCOMPUTE     PIC S9(03)      VALUE ZEROES.
      *      
       PROCEDURE DIVISION.
      * Los párrafos en algunos sitios numeran y en otros no.
      * La numeración es una guía para saber dentro de donde se 
      * encuentra.
      *
      * Estos 3 parrafos son casi obligatorios, 
      *siempre hay un INICIO, PROCESO y FIN.
      *
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-FIN
           .
      *
      ******************************************************************
      *                       1000-INICIO                              *
      *----------------------------------------------------------------*
      * Dentro de este parrafo se pone la lógica inicial para que el   * 
      * programa funcione, por ejemplo: abrir ficheros.                *
      ******************************************************************
       1000-INICIO.
      *
           PERFORM 1100-INICIAR-VARIABLES
           .
      ******************************************************************
      *                   1100-INICIAR-VARIABLES                       *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
       1100-INICIAR-VARIABLES.
      *
           MOVE ZEROES                   TO WK-RESULTADOSUMA
           MOVE ZEROES                   TO WK-RESULTADORESTA
           MOVE ZEROES                   TO WK-RESULTADOSUMAVARIOS
           MOVE ZEROES                   TO WK-RESULTADOMULTIPLICACION
           MOVE ZEROES                   TO WK-RESULTADODIVISION
           MOVE ZEROES                   TO WK-RESULTADOCOMPUTE
           .
      *
      ******************************************************************
      *                       2000-PROCESO                             *
      *----------------------------------------------------------------*
      * En este parrafo se pone la lógica del programa, por ejemplo:   * 
      * lectura del fichero, que se quiere hacer con los datos del     *
      * fichero, escribir en otro fichero,...                          *
      ******************************************************************
       2000-PROCESO.
      * 
           PERFORM 2100-CALCULA-SUMA
           PERFORM 2200-CALCULA-RESTA
           PERFORM 2300-CALCULA-SUMA-MULTIPLE
           PERFORM 2400-CALCULA-MULTIPLICACION
           PERFORM 2500-CALCULA-DIVISION
           PERFORM 2600-CALCULA-ECUACION
           PERFORM 2700-MUESTRA-RESULTADO
           .
      *   
      ******************************************************************
      *                      2100-CALCULA-SUMA                         *
      *----------------------------------------------------------------*
      * Este párrafo calcula la suma.                                  *
      ******************************************************************
       2100-CALCULA-SUMA.
      * 
           ADD CTE-NUMERO1                TO CTE-NUMERO2 
                                          GIVING WK-RESULTADOSUMA      
           .           
      *
      ******************************************************************
      *                    2200-CALCULA-RESTA                          *
      *----------------------------------------------------------------*
      * Este párrafo calcula la resta                                  *
      ******************************************************************
       2200-CALCULA-RESTA.
      * 
           SUBTRACT CTE-NUMERO1           FROM CTE-NUMERO2 
                                          GIVING WK-RESULTADORESTA
           .
      *
      ******************************************************************
      *                  2300-CALCULA-SUMA-MULTIPLE                    *
      *----------------------------------------------------------------*
      * Este párrafo calcula la suma múltiple.                         *
      ******************************************************************
       2300-CALCULA-SUMA-MULTIPLE.
      * 
           ADD CTE-NUMERO1,CTE-NUMERO2    TO CTE-NUMERO3 
                                          GIVING WK-RESULTADOSUMAVARIOS
           .
      *
      ******************************************************************
      *                2400-CALCULA-MULTIPLICACION                     *
      *----------------------------------------------------------------*
      * Este párrafo calcula la multiplicación.                        *
      ******************************************************************
       2400-CALCULA-MULTIPLICACION.
      * 
           MULTIPLY CTE-NUMERO1           BY CTE-NUMERO2 
                                       GIVING WK-RESULTADOMULTIPLICACION
           .
      *
      ******************************************************************
      *                  2500-CALCULA-DIVISION                         *
      *----------------------------------------------------------------*
      * Este párrafo calcula la división                               *
      ******************************************************************
       2500-CALCULA-DIVISION.
      * 
           DIVIDE CTE-NUMERO1             BY CTE-NUMERO2 
                                          GIVING WK-RESULTADODIVISION
           .
      *
      ******************************************************************
      *                   2600-CALCULA-ECUACION                        *
      *----------------------------------------------------------------*
      * Este párrafo calcula una ecuación.                             *
      ******************************************************************
       2600-CALCULA-ECUACION.
      * 
           COMPUTE WK-RESULTADOCOMPUTE = (CTE-NUMERO2 - CTE-NUMERO1) 
                                      * CTE-NUMERO3
           .
      *        
      ******************************************************************
      *                   2700-MUESTRA-RESULTADO                       *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
       2700-MUESTRA-RESULTADO.
      *
           DISPLAY "El resultado de sumar "
                   CTE-NUMERO1 " + " CTE-NUMERO2
                   " es " WK-RESULTADOSUMA

           DISPLAY "El resultado de restar "
                   CTE-NUMERO2 " - " CTE-NUMERO1
                   " es " WK-RESULTADORESTA

           DISPLAY "El resultado de sumar "
                   CTE-NUMERO1 " + " CTE-NUMERO2" + " CTE-NUMERO3
                   " es " WK-RESULTADOSUMAVARIOS

           DISPLAY "El resultado de multiplicar "
                   CTE-NUMERO1 " X " CTE-NUMERO2
                   " es " WK-RESULTADOMULTIPLICACION.

           DISPLAY "El resultado de dividir "
                   CTE-NUMERO1 " / " CTE-NUMERO2
                   " es " WK-RESULTADODIVISION

           DISPLAY "El resultado de("
                   CTE-NUMERO2 " - " CTE-NUMERO1 ") x " CTE-NUMERO3
                   " es " WK-RESULTADOCOMPUTE
           .
      *        
      ******************************************************************
      *                         3000-FIN                               *
      *----------------------------------------------------------------*
      * Esta parte corresponde la lógica para finalizar el programa,   *
      * por ejemplo: cierre de ficheros, estadisticas del programa ... *      
      ******************************************************************
       3000-FIN.
      *
           STOP RUN      
           .
      *
      ******************************************************************

