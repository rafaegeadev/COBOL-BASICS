      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLAS-MULTIPLICAR.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *
      ******************************************************************
      *                       VARIABLES                                *
      ******************************************************************
      *
           77 WK-NUMERO                  PIC 9(3)       VALUE ZEROES.
           77 WK-MULTIPLICADOR           PIC 9(3)       VALUE ZEROES.
           77 WK-RESULTADO               PIC 9(4)       VALUE ZEROES.

       01 WS-MENU                        PIC S9(1)      VALUE ZERO.
           88 WK-EMPEZAR                                VALUE 1.
           88 WK-SALIR                                  VALUE 2.
           88 WK-CONTINUAR                              VALUE 3.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-FIN
           .
      *
      ******************************************************************
      *                       1000-INICIO                              *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       1000-INICIO.
      *
               DISPLAY
               "--------------------------------------------"
               DISPLAY
               "BIENVENIDO A LAS TABLAS DE MULTIPLICAR DE COBOL"
               DISPLAY
               "--------------------------------------------"
               .
      *
      ******************************************************************
      *                       2000-PROCESO                             *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       2000-PROCESO.
      *
           PERFORM 2100-MENU
           PERFORM 2200-OPCION
           .
      *
      ******************************************************************
      *                   2100-MENU                                    *
      *----------------------------------------------------------------*
      *        MENÚ DE SELECCIÓN DE OPERACIÓN POR EL USUARIO           *
      ******************************************************************
           2100-MENU.
      *
               MOVE ZERO                   TO WK-MULTIPLICADOR
               DISPLAY
               "Elige una opción: "
               DISPLAY
               "1 - EMPEZAR"
               DISPLAY
               "2 - SALIR"
               DISPLAY
               "---------------------------------------"
               ACCEPT
               WS-MENU
               .
      *
      ******************************************************************
      *                   2200-SOLICITA-NUM                            *
      *----------------------------------------------------------------*
      *              EL USUARIO INTRODUCE LOS NÚMEROS                  *
      ******************************************************************
           2200-OPCION.
      *
               EVALUATE TRUE

                       WHEN WK-EMPEZAR
                           DISPLAY
                           "****************************************"
                           PERFORM 2300-LA-TABLA

                       WHEN WK-SALIR
                           PERFORM 3000-FIN

                       WHEN OTHER
                           DISPLAY
                           "****************************************"
                           DISPLAY
                           "OPCIÓN INVÁLIDA, INTÉNTELO DE NUEVO."
                           PERFORM 2100-MENU

              END-EVALUATE
              .
      *
      ******************************************************************
      *                   2300-LA-TABLA                                *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2300-LA-TABLA.
      *
               DISPLAY
               "-----------------------------"
               DISPLAY
               "COMENCEMOS..."
               DISPLAY
               "-----------------------------"
               PERFORM 2301-SOLICITA-NUM
               .
      *
      ******************************************************************
      *                   2301-SOLICITA-NUM                            *
      ******************************************************************
               2301-SOLICITA-NUM.
      *
                   DISPLAY
                   "INTRODUCE UN NUMERO: "
                   DISPLAY
                   "-----------------------------"
                   ACCEPT WK-NUMERO
                   DISPLAY
                   "-----------------------------"
                       IF WK-NUMERO IS ZERO
                       DISPLAY
                       "Por favor, introduce un número"-
                       " o un valor superior a cero."
                       DISPLAY "-----------------------------"
                       PERFORM 2301-SOLICITA-NUM
                       ELSE
                           PERFORM 2302-MOSTRAR-TABLA
                       END-IF
                   .
      *
      ******************************************************************
      *                   2302-MOSTRAR-TABLA                           *
      ******************************************************************
               2302-MOSTRAR-TABLA.
      *
                   DISPLAY
                   "LA TABLA DEL " WK-NUMERO ":"
                   PERFORM 2304-CALULAR-TABLA 10 TIMES
                   DISPLAY
                   "-----------------------------"
                   PERFORM 2400-CONTINUAR-O-SALIR
                   .
      *
      ******************************************************************
      *                   2304-CALULAR-TABLA                           *
      ******************************************************************
               2304-CALULAR-TABLA.
      *
                   ADD 1                       TO WK-MULTIPLICADOR
                   COMPUTE
                   WK-RESULTADO = WK-NUMERO * WK-MULTIPLICADOR
                   DISPLAY
                   WK-NUMERO " * " WK-MULTIPLICADOR
                   " = " WK-RESULTADO
                   .
      *
      ******************************************************************
      *                   2400-CONTINUAR-O-SALIR                       *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2400-CONTINUAR-O-SALIR.
      *
               DISPLAY
                   "¿Quieres seguir haciendo tablas?"
                   DISPLAY
                   "2 - Salir"
                   DISPLAY
                   "3 - Continuar ->"
                   DISPLAY
                   "-------------------------------------"
                   ACCEPT
                   WS-MENU
                   DISPLAY
                   "-------------------------------------"
                   IF WK-CONTINUAR
                   THEN
                       PERFORM 2301-SOLICITA-NUM
                   IF WK-SALIR
                       PERFORM 3000-FIN
                   ELSE
                       DISPLAY
                       "INTRODUCE UNA OPCIÓN VÁLIDA"
                       PERFORM 2400-CONTINUAR-O-SALIR
                   END-IF
                   .
      *
      ******************************************************************
      *                   2500-CONTINUAR-O-SALIR                       *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2500-CONTINUAR-O-SALIR.
      *

      ******************************************************************
      *                   3000-FIN                                     *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
       3000-FIN.
      *
           DISPLAY
           "****************************************"
           DISPLAY
           "SALIENDO DEL PROGRAMA..."
           DISPLAY
           "****************************************"
           STOP RUN
           .
      *
      ******************************************************************
