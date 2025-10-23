      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA-PERFORM.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      *
      ******************************************************************
      *                       VARIABLES                                *
      ******************************************************************
      *
       01  WS-OPCIONES                        PIC S9      VALUE ZERO.
           88 WK-SUMAR                                    VALUE 1.
           88 WK-RESTAR                                   VALUE 2.
           88 WK-MULTIPLICAR                              VALUE 3.
           88 WK-DIVIDIR                                  VALUE 4.
           88 WK-SALIR                                    VALUE 5.
           88 WK-CONTINUAR                                VALUE 6.

       77  WK-NUMERO1                      PIC S9(3)       VALUE ZEROES.
       77  WK-NUMERO2                      PIC S9(3)       VALUE ZEROES.

       77  WK-RESULTADO                    PIC S9(6)       VALUE ZEROES.
       77  WK-RESTO-DIV                    PIC S9(6)       VALUE ZEROES.
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
               "BIENVENIDO A LA CALCULADORA DE COBOL"
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
           PERFORM 2200-SOLICITA-NUM
           PERFORM 2100-MENU
           PERFORM 2300-OPCION
           .
      *
      ******************************************************************
      *                   2100-MENU                                    *
      *----------------------------------------------------------------*
      *        MENÚ DE SELECCIÓN DE OPERACIÓN POR EL USUARIO           *
      ******************************************************************
           2100-MENU.
      *
               DISPLAY
               "Elige una operación: "
               DISPLAY
               "1 - Sumar (+)"
               DISPLAY
               "2 - Restar (-)"
               DISPLAY
               "3 - Multiplicar (*)"
               DISPLAY
               "4 - Dividir (/)"
               DISPLAY
               "5 - Salir"
               DISPLAY
               "---------------------------------------"
               ACCEPT
               WS-OPCIONES
               .
      *
      ******************************************************************
      *                   2200-SOLICITA-NUM                            *
      *----------------------------------------------------------------*
      *              EL USUARIO INTRODUCE LOS NÚMEROS                  *
      ******************************************************************
           2200-SOLICITA-NUM.
      *        
               DISPLAY
               "Por favor, introduzca el primer número."
               ACCEPT
               WK-NUMERO1
               DISPLAY
               "Por favor, introduzca el segundo número."
               ACCEPT
               WK-NUMERO2
               DISPLAY
               "---------------------------------------"
               .
      *
      ******************************************************************
      *                   2300-OPCION                                  *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2300-OPCION.
      *
               EVALUATE TRUE

                       WHEN WK-SUMAR
                           DISPLAY
                           "****************************************"
                           PERFORM 2401-SUMA

                       WHEN WK-RESTAR
                           DISPLAY
                           "****************************************"
                           PERFORM 2402-RESTA

                       WHEN WK-MULTIPLICAR
                           DISPLAY
                           "****************************************"
                           PERFORM 2403-MULTIPLICACION

                       WHEN WK-DIVIDIR
                           DISPLAY
                           "****************************************"
                           PERFORM 2404-DIVIDIENDO

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
      *                   2400-OPERACIONES                             *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2400-OPERACIONES.
      *
      ******************************************************************
      *                   2401-SUMA                                    *
      ******************************************************************
      *
               2401-SUMA.
      *
                   DISPLAY
                   "Has elegido sumar"
                   DISPLAY
                   "CALCULANDO..."
                   
                   ADD                     WK-NUMERO1 TO WK-NUMERO2
                                           GIVING WK-RESULTADO.
                   DISPLAY
                   "-------------------------------------"
                   DISPLAY
                   "El resultado de la suma es: "
                   WK-RESULTADO
                   "."
                   DISPLAY
                   "-------------------------------------"
                   PERFORM 2500-CONTINUAR-O-SALIR
                   .
      *
      ******************************************************************
      *                   2401-RESTA                                   *
      ******************************************************************
      *
               2402-RESTA.
      *
                   DISPLAY
                   "Has elegido restar"
                   DISPLAY
                   "CALCULANDO..."
                   
                   SUBTRACT                WK-NUMERO2 FROM WK-NUMERO1
                                           GIVING WK-RESULTADO.
                   DISPLAY
                   "-------------------------------------"
                   DISPLAY
                   "El resultado de la resta es: "
                   WK-RESULTADO
                   "."
                   DISPLAY
                   "-------------------------------------"
                   PERFORM 2500-CONTINUAR-O-SALIR
                   .
      *
      ******************************************************************
      *                   2403-MULTIPLICACION                          *
      ******************************************************************
      *
               2403-MULTIPLICACION.
      *
                   DISPLAY
                   "Has elegido Multiplicación"
                   DISPLAY
                   "CALCULANDO..."
                   
                   MULTIPLY                WK-NUMERO1 BY WK-NUMERO2
                                           GIVING WK-RESULTADO
                   DISPLAY
                   "-------------------------------------"
                   DISPLAY
                   "El resultado de la Multiplicación es: "
                   WK-RESULTADO
                   "."
                   DISPLAY
                   "-------------------------------------"
                   PERFORM 2500-CONTINUAR-O-SALIR
                   .
      *
      ******************************************************************
      *                   2404-DIVIDIENDO                              *
      ******************************************************************
      *
               2404-DIVIDIENDO.
      *
                   DISPLAY
                   "Has elegido División"
                   DISPLAY
                   "CALCULANDO..."
                   
                   DIVIDE                  WK-NUMERO1 BY WK-NUMERO2
                                           GIVING WK-RESULTADO
                                           REMAINDER WK-RESTO-DIV
                   DISPLAY
                   "-------------------------------------"
                   DISPLAY
                   "El resultado de la división es: "
                   WK-RESULTADO " con un resto de " WK-RESTO-DIV
                   "."
                   DISPLAY
                   "-------------------------------------"
                   PERFORM 2500-CONTINUAR-O-SALIR
                   .
      *
      ******************************************************************
      *                   2500-CONTINUAR-O-SALIR                       *
      *----------------------------------------------------------------*
      *                                                                *
      ******************************************************************
           2500-CONTINUAR-O-SALIR.
      *
               DISPLAY
                   "¿Quieres seguir operando?" 
                   DISPLAY
                   "5 - Salir"
                   DISPLAY
                   "6 - Continuar ->"
                   DISPLAY
                   "-------------------------------------"
                   ACCEPT
                   WS-OPCIONES
                   DISPLAY
                   "-------------------------------------"
                   IF WK-CONTINUAR
                   THEN
                       PERFORM 2000-PROCESO
                   IF WK-SALIR
                       PERFORM 3000-FIN
                   ELSE
                       DISPLAY
                       "INTRODUCE UNA OPCIÓN VÁLIDA"
                       PERFORM 2500-CONTINUAR-O-SALIR
                   END IF
                   .
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
