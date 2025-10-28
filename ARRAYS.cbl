      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      *  INSERTAR DATOS NUMÉRICOS EN UN ARRAY
      *  Y CALCULAR EL PROMEDIO DEL CONTENIDO DE LAS CELDAS:
      *     1) SOLICITAR AL USUARIO QUE INGRESE UN NÚMERO
      *        DE CELDAS PARA DIMENSIONAR UN ARRAY.
      *     2) SOLICITAR AL USUARIO QUE INGRESE LOS NÚMEROS
      *        NECESARIOS PARA LLENAR CADA CELDA.
      *     3) CALCULAR EL PROMEDIO DE LOS NÚMEROS INGRESADOS
      *        EN TODO EL ARRAY.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROMEDIO-ARRAY.
      ******************************************************************
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      ******************************************************************
      *                       VARIABLES                                *
      ******************************************************************
      *
       77 WK-CELDAS                PIC S9(10)             VALUE ZERO.
       77 WK-INDICE-VALORES        PIC 9             VALUE ZERO.
       77 WK-RESULTADO-SUMA        PIC 9(4)          VALUE ZEROES.
       77 WK-PROMEDIO              PIC 9(4)V9(2)     VALUE ZEROES.
       01 WK-NUMEROS               OCCURS 7 TIMES    INDEXED BY IDX.
           05 WK-NUMERO            PIC 9(2).
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
           PERFORM
           VARYING                 WK-INDICE-VALORES FROM 1 BY 1
           UNTIL                   WK-INDICE-VALORES > 7
               MOVE ZERO TO        WK-NUMERO(WK-INDICE-VALORES)
           END-PERFORM
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
           PERFORM 2100-SOLICITAR-CELDAS
           PERFORM 2200-INGRESAR-VALORES
           PERFORM 2300-MOSTRAR-ARRAY
           PERFORM 2400-CALCULAR-PROMEDIO
           .
      *
      ******************************************************************
      *                       2100-SOLICITAR-CELDAS                    *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       2100-SOLICITAR-CELDAS.
      *
           PERFORM 1000-INICIO
           DISPLAY "VAMOS A CREAR UNA ARRAY"
           DISPLAY "INTRODUCE UN NÚMERO DE CELDAS (1 A 7):"
           ACCEPT WK-CELDAS
            IF WK-CELDAS <= 0 OR WK-CELDAS > 7
                DISPLAY "CELDAS FUERA DE RANGO"
                DISPLAY "REINICIANDO SISTEMA"
                PERFORM 2100-SOLICITAR-CELDAS
           .
      *
      ******************************************************************
      *                       2200-INGRESAR-VALORES                    *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       2200-INGRESAR-VALORES.
           IF WK-CELDAS = 1
                THEN
                MOVE 1 TO WK-INDICE-VALORES
                DISPLAY "INTRODUCE EL VALOR DE LA CELDA 1:"
                ACCEPT WK-NUMERO(1)
           ELSE
           PERFORM
               VARYING                 WK-INDICE-VALORES FROM 1 BY 1
               UNTIL                   WK-INDICE-VALORES > WK-CELDAS
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA " WK-INDICE-VALORES ":"
               ACCEPT WK-NUMERO(WK-INDICE-VALORES)
           END-PERFORM
           END-IF
           .
      ******************************************************************
      *                       2300-MOSTRAR-ARRAY                       *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       2300-MOSTRAR-ARRAY.
      *
           DISPLAY "TU ARRAY ES:"
           IF WK-CELDAS = 1
               DISPLAY WK-NUMERO(1)
           ELSE
           PERFORM
               VARYING                 WK-INDICE-VALORES FROM 1 BY 1
               UNTIL                   WK-INDICE-VALORES > WK-CELDAS
               DISPLAY
               WK-NUMERO(WK-INDICE-VALORES) WITH NO ADVANCING
               IF WK-INDICE-VALORES < WK-CELDAS
                   DISPLAY
                   ", " WITH NO ADVANCING
               ELSE
                   DISPLAY " "
               END-IF
           END-PERFORM
           END-IF
           .
      ******************************************************************
      *                       2400-CALCULAR-PROMEDIO                   *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
       2400-CALCULAR-PROMEDIO.
      *
           MOVE ZERO TO                  WK-RESULTADO-SUMA

           PERFORM
               VARYING                   WK-INDICE-VALORES FROM 1 BY 1
               UNTIL                     WK-INDICE-VALORES > WK-CELDAS
           ADD
           WK-NUMERO(WK-INDICE-VALORES)  TO WK-RESULTADO-SUMA
           COMPUTE
           WK-PROMEDIO = WK-RESULTADO-SUMA / WK-CELDAS
           END-PERFORM

           DISPLAY
           "LA SUMA TOTAL ES: " WK-RESULTADO-SUMA
           DISPLAY
           "EL PROMEDIO ES: " WK-PROMEDIO
           .
      ******************************************************************
      *                       3000-FIN                                 *
      *----------------------------------------------------------------*
      *                                                                *
      *                                                                *
      ******************************************************************
      *
       3000-FIN.
      *
       STOP RUN
       .
      *
