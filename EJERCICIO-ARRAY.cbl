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
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WK-I                     PIC 9           VALUE ZERO.
       01 WK-NUMEROS               OCCURS          7 TIMES.
           05 WK-NUMERO            PIC 9(2).
       77 RESULTADO-SUMA           PIC 9(4)       VALUE ZEROES.
       PROCEDURE DIVISION.


       INICIALIZAR-ARRAY.
           MOVE ZERO       TO          WK-NUMERO(1)
           MOVE ZERO       TO          WK-NUMERO(2)
           MOVE ZERO       TO          WK-NUMERO(3)
           MOVE ZERO       TO          WK-NUMERO(4)
           MOVE ZERO       TO          WK-NUMERO(5)
           MOVE ZERO       TO          WK-NUMERO(6)
           MOVE ZERO       TO          WK-NUMERO(7)
           .
       SOLICITAR-CELDAS.
           DISPLAY
           "VAMOS A CREAR UNA ARRAY"
           DISPLAY
           "INTRODUCE UN NÚMERO DE CELDAS NO MAYOR QUE 7"
           ACCEPT WK-I
           .
       SOLICITAR-NUMEROS.
           EVALUATE TRUE
           WHEN WK-I = 1
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
           WHEN WK-I = 2
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
           WHEN WK-I = 3
           DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 3"
               ACCEPT WK-NUMERO(3)
           WHEN WK-I = 4
           DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 3"
               ACCEPT WK-NUMERO(3)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 4"
               ACCEPT WK-NUMERO(4)
           WHEN WK-I = 5
           DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 3"
               ACCEPT WK-NUMERO(3)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 4"
               ACCEPT WK-NUMERO(4)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 5"
               ACCEPT WK-NUMERO(5)
           WHEN WK-I = 6
           DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 3"
               ACCEPT WK-NUMERO(3)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 4"
               ACCEPT WK-NUMERO(4)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 5"
               ACCEPT WK-NUMERO(5)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 6"
               ACCEPT WK-NUMERO(6)
           WHEN WK-I = 7
           DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 1"
               ACCEPT WK-NUMERO(1)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 2"
               ACCEPT WK-NUMERO(2)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 3"
               ACCEPT WK-NUMERO(3)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 4"
               ACCEPT WK-NUMERO(4)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 5"
               ACCEPT WK-NUMERO(5)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 6"
               ACCEPT WK-NUMERO(6)
               DISPLAY
               "INTRODUCE EL VALOR DE LA CELDA 7"
               ACCEPT WK-NUMERO(7)
           WHEN OTHER
               DISPLAY
               "NUMERO FUERA DE RANGO"
           .
       CREAR-TABLA.
           DISPLAY
           "CREANDO ARRAY... "
           EVALUATE TRUE
           WHEN WK-I = 1
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 2
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 3
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 4
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 5
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 6
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN WK-I = 7
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
               MOVE WK-NUMERO(1)           TO WK-NUMERO(1)
           WHEN OTHER
               DISPLAY
               "NUMERO FUERA DE RANGO"
               .


       MOSTRAR-TABLA.
           DISPLAY
           "TU ARRAY ES: "
           EVALUATE TRUE
           WHEN WK-I = 1
               DISPLAY WK-NUMERO(1)
           WHEN WK-I = 2
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2)
           WHEN WK-I = 3
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(3)
           WHEN WK-I = 4
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(3) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(4)
           WHEN WK-I = 5
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(3) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(4) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(5)
           WHEN WK-I = 6
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(3) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(4) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(5) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(6)
           WHEN WK-I = 7
               DISPLAY WK-NUMERO(1) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(2) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(3) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(4) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(5) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(6) ", " WITH NO ADVANCING
               DISPLAY WK-NUMERO(7)
           WHEN OTHER
               DISPLAY
               "NUMERO FUERA DE RANGO"
               .


       END PROGRAM YOUR-PROGRAM-NAME.
