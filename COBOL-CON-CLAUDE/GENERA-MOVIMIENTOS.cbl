       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERA-MOVIMIENTOS.
      *AUTHOR. RAFAEL.
      *****************************************************************
      * PROGRAMA: GENERADOR DE ARCHIVO MOVIMIENTOS
      * PROPOSITO: CREA UN ARCHIVO DE MOVIMIENTOS CON DATOS DE PRUEBA
      *            ORDENADO POR CODIGO
      *            INCLUYE CODIGOS QUE EXISTEN Y NO EXISTEN EN MAESTRO
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-MOVIMIENTOS
               ASSIGN TO "MOVIMIENTOS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-MOVIMIENTOS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-MOVIMIENTOS.
       01  REG-MOVIMIENTOS.
           05 CODIGO-MOVIMIENTO    PIC 9(5).
           05 TIPO-MOVIMIENTO      PIC X(01).
           05 IMPORTE-MOVIMIENTO   PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  FS-MOVIMIENTOS          PIC XX.

       01  CONTADOR                PIC 9(3) VALUE ZEROS.

       01  TABLA-MOVIMIENTOS.
      * CODIGO (5) + TIPO (1) + IMPORTE (9) = 15 caracteres
           05 FILLER PIC X(15) VALUE "00001C000100000".
           05 FILLER PIC X(15) VALUE "00002A000050000".
           05 FILLER PIC X(15) VALUE "00003C000250000".
           05 FILLER PIC X(15) VALUE "00005A000200000".
           05 FILLER PIC X(15) VALUE "00007C000150000".
           05 FILLER PIC X(15) VALUE "00008A000300000".
           05 FILLER PIC X(15) VALUE "00010C000450000".
           05 FILLER PIC X(15) VALUE "00012A000100000".
           05 FILLER PIC X(15) VALUE "00015C000080000".
           05 FILLER PIC X(15) VALUE "00018A000120000".
           05 FILLER PIC X(15) VALUE "00020C000350000".
           05 FILLER PIC X(15) VALUE "00022A000500000".
           05 FILLER PIC X(15) VALUE "00025C000280000".

       01  TABLA-MOVIMIENTOS-R REDEFINES TABLA-MOVIMIENTOS.
           05 MOVIMIENTO OCCURS 13 TIMES.
              10 COD-MOV           PIC 9(5).
              10 TIPO-MOV          PIC X(01).
              10 IMP-MOV           PIC 9(9).

       01  INDICE                  PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIALIZAR
           PERFORM 200-GENERAR-REGISTROS
           PERFORM 300-FINALIZAR
           STOP RUN.

       100-INICIALIZAR.
           DISPLAY "========================================".
           DISPLAY "  GENERADOR DE ARCHIVO MOVIMIENTOS".
           DISPLAY "========================================".
           DISPLAY " ".

           OPEN OUTPUT ARCHIVO-MOVIMIENTOS

           IF FS-MOVIMIENTOS NOT = '00'
               DISPLAY "ERROR ABRIENDO ARCHIVO: " FS-MOVIMIENTOS
               STOP RUN
           END-IF

           DISPLAY "Archivo MOVIMIENTOS.DAT abierto correctamente".

       200-GENERAR-REGISTROS.
           PERFORM VARYING INDICE FROM 1 BY 1
                   UNTIL INDICE > 13

               MOVE COD-MOV(INDICE)  TO CODIGO-MOVIMIENTO
               MOVE TIPO-MOV(INDICE) TO TIPO-MOVIMIENTO
      * Dividir por 100 para ajustar los decimales
               DIVIDE IMP-MOV(INDICE) BY 100
                   GIVING IMPORTE-MOVIMIENTO

               WRITE REG-MOVIMIENTOS

               IF FS-MOVIMIENTOS = '00'
                   ADD 1 TO CONTADOR
                   DISPLAY "Movimiento creado: " CODIGO-MOVIMIENTO
                           " Tipo: " TIPO-MOVIMIENTO
                           " Importe: " IMPORTE-MOVIMIENTO
               ELSE
                   DISPLAY "ERROR ESCRIBIENDO REGISTRO: "
                           FS-MOVIMIENTOS
               END-IF
           END-PERFORM.

       300-FINALIZAR.
           CLOSE ARCHIVO-MOVIMIENTOS

           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "  PROCESO FINALIZADO".
           DISPLAY "========================================".
           DISPLAY "Total movimientos creados: " CONTADOR.
           DISPLAY "Archivo generado: MOVIMIENTOS.DAT".
           DISPLAY " ".
           DISPLAY "NOTA: Algunos codigos existen en MAESTRO".
           DISPLAY "      y otros NO (para probar el cruzado)".
           DISPLAY " ".
