       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERA-MAESTRO.
      *AUTHOR. RAFAEL.
      *****************************************************************
      * PROGRAMA: GENERADOR DE ARCHIVO MAESTRO
      * PROPOSITO: CREA UN ARCHIVO MAESTRO CON DATOS DE PRUEBA
      *            ORDENADO POR CODIGO
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-MAESTRO
               ASSIGN TO "MAESTRO.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-MAESTRO.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-MAESTRO.
       01  REG-MAESTRO.
           05 CODIGO-MAESTRO       PIC 9(5).
           05 NOMBRE-MAESTRO       PIC X(30).
           05 SALDO-MAESTRO        PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  FS-MAESTRO              PIC XX.

       01  CONTADOR                PIC 9(3) VALUE ZEROS.

       01  TABLA-CLIENTES.
      * CODIGO (5) + NOMBRE (30) = 35 caracteres
           05 FILLER PIC X(35) VALUE "00001Juan Perez Martinez        ".
           05 FILLER PIC X(35) VALUE "00003Maria Garcia Lopez         ".
           05 FILLER PIC X(35) VALUE "00005Pedro Lopez Sanchez        ".
           05 FILLER PIC X(35) VALUE "00007Ana Rodriguez Fernandez    ".
           05 FILLER PIC X(35) VALUE "00010Carlos Gonzalez Ruiz       ".
           05 FILLER PIC X(35) VALUE "00012Laura Martinez Gomez       ".
           05 FILLER PIC X(35) VALUE "00015Roberto Sanchez Diaz       ".
           05 FILLER PIC X(35) VALUE "00018Sofia Fernandez Castro     ".
           05 FILLER PIC X(35) VALUE "00020Miguel Torres Moreno       ".
           05 FILLER PIC X(35) VALUE "00025Elena Ramirez Ortiz        ".

       01  TABLA-CLIENTES-R REDEFINES TABLA-CLIENTES.
           05 CLIENTE OCCURS 10 TIMES.
              10 COD-CLI           PIC 9(5).
              10 NOM-CLI           PIC X(30).

       01  TABLA-SALDOS.
      * Saldos en formato de 9 digitos sin decimales
           05 FILLER PIC 9(9) VALUE 005000000.
           05 FILLER PIC 9(9) VALUE 010000000.
           05 FILLER PIC 9(9) VALUE 007500000.
           05 FILLER PIC 9(9) VALUE 012000000.
           05 FILLER PIC 9(9) VALUE 003500000.
           05 FILLER PIC 9(9) VALUE 020000000.
           05 FILLER PIC 9(9) VALUE 001500000.
           05 FILLER PIC 9(9) VALUE 008900000.
           05 FILLER PIC 9(9) VALUE 015600000.
           05 FILLER PIC 9(9) VALUE 006700000.

       01  TABLA-SALDOS-R REDEFINES TABLA-SALDOS.
           05 SALDO OCCURS 10 TIMES PIC 9(9).

       01  INDICE                  PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIALIZAR
           PERFORM 200-GENERAR-REGISTROS
           PERFORM 300-FINALIZAR
           STOP RUN.

       100-INICIALIZAR.
           DISPLAY "========================================".
           DISPLAY "  GENERADOR DE ARCHIVO MAESTRO".
           DISPLAY "========================================".
           DISPLAY " ".

           OPEN OUTPUT ARCHIVO-MAESTRO

           IF FS-MAESTRO NOT = '00'
               DISPLAY "ERROR ABRIENDO ARCHIVO MAESTRO: " FS-MAESTRO
               STOP RUN
           END-IF

           DISPLAY "Archivo MAESTRO.DAT abierto correctamente".

       200-GENERAR-REGISTROS.
           PERFORM VARYING INDICE FROM 1 BY 1
                   UNTIL INDICE > 10

               MOVE COD-CLI(INDICE) TO CODIGO-MAESTRO
               MOVE NOM-CLI(INDICE) TO NOMBRE-MAESTRO
      * Dividir por 100 para ajustar los decimales
               DIVIDE SALDO(INDICE) BY 100
                   GIVING SALDO-MAESTRO

               WRITE REG-MAESTRO

               IF FS-MAESTRO = '00'
                   ADD 1 TO CONTADOR
                   DISPLAY "Registro creado: " CODIGO-MAESTRO
                           " - " NOMBRE-MAESTRO
                           " Saldo: " SALDO-MAESTRO
               ELSE
                   DISPLAY "ERROR ESCRIBIENDO REGISTRO: " FS-MAESTRO
               END-IF
           END-PERFORM.

       300-FINALIZAR.
           CLOSE ARCHIVO-MAESTRO

           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "  PROCESO FINALIZADO".
           DISPLAY "========================================".
           DISPLAY "Total registros creados: " CONTADOR.
           DISPLAY "Archivo generado: MAESTRO.DAT".
           DISPLAY " ".
