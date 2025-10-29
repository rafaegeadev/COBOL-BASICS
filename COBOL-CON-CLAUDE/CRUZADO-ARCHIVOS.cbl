       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUZADO-ARCHIVOS.
      *AUTHOR. RAFAEL.
      *****************************************************************
      * PROGRAMA: CRUZADO DE ARCHIVOS SECUENCIALES
      * PROPOSITO: CRUZA DOS ARCHIVOS ORDENADOS POR CODIGO
      *            Y GENERA UN REPORTE DE COINCIDENCIAS
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-MAESTRO
               ASSIGN TO "MAESTRO.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-MAESTRO.

           SELECT ARCHIVO-MOVIMIENTOS
               ASSIGN TO "MOVIMIENTOS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-MOVIMIENTOS.

           SELECT ARCHIVO-SALIDA
               ASSIGN TO "REPORTE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-MAESTRO.
       01  REG-MAESTRO.
           05 CODIGO-MAESTRO       PIC 9(5).
           05 NOMBRE-MAESTRO       PIC X(30).
           05 SALDO-MAESTRO        PIC 9(7)V99.

       FD  ARCHIVO-MOVIMIENTOS.
       01  REG-MOVIMIENTOS.
           05 CODIGO-MOVIMIENTO    PIC 9(5).
           05 TIPO-MOVIMIENTO      PIC X(01).
              88 ES-CARGO          VALUE 'C'.
              88 ES-ABONO          VALUE 'A'.
           05 IMPORTE-MOVIMIENTO   PIC 9(7)V99.

       FD  ARCHIVO-SALIDA.
       01  REG-SALIDA              PIC X(80).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05 FS-MAESTRO           PIC XX.
           05 FS-MOVIMIENTOS       PIC XX.
           05 FS-SALIDA            PIC XX.

       01  SWITCHES.
           05 SW-FIN-MAESTRO       PIC X VALUE 'N'.
              88 FIN-MAESTRO       VALUE 'S'.
           05 SW-FIN-MOVIMIENTOS   PIC X VALUE 'N'.
              88 FIN-MOVIMIENTOS   VALUE 'S'.

       01  CONTADORES.
           05 CONT-MAESTRO         PIC 9(5) VALUE ZEROS.
           05 CONT-MOVIMIENTOS     PIC 9(5) VALUE ZEROS.
           05 CONT-COINCIDENCIAS   PIC 9(5) VALUE ZEROS.
           05 CONT-SOLO-MAESTRO    PIC 9(5) VALUE ZEROS.
           05 CONT-SOLO-MOVIMIENTO PIC 9(5) VALUE ZEROS.

       01  LINEA-DETALLE.
           05 FILLER               PIC X(10) VALUE 'CODIGO: '.
           05 LD-CODIGO            PIC 9(5).
           05 FILLER               PIC X(03) VALUE ' | '.
           05 LD-NOMBRE            PIC X(30).
           05 FILLER               PIC X(03) VALUE ' | '.
           05 LD-SITUACION         PIC X(25).

       01  CLAVE-MAYOR             PIC 9(5) VALUE 99999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIALIZAR
           PERFORM 200-PROCESAR-ARCHIVOS
           PERFORM 300-FINALIZAR
           STOP RUN.

       100-INICIALIZAR.
           DISPLAY "INICIANDO CRUZADO DE ARCHIVOS..."

           OPEN INPUT  ARCHIVO-MAESTRO
                INPUT  ARCHIVO-MOVIMIENTOS
                OUTPUT ARCHIVO-SALIDA

           IF FS-MAESTRO NOT = '00'
               DISPLAY "ERROR ABRIENDO MAESTRO: " FS-MAESTRO
               STOP RUN
           END-IF

           IF FS-MOVIMIENTOS NOT = '00'
               DISPLAY "ERROR ABRIENDO MOVIMIENTOS: " FS-MOVIMIENTOS
               STOP RUN
           END-IF

           PERFORM 110-LEER-MAESTRO
           PERFORM 120-LEER-MOVIMIENTOS

           PERFORM 130-ESCRIBIR-ENCABEZADO.

       110-LEER-MAESTRO.
           READ ARCHIVO-MAESTRO
               AT END
                   MOVE 'S' TO SW-FIN-MAESTRO
                   MOVE CLAVE-MAYOR TO CODIGO-MAESTRO
               NOT AT END
                   ADD 1 TO CONT-MAESTRO
           END-READ.

       120-LEER-MOVIMIENTOS.
           READ ARCHIVO-MOVIMIENTOS
               AT END
                   MOVE 'S' TO SW-FIN-MOVIMIENTOS
                   MOVE CLAVE-MAYOR TO CODIGO-MOVIMIENTO
               NOT AT END
                   ADD 1 TO CONT-MOVIMIENTOS
           END-READ.

       130-ESCRIBIR-ENCABEZADO.
           MOVE "========================================" TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "    REPORTE DE CRUZADO DE ARCHIVOS    " TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "========================================" TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE SPACES TO REG-SALIDA
           WRITE REG-SALIDA.

       200-PROCESAR-ARCHIVOS.
      * LOGICA CLASICA DE CRUZADO: COMPARAR CLAVES
           PERFORM UNTIL FIN-MAESTRO AND FIN-MOVIMIENTOS
               EVALUATE TRUE
                   WHEN CODIGO-MAESTRO < CODIGO-MOVIMIENTO
                       PERFORM 210-SOLO-EN-MAESTRO
                       PERFORM 110-LEER-MAESTRO

                   WHEN CODIGO-MAESTRO = CODIGO-MOVIMIENTO
                       PERFORM 220-EN-AMBOS-ARCHIVOS
                       PERFORM 110-LEER-MAESTRO
                       PERFORM 120-LEER-MOVIMIENTOS

                   WHEN CODIGO-MAESTRO > CODIGO-MOVIMIENTO
                       PERFORM 230-SOLO-EN-MOVIMIENTOS
                       PERFORM 120-LEER-MOVIMIENTOS
               END-EVALUATE
           END-PERFORM.

       210-SOLO-EN-MAESTRO.
           ADD 1 TO CONT-SOLO-MAESTRO
           MOVE CODIGO-MAESTRO TO LD-CODIGO
           MOVE NOMBRE-MAESTRO TO LD-NOMBRE
           MOVE "SOLO EN MAESTRO" TO LD-SITUACION
           WRITE REG-SALIDA FROM LINEA-DETALLE.

       220-EN-AMBOS-ARCHIVOS.
           ADD 1 TO CONT-COINCIDENCIAS
           MOVE CODIGO-MAESTRO TO LD-CODIGO
           MOVE NOMBRE-MAESTRO TO LD-NOMBRE
           MOVE "*** COINCIDENCIA ***" TO LD-SITUACION
           WRITE REG-SALIDA FROM LINEA-DETALLE.

       230-SOLO-EN-MOVIMIENTOS.
           ADD 1 TO CONT-SOLO-MOVIMIENTO
           MOVE CODIGO-MOVIMIENTO TO LD-CODIGO
           MOVE "SIN NOMBRE" TO LD-NOMBRE
           MOVE "SOLO EN MOVIMIENTOS" TO LD-SITUACION
           WRITE REG-SALIDA FROM LINEA-DETALLE.

       300-FINALIZAR.
           MOVE SPACES TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "========================================" TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "           ESTADISTICAS                " TO REG-SALIDA
           WRITE REG-SALIDA
           MOVE "========================================" TO REG-SALIDA
           WRITE REG-SALIDA

           STRING "REGISTROS MAESTRO LEIDOS:     "
                  CONT-MAESTRO
                  DELIMITED BY SIZE INTO REG-SALIDA
           WRITE REG-SALIDA

           STRING "REGISTROS MOVIMIENTOS LEIDOS: "
                  CONT-MOVIMIENTOS
                  DELIMITED BY SIZE INTO REG-SALIDA
           WRITE REG-SALIDA

           STRING "COINCIDENCIAS:                "
                  CONT-COINCIDENCIAS
                  DELIMITED BY SIZE INTO REG-SALIDA
           WRITE REG-SALIDA

           STRING "SOLO EN MAESTRO:              "
                  CONT-SOLO-MAESTRO
                  DELIMITED BY SIZE INTO REG-SALIDA
           WRITE REG-SALIDA

           STRING "SOLO EN MOVIMIENTOS:          "
                  CONT-SOLO-MOVIMIENTO
                  DELIMITED BY SIZE INTO REG-SALIDA
           WRITE REG-SALIDA

           CLOSE ARCHIVO-MAESTRO
                 ARCHIVO-MOVIMIENTOS
                 ARCHIVO-SALIDA

           DISPLAY "PROCESO FINALIZADO CORRECTAMENTE"
           DISPLAY "COINCIDENCIAS: " CONT-COINCIDENCIAS
           DISPLAY "REPORTE GENERADO EN: REPORTE.TXT".
