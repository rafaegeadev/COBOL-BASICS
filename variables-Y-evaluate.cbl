      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VARIABLES-EVALUATE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      *           LA PRIMERA VARIABLE FUNCIONA COMO ARRAY              *
      ******************************************************************

       01 USUARIO.
           02 ALUMNO.
               05 NOMBRE-ALUMNO              PIC X(8)    VALUE SPACES.
               05 APELLIDOS-ALUMNO.
                   10 APELLIDO1-ALUMNO       PIC X(10)   VALUE SPACES.
                   10 APELLIDO2-ALUMNO       PIC X(10)   VALUE SPACES.
               05 TELEFONOS-ALUMNO.
                   10 TELEFONO1-ALUMNO       PIC X(9)    VALUE SPACES.
                   10 TELEFONO2-ALUMNO       PIC X(9)    VALUE SPACES.
           02 PROFESOR.
               05 NOMBRE-PROFESOR            PIC X(8)    VALUE SPACES.
               05 APELLIDOS-PROFESOR.
                   10 APELLIDO1-PROFESOR     PIC X(10)   VALUE SPACES.
                   10 APELLIDO2-PROFESOR     PIC X(10)   VALUE SPACES.
               05 TELEFONOS-PROFESOR.
                   10 TELEFONO1-PROFESOR     PIC X(9)    VALUE SPACES.
                   10 TELEFONO2-PROFESOR     PIC X(9)    VALUE SPACES.


      ******************************************************************
      *        EL 77 SE UTILIZA PARA VARIABLES INDEPENDIENTES          *
      ******************************************************************

       77 OTRA-VARIABLE-INDEPENDIENTE        PIC 9(9)    VALUE ZEROES.

      ******************************************************************
      *            ESTA VARIABLE FUNCIONARÍA COMO UN SWITCH             *
      ******************************************************************

       01 EDAD                               PIC 9(3)    VALUE ZEROES.
           88 JOVEN                          VALUE 1     THRU 39.
           88 MADURO                         VALUE 40    THRU 65.
           88 ANCIANO                        VALUE 66    THRU 100.




       PROCEDURE DIVISION.
       COMPRUEBA-EDAD.
      *> SE SOLICITA LA EDAD DEL USUARIOS.
           DISPLAY "INTRODUCE TU EDAD:"
           ACCEPT EDAD.

      *> SE COMPRUEBA EN QUÉ RANGO ESTÁ LA EDAD.
      ******************************************************************
      *  CON WITH NO ADVANCING EVITAMOS SALTO DE LINEA EN EL DISPLAY   *
      ******************************************************************

           IF JOVEN THEN
               DISPLAY "ERES JOVEN, " WITH NO ADVANCING
           END-IF

           IF MADURO THEN
               DISPLAY "ERES MADURO, " WITH NO ADVANCING
           END-IF

           IF ANCIANO THEN
               DISPLAY "ERES ANCIANO, " WITH NO ADVANCING
           END-IF.

      ******************************************************************
      *    SI NO SE CUMPLE NINGÚNA DE LAS OPCIONES,                    *
      *    LA VARIABLE SIGUE FUNCIONANDO COMO VARIABLE INDEPENDIENTE   *
      ******************************************************************

            DISPLAY "TIENES " EDAD " AÑOS."

      ******************************************************************
      *                 MISMO CASO CON EVALUATE                        *
      ******************************************************************

      *> SE SOLICITA LA EDAD DEL USUARIOS.
       DISPLAY "INTRODUCE TU EDAD: "
       ACCEPT EDAD.

       *> SE COMPRUEBA EN QUÉ RANGO ESTÁ LA EDAD.
       EVALUATE EDAD

           WHEN 1 THRU 39
            DISPLAY "ERES JOVEN, TU EDAD ES " EDAD "."

           WHEN 40 THRU 65
            DISPLAY "ERES MADURO, TU EDAD ES " EDAD "."

           WHEN 66 THRU 100
            DISPLAY "ERES ANCIANO, TU EDAD ES " EDAD "."

           WHEN OTHER
            DISPLAY "EDAD INCORRECTA."

       END-EVALUATE.

      ******************************************************************
      *              MISMO CASO CON EVALUATE TRUE,                     *
      *              DE ESTE MODO SE PUEDE USAR EL NIVEL 88            *
      ******************************************************************

      *> SE SOLICITA LA EDAD DEL USUARIOS.
       DISPLAY "INTRODUCE TU EDAD: "
       ACCEPT EDAD.

      *> SE COMPRUEBA EN QUÉ RANGO ESTÁ LA EDAD.
       EVALUATE TRUE

           WHEN JOVEN
            DISPLAY "ERES JOVEN, TU EDAD ES " EDAD "."

           WHEN MADURO
            DISPLAY "ERES MADURO, TU EDAD ES " EDAD "."

           WHEN ANCIANO
            DISPLAY "ERES ANCIANO, TU EDAD ES " EDAD "."

           WHEN OTHER
            DISPLAY "EDAD INCORRECTA."

       END-EVALUATE.


            STOP RUN.
       END PROGRAM VARIABLES-EVALUATE.
