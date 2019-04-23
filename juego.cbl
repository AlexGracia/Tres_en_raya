
      *****************************************************************
      *			      juego				      *
      *	 -----------------------------------------------------------  *
      * Tres en raya.                                                 *
      *                                                               *
      *****************************************************************
      * Log de modificaciones:                                        *
      * --------------------------------------------------------------*
      * Marca|Fecha     |Usuario|Descripcion                          *
      * -----|----------|-------|------------------------------------ *
      *      |17/10/2018|Alex   |Creacion del programa.               *
      * --------------------------------------------------------------*
      *****************************************************************
      *****************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.                    JUEGO.
       AUTHOR.                        ALEX.
       INSTALLATION.                  https://github.com/AlexGracia.
       DATE-WRITTEN.                  17/10/2018.
       DATE-COMPILED.

      *****************************************************************
      *                                                               *
      *                      ENVIRONMENT DIVISION                     *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      *****************************************************************
      *                                                               *
      *                         DATA DIVISION                         *
      *                                                               *
      *****************************************************************

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

      *****************************************************************
      *                                                               *
      *                       VARIABLES WORKING                       *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Hora actual.                                               *
      *  · Número aleatorio.                                          *
      *  · Tablero.                                                   *
      *  · Mensaje final.                                             *
      *  · Booleanos.                                                 *
      *  · Literales.                                                 *
      *                                                               *
      *****************************************************************
      * · Hora actual.
       01  SQLFPN GLOBAL.
           02  SQLFPN-FILE-LEN PIC S9(4) COMP VALUE +9.
           02  SQLFPN-FILENAME PIC X(9) VALUE "juego.pco".

       01  SQLCTX GLOBAL PIC S9(9) COMP VALUE +38677.


       01  SQLEXD GLOBAL.
           02  SQL-SQLVSN   PIC S9(18) COMP VALUE +10.
           02  SQL-ARRSIZ   PIC S9(9) COMP VALUE +1.
           02  SQL-ITERS    PIC S9(9) COMP.
           02  SQL-OFFSET   PIC S9(9) COMP.
           02  SQL-SELERR   PIC S9(4) COMP.
           02  SQL-SQLETY   PIC S9(4) COMP.
           02  SQL-OCCURS   PIC S9(9) COMP.
           02  SQL-DUMMY    PIC S9(9) COMP.
           02  SQL-CUD      PIC S9(18) COMP.
           02  SQL-SQLEST   PIC S9(18) COMP.
           02  SQL-STMT     PIC S9(18) COMP.
           02  SQL-SQLADTP  PIC S9(18) COMP VALUE 0.
           02  SQL-SQLTDSP  PIC S9(18) COMP VALUE 0.
           02  SQL-SQPHSV   PIC S9(18) COMP.
           02  SQL-SQPHSL   PIC S9(18) COMP.
           02  SQL-SQPHSS   PIC S9(18) COMP.
           02  SQL-SQPIND   PIC S9(18) COMP.
           02  SQL-SQPINS   PIC S9(18) COMP.
           02  SQL-SQPARM   PIC S9(18) COMP.
           02  SQL-SQPARC   PIC S9(18) COMP.
           02  SQL-SQPADTO  PIC S9(18) COMP.
           02  SQL-SQPTDSO  PIC S9(18) COMP.
           02  SQL-SQHSTV   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQHSTL   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQHSTS   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQINDV   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQINDS   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQHARM   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQHARC   PIC S9(18) COMP OCCURS 1 TIMES.
           02  SQL-SQADTO   PIC S9(4) COMP OCCURS 1 TIMES.
           02  SQL-SQTDSO   PIC S9(4) COMP OCCURS 1 TIMES.


       01  SQL-RUNTIME-VARS.
           02  SQL-IAPXIT-SUCCESS  PIC S9(9) COMP VALUE    +0.
           02  SQL-IAPXIT-FAILURE  PIC S9(9) COMP VALUE +1403.
           02  SQL-IAPXIT-FATALERR PIC S9(9) COMP VALUE  +535.

       01  SQLCUD GLOBAL.
           02     FILLER PIC S9(4) COMP VALUE +10.
           02     FILLER PIC S9(4) COMP VALUE +4130.
           02     FILLER PIC S9(4) COMP VALUE +46.
           02     FILLER PIC S9(4) COMP VALUE +0.
           02     FILLER PIC S9(4) COMP VALUE +0.
       01 WS-TIEMPO-ACTUAL.
          05 TIEMPO               OCCURS 4 PIC 99 VALUE ZEROS.

      * · Número aleatorio.
       01 WS-NUM-ALEATORIO        PIC 9           VALUE ZEROS.

      * · Tablero.
       01 WS-TABLERO.
          05 WS-ASTERISCOS        PIC X(9)        VALUE ALL '*'.
          05 WS-TAB-INI.
             10 WS-FILA           OCCURS 3.
                15 WS-CELDA       OCCURS 9 PIC X.
             10 WS-INDICE         PIC 9 COMP-3    VALUE ZEROS.
             10 WS-CONT-CELDAS    PIC 9 COMP.
                88 TABLERO-LLENO                  VALUE 9.
                88 TABLERO-HUECO                  VALUE 0 THRU 8.
             10 WS-COORDENADAS.
                15 WS-X           OCCURS 9 PIC 9 COMP.
                15 WS-Y           OCCURS 9 PIC 9 COMP.
             10 WS-RAYAS.
                15 WS-RAYA-USUARIO OCCURS 8 PIC 99 COMP.

                15 WS-RAYA-MAQUINA OCCURS 8 PIC 99 COMP.

                15 WS-RAYA-TOTAL   OCCURS 8 PIC 99 COMP.

      * · Mensaje final.
       01 WS-MSG-FINAL            PIC X(15)       VALUE SPACES.

      * · Booleanos.
       01 WS-FIN-JUEGO            PIC 9 COMP.
          88 NO-FIN                               VALUE 0.
          88 FIN                                  VALUE 1.

       01 WS-CELDA-LLENA          PIC 9 COMP.
          88 NO-CELDA-LLENA                       VALUE 0.
          88 SI-CELDA-LLENA                       VALUE 1.

       01 WS-TURNO                PIC 9 COMP.
          88 USUARIO                              VALUE 0.
          88 MAQUINA                              VALUE 1.

      * · Literales.
       77 WS-MSG-CELDA-LLENA      PIC X(12)       VALUE 'Celda llena.'.
       77 WS-MSG-PERDEDOR         PIC X(15)    VALUE 'Has perdido. :O'.
       77 WS-MSG-GANADOR          PIC X(15)    VALUE 'Has ganado.  :)'.
       77 WS-MSG-SALIR            PIC X(15)    VALUE 'Has salido.  :('.
       *> Rayas.
       77 WS-1-LLENA              PIC 99 COMP  VALUE 6.
       77 WS-2-LLENA              PIC 99 COMP  VALUE 15.
       77 WS-3-LLENA              PIC 99 COMP  VALUE 24.
       77 WS-4-LLENA              PIC 99 COMP  VALUE 12.
       77 WS-5-LLENA              PIC 99 COMP  VALUE 15.
       77 WS-6-LLENA              PIC 99 COMP  VALUE 18.
       77 WS-7-LLENA              PIC 99 COMP  VALUE 15.
       77 WS-8-LLENA              PIC 99 COMP  VALUE 15.

      *****************************************************************
      *                                                               *
      *                      PROCEDURE DIVISION.                      *
      *                                                               *
      *****************************************************************
       PROCEDURE DIVISION.

      *****************************************************************
      *                                                               *
      *   0000-PROCESO-PRINCIPAL                                      *
      *                                                               *
      *****************************************************************
       0000-PROCESO-PRINCIPAL.

           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT.

           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT UNTIL FIN OR TABLERO-LLENO.

           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT.

       0000-PROCESO-PRINCIPAL-EXIT.
           STOP RUN.

      *****************************************************************
      *                                                               *
      *   1000-INICIO                                                 *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Inicialización de variables.                               *
      *                                                               *
      *****************************************************************
       1000-INICIO.
      * · Inicializar variables.
           MOVE ZEROS TO WS-TIEMPO-ACTUAL.
           INITIALIZE WS-TAB-INI
                      WS-FIN-JUEGO
                      WS-CELDA-LLENA
                      WS-TURNO.

      *   Coordenadas (x,y) (fila, columna).
           *> X (filas de la tabla).
           MOVE 1 TO WS-X(1) WS-X(2) WS-X(3).
           MOVE 2 TO WS-X(4) WS-X(5) WS-X(6).
           MOVE 3 TO WS-X(7) WS-X(8) WS-X(9).
           *> Y (columnas de la tabla).
           MOVE 3 TO WS-Y(1) WS-Y(4) WS-Y(7).
           MOVE 5 TO WS-Y(2) WS-Y(5) WS-Y(8).
           MOVE 7 TO WS-Y(3) WS-Y(6) WS-Y(9).

      *   Asteriscos laterales.
           PERFORM 3 TIMES
              ADD 1 TO WS-INDICE
              MOVE '*' TO WS-CELDA(WS-INDICE,1)
              MOVE '*' TO WS-CELDA(WS-INDICE,9)
           END-PERFORM.

      *   Mensaje final.
           MOVE WS-MSG-SALIR TO WS-MSG-FINAL.

      *   Valores de las rayas llenas.
           ADD 6  TO WS-RAYA-TOTAL(1).
           ADD 15 TO WS-RAYA-TOTAL(2).
           ADD 24 TO WS-RAYA-TOTAL(3).
           ADD 12 TO WS-RAYA-TOTAL(4).
           ADD 15 TO WS-RAYA-TOTAL(5).
           ADD 18 TO WS-RAYA-TOTAL(6).
           ADD 15 TO WS-RAYA-TOTAL(7).
           ADD 15 TO WS-RAYA-TOTAL(8).

       1000-INICIO-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   2000-PROCESO                                                *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Poner ficha usuario.                                       *
      *  · Pintar tablero.                                            *
      *  · Poner ficha máquina.                                       *
      *  · Pintar tablero.                                            *
      *                                                               *
      *****************************************************************
       2000-PROCESO.
       
      * · Poner ficha usuario.
           SET SI-CELDA-LLENA TO TRUE.
           PERFORM 2100-MOVER-USUARIO
             THRU 2100-MOVER-USUARIO-EXIT UNTIL NO-CELDA-LLENA.

      * · Pintar tablero.
           PERFORM 8000-PINTAR-TABLERO
              THRU 8000-PINTAR-TABLERO-EXIT.

           IF NO-FIN
      * · Poner ficha máquina.
              SET SI-CELDA-LLENA TO TRUE
              PERFORM 2200-MOVER-MAQUINA
                 THRU 2200-MOVER-MAQUINA-EXIT UNTIL NO-CELDA-LLENA
              DISPLAY '(Máquina)'
      * · Pintar tablero.
              PERFORM 8000-PINTAR-TABLERO
                 THRU 8000-PINTAR-TABLERO-EXIT
           END-IF.

       2000-PROCESO-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   2100-MOVER-USUARIO                                          *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Preguntar posición.                                        *
      *  · Comprobar celda y añadir ficha en el tablero.              *
      *                                                               *
      *****************************************************************
       2100-MOVER-USUARIO.
           SET NO-CELDA-LLENA TO TRUE.

      * · Preguntar posición.
           DISPLAY ' '.
           DISPLAY 'Mueve ficha (1-9, 0 salir): '.
           ACCEPT WS-INDICE.

      * · Comprobar celda y añadir ficha en el tablero.
           EVALUATE WS-INDICE
              WHEN 1 THRU 9
                 *> Celda vacía.
                 IF WS-CELDA(WS-X(WS-INDICE),WS-Y(WS-INDICE)) = SPACE
                    MOVE 'x' TO WS-CELDA(WS-X(WS-INDICE)
                                   ,WS-Y(WS-INDICE))
                    ADD 1 TO WS-CONT-CELDAS
                    SET USUARIO TO TRUE
                    *> Comprobar raya.
                    PERFORM 8100-COMPROBAR-RAYA
                       THRU 8100-COMPROBAR-RAYA-EXIT
                 *> Celda llena.
                 ELSE
                    SET SI-CELDA-LLENA TO TRUE
                 END-IF
              WHEN OTHER
                 SET FIN TO TRUE
           END-EVALUATE.

      *    Aviso de celda llena.
           IF SI-CELDA-LLENA
              DISPLAY WS-MSG-CELDA-LLENA
           END-IF.

       2100-MOVER-USUARIO-EXIT.
           EXIT.

      *****************************************************************
      *   2200-MOVER-MAQUINA.                                         *
      * ------------------------------------------------------------- *
      *                                                               *
      * Descripción:                                                  *
      * Generación de un número aleatorio                             *
      * , para que la máquina mueva ficha.                            *
      *                                                               *
      * Apartados:                                                    *
      * · Generación de número aleatorio.                             *
      * · Comprobar celda y añadir ficha en el tablero.               *
      *                                                               *
      *****************************************************************
       2200-MOVER-MAQUINA.
           SET NO-CELDA-LLENA TO TRUE.

      * · Generación de número aleatorio.
           ACCEPT WS-TIEMPO-ACTUAL FROM TIME.
           COMPUTE WS-INDICE = TIEMPO(2) + TIEMPO(3)
                                      * TIEMPO(4) - TIEMPO(1).

      * · Comprobar celda y añadir ficha en el tablero.
           EVALUATE WS-INDICE
              WHEN 1 THRU 9
                 *> Celda vacía.
                 IF WS-CELDA(WS-X(WS-INDICE),WS-Y(WS-INDICE)) = SPACE
                    MOVE 'o' TO WS-CELDA(WS-X(WS-INDICE)
                                   ,WS-Y(WS-INDICE))
                    ADD 1 TO WS-CONT-CELDAS
                    SET MAQUINA TO TRUE
                    *> Comprobar raya.
                    PERFORM 8100-COMPROBAR-RAYA
                       THRU 8100-COMPROBAR-RAYA-EXIT
                 *> Celda llena.
                 ELSE
                    SET SI-CELDA-LLENA TO TRUE
                 END-IF
              WHEN 0
                 ADD 1 TO TIEMPO(4)
                 SET SI-CELDA-LLENA TO TRUE
           END-EVALUATE.

       2200-MOVER-MAQUINA-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   3000-FIN                                                    *
      *                                                               *
      *****************************************************************
       3000-FIN.
           PERFORM 6000-ESTADISTICAS
              THRU 6000-ESTADISTICAS-EXIT.

       3000-FIN-EXIT.
           EXIT.

      *****************************************************************
      *   6000-ESTADISTICA (2 pasos).                                 *
      *****************************************************************
       6000-ESTADISTICAS.
           DISPLAY '*************************************************'.
           DISPLAY '**         Resumen Programa JUEGO               *'.
           DISPLAY '* --------------------------------------------- *'.
           DISPLAY '*                  ' WS-MSG-FINAL
           '              *'.
           DISPLAY '*                                               *'.
           DISPLAY '*************************************************'.

       6000-ESTADISTICAS-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   8000-PINTAR-TABLERO                                         *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Título.                                                    *
      *  · Tablero.                                                   *
      *                                                               *
      *****************************************************************
       8000-PINTAR-TABLERO.
      * · Título.
           DISPLAY 'TRES EN RAYA'.
           DISPLAY '============'.

      * · Tablero.
           DISPLAY WS-ASTERISCOS.
           DISPLAY WS-FILA(1).
           DISPLAY WS-FILA(2).
           DISPLAY WS-FILA(3).
           DISPLAY WS-ASTERISCOS.

           DISPLAY ' '.

       8000-PINTAR-TABLERO-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   8100-COMPROBAR-RAYA                                         *
      * ------------------------------------------------------------- *
      *                                                               *
      *  · Sumar valor de la celda al de las rayas posibles.          *
      *  · Comprobar raya.                                            *
      *                                                               *
      *****************************************************************
       8100-COMPROBAR-RAYA.
      * · Sumar valor de la celda al de las rayas posibles.
           EVALUATE WS-INDICE
              WHEN 1
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(1)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(4)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(7)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(1)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(4)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(7)
                 END-IF
              WHEN 2
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(1)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(5)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(1)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(5)
                 END-IF
              WHEN 3
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(1)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(6)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(8)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(1)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(6)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(8)
                 END-IF
              WHEN 4
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(2)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(4)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(2)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(4)
                 END-IF
              WHEN 5
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(2)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(5)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(7)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(8)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(2)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(5)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(7)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(8)
                 END-IF
              WHEN 6
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(2)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(6)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(2)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(6)
                 END-IF
              WHEN 7
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(3)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(4)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(8)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(3)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(4)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(8)
                 END-IF
              WHEN 8
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(3)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(5)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(3)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(5)
                 END-IF
              WHEN 9
                 IF USUARIO
                    ADD WS-INDICE TO WS-RAYA-USUARIO(3)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(6)
                    ADD WS-INDICE TO WS-RAYA-USUARIO(7)
                 ELSE IF MAQUINA
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(3)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(6)
                    ADD WS-INDICE TO WS-RAYA-MAQUINA(7)
                 END-IF
           END-EVALUATE.
           
      * · Comprobar raya.
           MOVE ZERO TO WS-INDICE
           PERFORM 8 TIMES
              ADD 1 TO WS-INDICE
              *> Gana usuario.
              IF WS-RAYA-USUARIO(WS-INDICE) = WS-RAYA-TOTAL(WS-INDICE)
                 MOVE WS-MSG-GANADOR TO WS-MSG-FINAL
                 SET FIN TO TRUE
              *> Gana máquina.
              ELSE IF WS-RAYA-MAQUINA(WS-INDICE)
                                            = WS-RAYA-TOTAL(WS-INDICE)
                 MOVE WS-MSG-PERDEDOR TO WS-MSG-FINAL
                 SET FIN TO TRUE
              END-IF
           END-PERFORM.

       8100-COMPROBAR-RAYA-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *   9999-ABORTAR                                                *
      *                                                               *
      *****************************************************************
       9999-ABORTAR.

           PERFORM 6000-ESTADISTICAS
              THRU 6000-ESTADISTICAS-EXIT.

           MOVE 12     TO RETURN-CODE.

       9999-ABORTAR-EXIT.
           STOP RUN.

      *****************************************************************
      *         *
      *  F I N  *
      *         *
      ***********
