       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADEND.
       AUTHOR. MSZS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADEND ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CEP
           FILE STATUS IS ERRO
           ALTERNATE RECORD KEY IS LOGRADOURO.


       DATA DIVISION.
       FILE SECTION.
       FD  CADEND
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP.DAT".

       01  REGEND.
           03 CEP PIC 9(8).
           03 LOGRADOURO PIC X(20).
           03 BAIRRO PIC X(20).
           03 UF PIC X(2).
           03 CEPREF PIC 9(8).
           03 TIPOPISO PIC 9(1).
           03 OBS PIC X(60).
           03 CIDADE PIC X(20).

       WORKING-STORAGE SECTION.
       01  ESTADO PIC X(20).
       01  ERRO PIC X(02) VALUE "00".
       01  TABESTADOS.
           03 TABESTADO PIC X(20) OCCURS 26 TIMES.
       01  TABTIPOS.
           03 TABTIPO PIC X(20) OCCURS 5 TIMES.
       01  TABUFX.
           03 FILLER PIC X(20) VALUE "SP SAO PAULO".
           03 FILLER PIC X(20) VALUE "AC ACRE".
           03 FILLER PIC X(20) VALUE "BA BAHIA".
           03 FILLER PIC X(20) VALUE "TO TOCANTINS".
       01  TABUF REDEFINES TABUFX.
           03 TBUF PIC X(20) OCCURS 6 TIMES.
       01  TXTUF.
           03 TXTUF1 PIC X(02) VALUE SPACES.
           03 TXTUF2 PIC X(18) VALUE SPACES.

       01  ESC-KEY PIC 9(02) VALUE ZEROS.
       01  MENS PIC X(100) VALUE SPACES.
       01  CONT PIC 9(5).
       01  INDICE PIC 9(02).
       01  TIPOEXTENSO PIC X(50) VALUE "TIPO".
       01  OPC PIC X(02) VALUE SPACES.
       01  ALT PIC 9(01) VALUE ZEROS.

       SCREEN SECTION.
       01  TELAEND.
           05  LINE 01  COLUMN 01
               VALUE  "                         CADASTRO DE END".
           05  LINE 01  COLUMN 41
               VALUE  "ERECO".
           05  LINE 02  COLUMN 01
               VALUE  " CEP:".
           05  LINE 04  COLUMN 01
               VALUE  " LOGRADOURO:".
           05  LINE 06  COLUMN 01
               VALUE  " BAIRRO:".
           05  LINE 08  COLUMN 01
               VALUE  " UF:".
           05  LINE 10  COLUMN 01
               VALUE  " CEP REFERENCIA:".
           05  LINE 12  COLUMN 01
               VALUE  " TIPO PISO:".
           05  LINE 14  COLUMN 01
               VALUE  " OBSERVACAO:".
           05  LINE 16  COLUMN 01
               VALUE  " CIDADE:".
           05  LINE 23  COLUMN 01
               VALUE  "MENSAGEM :".
           05  TCEP
               LINE 02  COLUMN 08  PIC 99999.999
               USING  CEP
               FOREGROUND-COLOR 09.
           05  TLOG
               LINE 04  COLUMN 14  PIC X(40)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 06  COLUMN 10  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TUF
               LINE 08  COLUMN 06  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TESTADO
               LINE 08  COLUMN 09  PIC X(20)
               USING  ESTADO.
           05  TCEPREF
               LINE 10  COLUMN 18  PIC 9(8)
               USING  CEPREF
               HIGHLIGHT.
           05  TTIPO
               LINE 12  COLUMN 13  PIC X(01)
               USING  TIPOPISO
               HIGHLIGHT.
           05  TTIPOEXTENSO
               LINE 12  COLUMN 15  PIC X(20)
               USING  TIPOEXTENSO.
           05  TOBS
               LINE 14  COLUMN 14  PIC X(40)
               USING  OBS.
           05  TCIDADE
               LINE 16  COLUMN 10  PIC X(20)
               USING  CIDADE.


       01  TELAUF.
           05  LINE 03  COLUMN 41
               VALUE  "                       ESTADOS".
           05  LINE 04  COLUMN 41
               VALUE  "                  -----------------".
           05  LINE 05  COLUMN 41
               VALUE  "                   SP - SAO PAULO".
           05  LINE 06  COLUMN 41
               VALUE  "                   AC - ACRE".
           05  LINE 07  COLUMN 41
               VALUE  "                   BA - BAHIA".
           05  LINE 08  COLUMN 41
               VALUE  "                   TO - TOCANTINS".



       PROCEDURE DIVISION.
       INICIO.
           MOVE "TIPO1" TO TABTIPO(1).
           MOVE "TIPO2" TO TABTIPO(2).


       ABRE-ARQ.
           OPEN I-O CADEND
           IF ERRO NOT = "00"
               IF ERRO = "30"
                   OPEN OUTPUT CADEND
                   CLOSE CADEND
                   GO TO ABRE-ARQ
                ELSE
                    IF ERRO = "95"
                        DISPLAY "ERRO"
           ELSE
               NEXT SENTENCE.

       PREPARA.
           MOVE ZEROS TO CEP CEPREF TIPOPISO.
           MOVE SPACES TO LOGRADOURO BAIRRO UF  OBS CIDADE.
           DISPLAY TELAEND.

       OP-CEP.
           ACCEPT TCEP.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
               CLOSE CADEND
               GO TO ROTINA-FIM.
           IF CEP = ZERO
               MOVE "CEP NAO PODE SER ZERO" TO MENS
               PERFORM  ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-CEP.

       LER-REGISTRO.
           READ CADEND
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE "ENDERECO JA CADASTRADO" TO MENS
                   PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
                   GO TO OP-CEP
               ELSE
                   MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENS
                   PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
                   GO TO ROTINA-FIM
           ELSE
               MOVE "CEP NAO CADASTRADO" TO MENS
               PERFORM ROTINA-MENS-FIM THRU ROTINA-MENS-FIM.


       OP-LOG.
           ACCEPT TLOG.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
               CLOSE CADEND
               GO TO OP-CEP.
           IF LOGRADOURO = SPACES
               MOVE "LOGRADOURO NAO PODE ESTAR VAZIO" TO MENS
               PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-LOG.

       OP-BAIRRO.
           ACCEPT TBAIRRO.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
               CLOSE CADEND
               GO TO OP-LOG.
           IF BAIRRO = SPACES
               MOVE "BAIRRO NAO PODE ESTAR VAZIO" TO MENS
               PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-BAIRRO.

       OP-UF.
           DISPLAY TELAUF.
           ACCEPT TUF.
       OP-UF2.
           MOVE 1 TO INDICE.
       OP-UF3.
           MOVE TBUF(INDICE) TO TXTUF
           IF TXTUF1 NOT = UF
               ADD 1 TO INDICE
               IF INDICE < 7
                   GO TO OP-UF3
               ELSE
                   MOVE "UF INCORRETO" TO MENS
                   PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
                   GO TO OP-UF
               ELSE
                   MOVE TXTUF2 TO ESTADO
                   DISPLAY TELAEND.

       OP-CEPREF.
           ACCEPT TCEPREF.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
               CLOSE CADEND
               GO TO ROTINA-FIM.
           IF CEPREF = ZERO
               MOVE "CEP REF. NAO PODE SER ZERO" TO MENS
               PERFORM  ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-CEPREF.
       OP-TIPO1.
           ACCEPT TTIPO.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
               GO TO ROTINA-FIM.

           IF TIPOPISO = 0 OR > 5
               MOVE " DIGITE UM NUMERO VALIDO" TO MENS
               PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-TIPO1.

       OP-TIPO2.
           MOVE TABTIPO(TIPOPISO) TO TIPOEXTENSO
           DISPLAY TELAEND.

       OP-OBS.
           ACCEPT TOBS.
           ACCEPT ESC-KEY FROM ESCAPE KEY

           IF ESC-KEY = 01
               GO TO ROTINA-FIM.

           IF OBS = SPACES
               MOVE " OBS NAO PODE ESTAR VAZIO" TO MENS
               PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-OBS.

       OP-CIDADE.
           ACCEPT TCIDADE.
           ACCEPT ESC-KEY FROM ESCAPE KEY

           IF ESC-KEY = 01
               GO TO ROTINA-FIM.

           IF CIDADE = SPACES
               MOVE " OBS NAO PODE ESTAR VAZIO" TO MENS
               PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
               GO TO OP-CIDADE.

       REGISTRO.
           DISPLAY (23, 10) "GRAVAR? (S/N): ".
           ACCEPT (23, 23) OPC
           IF OPC = "S"
               WRITE REGEND
               GO TO OP-CEP
           ELSE
               IF OPC NOT = "N"
                   MOVE "DIGITE S OU N" TO MENS
                   PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
                   GO TO REGISTRO
                ELSE
                    MOVE "REGISTRO NAO GRAVADO" TO MENS
                   PERFORM ROTINA-MENS THRU ROTINA-MENS-FIM
                   GO TO ROTINA-FIM.


       ROTINA-MENS.
           MOVE ZEROS TO CONT.
       ROTINA-MENS1.
           DISPLAY (23, 12) MENS.
       ROTINA-MENS2.
           ADD 1 TO CONT.
           IF CONT < 700
               GO TO ROTINA-MENS2
           ELSE
               MOVE SPACES TO MENS
               DISPLAY (23, 12) MENS.
       ROTINA-MENS-FIM.
           EXIT.
       FIM-ROTINA-TEMPO.

       ROTINA-FIM.
           CLOSE CADEND.
           STOP RUN.