       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP005.
       AUTHOR. MSZS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODCLIE
               FILE STATUS  IS ERRO.

           SELECT CADPEDIDO ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODPEDIDO
               FILE STATUS IS ERRO.

           SELECT CADPROD ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODPROD
               FILE STATUS IS ERRO.

           
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CEPCOD
               FILE STATUS IS ERRO.

           SELECT CADFORN ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODFORN
               FILE STATUS IS ERRO.



       DATA DIVISION.
       FILE SECTION.
       FD  CADCLI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
       01  REGCLI.
           03 CODCLIE PIC 9(06).
           03 NOME PIC X(30).

       FD  CADPEDIDO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPED.DAT".

       01  REGPEPDIDO.
           03 CODPEDIDO PIC 9(06).
           03 DATAPEDIDO PIC 9(07).
           03 QTDPEDIDO PIC 9(06).
           03 PRECOPEDIDO PIC 9(05).
           03 TOTALPEDIDO PIC 9(05).

       FD  CADFORN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFORN.DAT".

       01  REGFORN.
           03 CODFORN PIC 9(05).
           03 NOMEFORN PIC X(20).

       FD  CADCEP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP.DAT".

       01  REGCEP.
           03 CEPCOD PIC 9(08).
           03 CEPLOG PIC X(30).
           03 CEPBAIRRO PIC X(30).
           03 CEPCIDADE PIC X(20).

       FD  CADPROD.
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPROD.DAT".


       WORKING-STORAGE SECTION.
       01  ERRO PIC X(02) VALUE "00".

       PROCEDURE DIVISION.

       R0.
           OPEN I-O CADPEDIDO
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 OPEN OUTPUT CADPEDIDO
                 CLOSE CADPEDIDO
                 DISPLAY (23, 12) "*** ARQUIVO CADAMIGO FOI CRIADO **" 
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO =>" TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       R0A.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCEP NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.




       DISPLAY (12, 05) "DIGITE O CEP AI:".
       DISPLAY (12, 10) ACCEPT 