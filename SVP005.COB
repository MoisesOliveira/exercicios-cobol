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

       FD  CADPROD
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPROD.DAT".

       01  REGPROD.
           03 NOME PIC X(40).
           03 CODPROD PIC 9(06).

       WORKING-STORAGE SECTION.
       01  ERRO PIC X(02) VALUE "00".
       01  MENS.
           03 MENS1 PIC X(50) VALUE SPACES.
           03 MENS2 PIC ZZZ.
       01  CONTADOR PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       FN-ABRE1.
           OPEN I-O CADPEDIDO
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 OPEN OUTPUT CADPEDIDO
                 CLOSE CADPEDIDO
                 MOVE " ARQUIVO SENDO CRIADO " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-ABRE1
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO PEDIDO =>" TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.
      *
       FN-ABRE2.
           OPEN INPUT CADCEP
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 MOVE "* ARQUIVO CADCEP NAO ENCONTRADO *" TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CEP " TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.

       FN-ABRE3.
           OPEN INPUT CADCLI
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 MOVE "* ARQUIVO CADCLI NAO ENCONTRADO *" TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CLIENTE " TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.

       FN-MENS.
           MOVE ZEROS TO CONTADOR.
       FN-MENS2.
           DISPLAY (23, 12) MENS.
       FN-MENS3.
           ADD 1 TO CONTADOR
           IF CONTADOR < 800
               GO TO FN-MENS3
           ELSE
               MOVE SPACES TO MENS
               DISPLAY (23, 12) MENS.
       FN-MENS-FIM.
           EXIT.

       FN-FIM.
           CLOSE CADCEP.
           CLOSE CADPEDIDO.
           CLOSE CADCLI.
           CLOSE CADFORN.
           CLOSE CADPROD.
           EXIT PROGRAM.