       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP004.
       AUTHOR. MSZS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFORN ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CODFORN
               FILE STATUS  IS ERRO.
           
           SELECT CADPROD ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODPROD
               FILE STATUS IS ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD  CADFORN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFORN.DAT".
       01  REGFORN.
           03 CODFORN PIC 9(06).
           03 NOME PIC X(30).
           03 TIPOPESSOA PIC X(1).
           03 CNPJCPF PIC 9(15).
           03 NUMEROLOG PIC 9(5).
           03 CEPFORN PIC 9(08).
           03 COMPLEMENTO PIC X(12).
           03 EMAIL PIC X(30).
           03 TELEFONE1.
               05 DDD1 PIC 9(02).
               05 NUMERO1 PIC 9(09).
               05 TIPO1 PIC X(01).
           03 TELEFONE2.
               05 DDD2 PIC 9(02).
               05 NUMERO2 PIC 9(09).
               05 TIPO2 PIC X(01).

       FD  CADPROD
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPROD.DAT".
       01  REGPROD.
           03 CODPROD PIC 9(06).
           03 DESCR PIC X(30).
           03 UNID PIC X(02).
           03 TIPO PIC 9(01).
           03 FORNCOD PIC 9(06).
           03 ULTPRECO PIC 9(09).
           03 PRECOVENDA PIC 9(09).


       WORKING-STORAGE SECTION.
       01  ERRO PIC X(02) VALUE "00".
       01  MENS.
           03 MENS1 PIC X(50) VALUE SPACES.
           03 MENS2 PIC ZZZ.
       01  CONTADOR PIC 9(03) VALUE ZERO.
       01  ESC-KEY PIC 9(02) VALUE ZEROS.
       01  OPC-KEY PIC X(02) VALUE SPACES.
       01  DESCTIPOPES PIC X(30).
       01  ESPACOS PIC X(80) VALUE SPACES.
       01  DESCUNIDADE PIC X(20).
       01  DESCTIPO PIC X(30).
       01  OPC PIC X(3).
       01  INDICE PIC 9(03).

       01  TABTIPO.
           03 TBTIPO PIC X(30) OCCURS 9 TIMES.

       01  TABUNIDX.
           03 FILLER PIC X(30) VALUE "ALIMENTO".
           03 FILLER PIC X(30) VALUE "SAUDE".
           03 FILLER PIC X(30) VALUE "HIGIENE".
           03 FILLER PIC X(30) VALUE "VESTUARIO".
           03 FILLER PIC X(30) VALUE "MANUTENCAO".
           03 FILLER PIC X(30) VALUE "COMESTIVEL".
           03 FILLER PIC X(30) VALUE "CONSTRUCAO".
           03 FILLER PIC X(30) VALUE "ANIMAL".
       01  TABUNID REDEFINES TABUNIDX.
           03 TBUNID PIC X(30) OCCURS 9 TIMES.
       01  TXTUNID.
           03 TXTUNID1 PIC X(01) VALUE SPACES.
           03 TXTUNID2 PIC X(29) VALUE SPACES.


       SCREEN SECTION.
       
       01  TELAPRODUTO.
           05  LINE 02  COLUMN 01 
               VALUE  "                           CADASTRO DE P".
           05  LINE 02  COLUMN 41 
               VALUE  "RODUTOS".
           05  LINE 05  COLUMN 01 
               VALUE  " CODIGO:".
           05  LINE 05  COLUMN 41 
               VALUE  "  UNIDADE:".
           05  LINE 08  COLUMN 01 
               VALUE  " DESCRICAO:".
           05  LINE 08  COLUMN 41 
               VALUE  "  TIPO:".
           05  LINE 11  COLUMN 01 
               VALUE  " CODIGO DO FORNECEDOR:".
           05  LINE 11  COLUMN 41 
               VALUE  " NOME DO FORNECEDOR:".
           05  LINE 14  COLUMN 01 
               VALUE  " ULTIMO PRECO DE COMPRA:".
           05  LINE 14  COLUMN 41 
               VALUE  "PRECO DE VENDA:".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM:".
           05  TCOD
               LINE 05  COLUMN 10  PIC 9(06)
               USING  CODPROD.
           05  TUNID
               LINE 05  COLUMN 52  PIC X(02)
               USING  UNID.
           05  TDESCUNID
               LINE 05  COLUMN 55  PIC X(24)
               USING  DESCUNIDADE.
           05  TDESC
               LINE 08  COLUMN 12  PIC X(30)
               USING  DESCR.
           05  TTIPO
               LINE 08  COLUMN 49  PIC 9(01)
               USING  TIPO.
           05  TDESCTIPO
               LINE 08  COLUMN 51  PIC X(28)
               USING  DESCTIPO.
           05  TCODFORN
               LINE 11  COLUMN 24  PIC 9(06)
               USING  FORNCOD.
           05  TNOMEFORN
               LINE 11  COLUMN 61  PIC X(18)
               USING  NOME.
           05  TULTPRECO
               LINE 14  COLUMN 26  PIC 999999.999
               USING  ULTPRECO.
           05  TPRECO
               LINE 14  COLUMN 57  PIC 999999.999
               USING  PRECOVENDA.


       PROCEDURE DIVISION.

       FN-ALOCA.
           MOVE "ALIMENTO" TO TBTIPO(1).
           MOVE "SAUDE" TO TBTIPO(2).
           MOVE "HIGIENE" TO TBTIPO(3).
           MOVE "VESTUARIO" TO TBTIPO(4).
           MOVE "MANUTENCAO" TO TBTIPO(5).


       FN-ABRE.
           OPEN I-O CADPROD
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 OPEN OUTPUT CADPROD
                 CLOSE CADPROD
                 MOVE " ARQUIVO SENDO CRIADO " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-ABRE
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO PEDIDO =>" TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.
       
       FN-ABRE2.
           OPEN INPUT CADFORN
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 MOVE "* ARQUIVO CADFORN NAO ENCONTRADO *" TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADFORN " TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.
       

       FN-CAMPOS.
           MOVE SPACES TO NOME DESCR UNID.
           MOVE ZEROS TO CODPROD TIPO FORNCOD ULTPRECO PRECOVENDA.

       FN-INICIO.
           DISPLAY TELAPRODUTO.

       FN-COD.
           ACCEPT TCOD.

       FN-DESC.
           ACCEPT TDESC.

       FN-FORN.
           ACCEPT TCODFORN.
       FN-FORN2.
           MOVE FORNCOD TO CODFORN.
           READ CADFORN.
           DISPLAY TNOMEFORN.

       FN-ULTPRECO.
           ACCEPT TULTPRECO.

       FN-UNID.
           ACCEPT TUNID.


       FN-TIPO.
           ACCEPT TTIPO.
       FN-TIPO2.
           MOVE TBTIPO(TIPO) TO DESCTIPO.
           DISPLAY TELAPRODUTO.

       FN-PRECOVENDA.
           ACCEPT TPRECO.
       
       

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
           CLOSE CADPROD.
           CLOSE CADFORN.
           EXIT PROGRAM.