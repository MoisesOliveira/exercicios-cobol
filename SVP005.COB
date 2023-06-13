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
               RECORD KEY IS CEP
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
           03 TIPOPESSOA PIC X(1).
           03 CNPJCPF PIC 9(15).
           03 NUMEROLOG PIC 9(5).
           03 CEPCLIENTE PIC 9(08).
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

       FD  CADPEDIDO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPED.DAT".

       01  REGPEPDIDO.
           03 CLIENTEPEDIDO PIC 9(06).
           03 CODPEDIDO PIC 9(06).
           03 FORNPEDIDO PIC 9(06).
           03 DATAPEDIDO PIC 9(11).
           03 CODPRODUTO PIC 9(06).
           03 QUANT PIC 9(04).
           03 TOTALVENDA PIC 9(09).
           03 CEPPEDIDO PIC 9(08).

       FD  CADFORN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFORN.DAT".
       01  REGFORN.
           03 CODFORN PIC 9(06).
           03 NOMEFORN PIC X(30).
           03 TIPOPESSOA PIC X(1).
           03 CNPJCPF PIC 9(15).
           03 NUMERO PIC 9(5).
           03 CEPFORN PIC 9(08).
           03 COMPLEMENTOFORN PIC X(12).
           03 EMAIL PIC X(30).
           03 TELEFONE1.
               05 DDD1 PIC 9(02).
               05 NUMERO1 PIC 9(09).
               05 TIPO1 PIC X(01).
           03 TELEFONE2.
               05 DDD2 PIC 9(02).
               05 NUMERO2 PIC 9(09).
               05 TIPO2 PIC X(01).

       FD  CADCEP
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
       01  OPC PIC X(03) VALUE SPACES.
       01  OPC-KEY PIC X(03) VALUE SPACES.

       SCREEN SECTION.
       
       
       01  TELAPEDIDO.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PEDIDOS".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO CLIENTE:".
           05  LINE 05  COLUMN 01 
               VALUE  " NUMERO PEDIDO:".
           05  LINE 07  COLUMN 01 
               VALUE  " DATA DO PEDIDO:".
           05  LINE 09  COLUMN 01 
               VALUE  " CODIGO PRODUTO:".
           05  LINE 11  COLUMN 01 
               VALUE  " CODIGO FORNECEDOR:".
           05  LINE 13  COLUMN 01 
               VALUE  " QUANTIDADE:".
           05  LINE 15  COLUMN 01 
               VALUE  " PRECO VENDA:".
           05  LINE 17  COLUMN 01 
               VALUE  " TOTAL VENDA:".
           05  LINE 19  COLUMN 01 
               VALUE  " CEP ENTREGA:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCODCLIE
               LINE 04  COLUMN 18  PIC 9(06)
               USING  CLIENTEPEDIDO.
           05  TNOMECLIE
               LINE 04  COLUMN 27  PIC X(30)
               USING  NOME.
           05  TCODPEDIDO
               LINE 05  COLUMN 17  PIC 9(06)
               USING  CODPEDIDO.
           05  TDATAPEDIDO
               LINE 07  COLUMN 18  PIC 99.99.9999
               USING  DATAPEDIDO.
           05  TCODPROD
               LINE 09  COLUMN 18  PIC 9(06)
               USING  CODPRODUTO.
           05  TPRODESC
               LINE 09  COLUMN 27  PIC X(30)
               USING  DESCR.
           05  TCODFORN
               LINE 11  COLUMN 21  PIC 9(06)
               USING  FORNPEDIDO.
           05  TFORN
               LINE 11  COLUMN 29  PIC X(30)
               USING  NOMEFORN.
           05  TQUANT
               LINE 13  COLUMN 14  PIC 9(04)
               USING  QUANT.
           05  TPRECOVENDA
               LINE 15  COLUMN 15  PIC 999999.999
               USING  PRECOVENDA.
           05  TTOTAL
               LINE 17  COLUMN 15  PIC 999999.999
               USING  TOTALVENDA.
           05  TCEP
               LINE 19  COLUMN 15  PIC 9(08)
               USING  CEPPEDIDO.
           05  TLOG
               LINE 19  COLUMN 26  PIC X(20)
               USING  LOGRADOURO.
           05  TNUM
               LINE 19  COLUMN 49  PIC 9(05)
               USING  NUMEROLOG.
           05  TBAIRRO
               LINE 19  COLUMN 56  PIC X(20)
               USING  BAIRRO.
           05  TCOMP
               LINE 21  COLUMN 02  PIC X(30)
               USING  COMPLEMENTO.
           05  TCIDADE
               LINE 21  COLUMN 34  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 21  COLUMN 56  PIC X(02)
               USING  UF.




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

       FN-ABRE4.
           OPEN INPUT CADFORN
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

       FN-ABRE5.
           OPEN INPUT CADPROD
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

       FN-PREPARA.
           MOVE ZEROS TO CLIENTEPEDIDO CODPEDIDO 
           CEPPEDIDO FORNPEDIDO DATAPEDIDO QUANT PRECOVENDA NUMEROLOG
           CODPRODUTO TOTALVENDA.

       FN-INICIO.
           DISPLAY TELAPEDIDO.

       FN-CODCLI.
           ACCEPT TCODCLIE.
       FN-CODCLI2.
           MOVE CLIENTEPEDIDO TO CODCLIE.
           READ CADCLI.
           DISPLAY TELAPEDIDO.

       FN-CODPED.
           ACCEPT TCODPEDIDO.

       FN-READ-FORN.
           READ CADPEDIDO
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE " FORN JA CADASTRADO " TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-OPCOES.
       
       FN-DATA.
           ACCEPT TDATAPEDIDO.

       FN-CODPROD.
           ACCEPT TCODPROD.
       FN-CODPROD2.
           MOVE CODPRODUTO TO CODPROD.
           READ CADPROD.
           DISPLAY TELAPEDIDO.

       FN-CODFORN.
           ACCEPT TCODFORN.
       FN-CODFORN2.
           MOVE FORNPEDIDO TO CODFORN.
           READ CADFORN.
           DISPLAY TELAPEDIDO.

       FN-QUANT.
           ACCEPT TQUANT.

       FN-TOTAL.
           MULTIPLY PRECOVENDA BY QUANT GIVING TOTALVENDA.
           DISPLAY TELAPEDIDO.

       FN-CEP.
           ACCEPT TCEP.

       FN-REGISTRO.
           DISPLAY (23, 15) "GRAVAR? (S/N): ".
           ACCEPT (23, 30) OPC
           IF OPC = "S"
               WRITE REGPEPDIDO
               GO TO FN-CODCLI
           ELSE
               IF OPC NOT = "N"
                   MOVE "DIGITE S OU N" TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-REGISTRO
                ELSE
                    MOVE "REGISTRO NAO GRAVADO" TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-FIM.

       FN-EXCLUIR.
           DISPLAY (23, 12) "EXCLUIR?".
           ACCEPT (23, 30) OPC-KEY
           IF OPC-KEY = "N" OR "n"
               MOVE " REGISTRO NAO EXCLUIDO " TO MENS
               PERFORM FN-MENS THRU FN-MENS-FIM
               GO TO FN-INICIO.
           IF OPC-KEY = "S" OR "s"
               DELETE CADFORN RECORD
               MOVE " REGISTRO EXCLUIDO COM SUCESSO" TO MENS
               PERFORM FN-MENS THRU FN-MENS-FIM.
       
       FN-OPCOES.
           MOVE "N = NOVO A = ALTERAR E = EXCLUIR" TO MENS
           DISPLAY (23, 12) MENS
           ACCEPT (23, 60) OPC-KEY
           IF OPC-KEY NOT = "N" AND OPC-KEY = "A" AND OPC-KEY = "E"
               GO TO FN-OPCOES.
           MOVE SPACES TO MENS
           DISPLAY (23, 12) MENS
           IF OPC-KEY = "N"
               GO TO FN-INICIO
               MOVE SPACES TO MENS
               DISPLAY (23, 12) MENS
           ELSE IF OPC-KEY = "A"
               GO TO FN-CODCLI
               MOVE SPACES TO MENS
               DISPLAY (23, 12) MENS

           ELSE IF OPC-KEY = "E"
               GO TO FN-EXCLUIR
               MOVE SPACES TO MENS
               DISPLAY (23, 12) MENS.


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