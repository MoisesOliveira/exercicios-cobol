       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP002.
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
           
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CEPCOD
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

       FD  CADCEP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEP.DAT".
       01  REGCEP.
           03 CEPCOD PIC 9(08).
           03 CEPLOG PIC X(30).
           03 CEPBAIRRO PIC X(30).
           03 CEPCIDADE PIC X(20).
           03 CEPUF PIC X(02).

       WORKING-STORAGE SECTION.
       01  ERRO PIC X(02) VALUE "00".
       01  MENS.
           03 MENS1 PIC X(50) VALUE SPACES.
           03 MENS2 PIC ZZZ.
       01  CONTADOR PIC 9(03) VALUE ZERO.
       01  ESC-KEY PIC 9(02) VALUE ZEROS.
       01  DESCTIPOPES PIC X(30).
       01  DESCUF PIC X(20).
       01  DESCTIPO1 PIC X(30).
       01  DESCTIPO2 PIC X(30).
       01  INDICE PIC 9(03).
       01  TABTIPOPESX.
           03 FILLER PIC X(30) VALUE "F FISICA".
           03 FILLER PIC X(30) VALUE "JJURIDICA".
           03 FILLER PIC X(30) VALUE "G GOV".
       01  TABTIPOPES REDEFINES TABTIPOPESX.
           03 TBTIPOSPES PIC X(30) OCCURS 3 TIMES.
       01  TXTTIPOPES.
           03 TXTTIPOPES1 PIC X(01) VALUE SPACES.
           03 TXTTIPOPES2 PIC X(29) VALUE SPACES.

       SCREEN SECTION.
       
       01  TELACLIENTE.
           05  LINE 01  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  " CLIENTES".
           05  LINE 03  COLUMN 01 
               VALUE  " CODIGO:                               C".
           05  LINE 03  COLUMN 41 
               VALUE  "PF\CNPJ:".
           05  LINE 05  COLUMN 01 
               VALUE  " NOME:                                 E".
           05  LINE 05  COLUMN 41 
               VALUE  "MAIL:".
           05  LINE 07  COLUMN 01 
               VALUE  " TIPO PESSOA:".
           05  LINE 10  COLUMN 01 
               VALUE  " TELEFONE 1: (  )                  TIPO:".
           05  LINE 12  COLUMN 01 
               VALUE  " TELEFONE 2: (  )                  TIPO:".
           05  LINE 14  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 14  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 01 
               VALUE  " CEP:              NUMERO:".
           05  LINE 15  COLUMN 41 
               VALUE  "      BAIRRO:".
           05  LINE 18  COLUMN 01 
               VALUE  " COMPLEMENTO:                  CIDADE:".
           05  LINE 20  COLUMN 01 
               VALUE  " UF:".
           05  LINE 21  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCODCLI
               LINE 03  COLUMN 10  PIC 9(06)
               USING  CODCLIE.
           05  TCPFCNPJ
               LINE 03  COLUMN 50  PIC 9(15)
               USING  CNPJCPF.
           05  TNOME
               LINE 05  COLUMN 08  PIC X(30)
               USING  NOME.
           05  TEMAIL
               LINE 05  COLUMN 47  PIC X(30)
               USING  EMAIL.
           05  TTIPOPES
               LINE 07  COLUMN 15  PIC X(01)
               USING  TIPOPESSOA.
           05  TDESCTIPO
               LINE 07  COLUMN 18  PIC X(30)
               USING  DESCTIPOPES.
           05  TDDD1
               LINE 10  COLUMN 15  PIC 9(02)
               USING  DDD1.
           05  TNUMERO1
               LINE 10  COLUMN 18  PIC 9(09)
               USING  NUMERO1.
           05  TTIPO1
               LINE 10  COLUMN 43  PIC X(01)
               USING  TIPO1.
           05  TDESCTIPO1
               LINE 10  COLUMN 45  PIC X(30)
               USING  DESCTIPO1.
           05  TDDD2
               LINE 12  COLUMN 15  PIC 9(02)
               USING  DDD2.
           05  TNUMERO2
               LINE 12  COLUMN 18  PIC 9(09)
               USING  NUMERO2.
           05  TTIPO2
               LINE 12  COLUMN 43  PIC X(01)
               USING  TIPO2.
           05  TDESCTIPO2
               LINE 12  COLUMN 45  PIC X(30)
               USING  DESCTIPO2.
           05  TCEP
               LINE 15  COLUMN 07  PIC 9(08)
               USING  CEPCLIENTE.
           05  TNUMLOG
               LINE 15  COLUMN 28  PIC 9(05)
               USING  NUMEROLOG.
           05  TBAIRRO
               LINE 15  COLUMN 55  PIC X(13)
               USING  CEPBAIRRO.
           05  TCOMP
               LINE 18  COLUMN 15  PIC X(12)
               USING  COMPLEMENTO.
           05  TCIDADE
               LINE 18  COLUMN 40  PIC X(20)
               USING  CEPCIDADE.
           05  TUF
               LINE 20  COLUMN 06  PIC X(02)
               USING  CEPUF.

       01  TELATIPOS.
           05  LINE 07  COLUMN 41 
               VALUE  "           F - FISICA  J-JURIDICA".

       

       PROCEDURE DIVISION.
      *
       FN-ABRE2.
           OPEN I-O CADCLI
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 OPEN OUTPUT CADCLI
                 CLOSE CADCLI
                 MOVE " ARQUIVO SENDO CRIADO " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-ABRE2
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO PEDIDO =>" TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.

       FN-ABRE3.
           OPEN INPUT CADCEP
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

       FN-INICIO.
           MOVE ZEROS TO CODCLIE CNPJCPF DDD1 NUMERO1 DDD2 NUMERO2
           CEPCLIENTE NUMEROLOG.
           DISPLAY TELACLIENTE.

       FN-COD.
           ACCEPT TCODCLI.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-READ-CLIENTE.
           READ CADCLI
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE " CLIENTE JA CADASTRADO " TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-COD.
       FN-NOME.
           ACCEPT TNOME.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-TIPOPES.
           DISPLAY TELATIPOS.
           ACCEPT TTIPOPES.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.
       FN-TIPOPES2.
           MOVE 1 TO INDICE.
       FN-TIPOPES3.
           MOVE TBTIPOSPES(INDICE) TO TXTTIPOPES
           IF TXTTIPOPES1 NOT = TIPOPESSOA
               ADD 1 TO INDICE
               IF INDICE < 3
                   GO TO FN-TIPOPES3
               ELSE
                   MOVE "TIPO PESSOA INCORRETO" TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-TIPOPES
               ELSE
                   MOVE TXTTIPOPES2 TO DESCTIPOPES
                   DISPLAY DESCTIPOPES.
       FN-CPF.
           ACCEPT TCPFCNPJ.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-EMAIL.
           ACCEPT TEMAIL
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-TELEFONE1.
           ACCEPT TDDD1.
           ACCEPT TNUMERO1.  


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
           CLOSE CADCLI.
           EXIT PROGRAM.