       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP003.
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
           
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CEP
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
       01  DESCUF PIC X(20).
       01  DESCTIPO1 PIC X(30).
       01  DESCTIPO2 PIC X(30).
       01  OPC PIC X(3).
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

       01  TABTIPOTELX.
           03 FILLER PIC X(30) VALUE "F FIXO".
           03 FILLER PIC X(30) VALUE "C CELULAR".
           03 FILLER PIC X(30) VALUE "E EMPRESA".
       01  TABTIPOTEL REDEFINES TABTIPOTELX.
           03 TBTIPOTEL PIC X(30) OCCURS 3 TIMES.
       01  TXTTIPOTEL.
           03 TXTTIPOTEL1 PIC X(01) VALUE SPACES.
           03 TXTTIPOTEL2 PIC X(29) VALUE SPACES.


       SCREEN SECTION.
       
       01  TELAFORN.
           05  LINE 01  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  " FORNECEDORES".
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
               USING  CODFORN.
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
               LINE 15  COLUMN 07  PIC 99999.999
               USING  CEPFORN.
           05  TNUMLOG
               LINE 15  COLUMN 28  PIC 9(05)
               USING  NUMEROLOG.
           05  TBAIRRO
               LINE 15  COLUMN 55  PIC X(13)
               USING  BAIRRO.
           05  TCOMP
               LINE 18  COLUMN 15  PIC X(12)
               USING  COMPLEMENTO.
           05  TCIDADE
               LINE 18  COLUMN 40  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 20  COLUMN 06  PIC X(02)
               USING  UF.

       01  TELATIPOS.
           05  LINE 07  COLUMN 41 
               VALUE  "           F - FISICA  J-JURIDICA".

       

       PROCEDURE DIVISION.
      *
       FN-ABRE2.
           OPEN I-O CADFORN
           IF ERRO NOT = "00"  
              IF ERRO = "30"
                 OPEN OUTPUT CADFORN
                 CLOSE CADFORN
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
                 MOVE "ERRO NA ABERTURA DO ARQUIVO FORN " TO MENS1
                 MOVE ERRO TO MENS2
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM
           ELSE
                 NEXT SENTENCE.

       FN-INICIO.
           MOVE ZEROS TO CODFORN CNPJCPF DDD1 NUMERO1 DDD2 NUMERO2
           CEPFORN NUMEROLOG.
           DISPLAY TELAFORN.

       FN-COD.
           ACCEPT TCODCLI.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-READ-FORN.
           READ CADFORN
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE " FORN JA CADASTRADO " TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-OPCOES.
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
           MOVE 0 TO INDICE.
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

       FN-DDD1.
           ACCEPT TDDD1.

       FN-TELEFONE1.
           ACCEPT TNUMERO1.  
           
       FN-TIPOTEL1.
           ACCEPT TTIPO1.
           MOVE 1 TO INDICE.
       FN-TIPOTEL1A.
           MOVE TBTIPOTEL(INDICE) TO TXTTIPOTEL
           IF TXTTIPOTEL1 NOT = TIPO1
               ADD 1 TO INDICE
               IF INDICE < 4
                   GO TO FN-TIPOTEL1A
               ELSE
                   MOVE "TIPO TELEFONE INCORRETO" TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-TIPOTEL1
               ELSE
                   MOVE TXTTIPOTEL2 TO DESCTIPO1
                   DISPLAY DESCTIPO1.

       FN-DDD2.
           ACCEPT TDDD2.
           
       FN-TELEFONE2.
           ACCEPT TNUMERO2.
       FN-TIPOTEL2.
           MOVE 0 TO INDICE.
           ADD 1 TO INDICE.
           ACCEPT TTIPO2.
           ACCEPT ESC-KEY FROM ESCAPE KEY
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.
           
       FN-TIPOTEL2A.
           MOVE TBTIPOTEL(INDICE) TO TXTTIPOTEL
           IF TXTTIPOTEL1 NOT = TIPO2
               ADD 1 TO INDICE
               IF INDICE < 4
                   GO TO FN-TIPOTEL2A
               ELSE
                   MOVE " INCORRETO " TO MENS
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-TIPOTEL2
               ELSE
                   MOVE TXTTIPOTEL2 TO DESCTIPO2
                   DISPLAY DESCTIPO2.


       FN-CEP.
           ACCEPT TCEP.
       FN-CEP2.
           MOVE CEPFORN TO CEP
           READ CADCEP
           DISPLAY TELAFORN
           IF ERRO NOT = "00"
               IF ERRO = "23"
                   MOVE " CEP NAO CADASTRADO " TO MENS 
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-CEP
               ELSE
                   MOVE " ERRO NA LEITURA " TO MENS1
                   MOVE ERRO TO MENS2
                   PERFORM FN-MENS THRU FN-MENS-FIM
                   GO TO FN-FIM.

       FN-NUM.
           ACCEPT TNUMLOG.

       FN-COMP.
           ACCEPT TCOMP.
           IF ESC-KEY = 01
                 MOVE " SAINDO DO PROGRAMA " TO MENS
                 PERFORM FN-MENS THRU FN-MENS-FIM
                 GO TO FN-FIM.

       FN-REGISTRO.
           DISPLAY (23, 15) "GRAVAR? (S/N): ".
           ACCEPT (23, 30) OPC
           IF OPC = "S"
               WRITE REGFORN
               GO TO FN-COD
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
               GO TO FN-NOME
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
           CLOSE CADFORN.
           EXIT PROGRAM.