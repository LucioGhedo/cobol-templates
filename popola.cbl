 IDENTIFICATION DIVISION.
 PROGRAM-ID. POPOLA.
 AUTHOR. FILIPPO
 DATE-WRITTEN. 13/3/2017
* QUESTO E' UN COMMENTO
*
 ENVIRONMENT DIVISION.  
* 
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.   
* INPUT FILE 

     SELECT PRIMOF   ASSIGN TO 'ANAGRA.TXT'
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL.
* OUTPUT FILE 


*
  
************************************************************
*                       DATA DIVISION                       
************************************************************
 DATA DIVISION.
 FILE SECTION.
 FD   PRIMOF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-PRIMOF.
 01 REC-PRIMOF  PIC X(35).

************************************************************
*                WORKING-STORAGE SECTION                     
************************************************************

 WORKING-STORAGE SECTION.   
 01   WS-PRIMOF.
      05 CODICE         PIC 99.
      05 NOME           PIC X(10).
      05 COGNOME        PIC X(10). 
      05 ANNI           PIC 999.
      

 01   TABELLA.
      05 ELEMENTO OCCURS 6 TIMES INDEXED BY INDICE. 
         10 TAB-COD     PIC 99. 
         10 TAB-NOME    PIC X(10).
         10 TAB-COGNOME PIC X(10).
         10 TAB-ANNI    PIC 999.
 01   CONT              PIC 99 VALUE 0.
*                
    
 01 FINE-FILE        PIC X VALUE SPACES. 
 01 FINE-PGM         PIC X VALUE SPACES. 
 01 CODCLI           PIC 99 VALUE ZERO.
************************************************************
*                PROCEDURE DIVISION                    
************************************************************
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
      PERFORM ELABORA  THRU FINE-ELABORA UNTIL FINE-FILE = 'S'.
      PERFORM RICERCA THRU FINE-RICERCA UNTIL FINE-PGM = 'S'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
     SET INDICE TO 1.
     OPEN INPUT PRIMOF.
     
     READ PRIMOF INTO WS-PRIMOF AT END 
          MOVE 'S' TO FINE-FILE
          DISPLAY 'FILE PRIMOF VUOTO'. 
          
 FINE-APERTURA.   
      EXIT.
* 
 ELABORA.    
    
*     MOVE WS-PRIMOF TO WS-OUTF.
     
     ADD 1 TO CONT.
     
     MOVE WS-PRIMOF TO ELEMENTO(CONT).
     DISPLAY 'RIGA ' CONT ' ' ELEMENTO(CONT).
     
     READ PRIMOF INTO WS-PRIMOF AT END MOVE 'S' TO FINE-FILE.   
      
 FINE-ELABORA.
      EXIT.
* 
 RICERCA. 
       SET INDICE TO 1.
       DISPLAY 'DIGITA CODICE CLEINTE'
      ACCEPT CODCLI
      SEARCH ELEMENTO
      AT END  DISPLAY 'CODICE NON TROVATO'
      WHEN CODCLI = TAB-COD(INDICE)
            DISPLAY 'COGNOME ' TAB-COGNOME (INDICE)
            DISPLAY 'NOME ' TAB-NOME (INDICE)
            DISPLAY 'ETA ' TAB-ANNI  (INDICE)
      END-SEARCH.  
      
      DISPLAY 'VUOI FINIRE ?'
      ACCEPT FINE-PGM.
 FINE-RICERCA.
      EXIT.
 
 
 CHIUSURA.
 
     CLOSE PRIMOF.

 FINE-CHIUSURA.    
     
     EXIT.