 IDENTIFICATION DIVISION.
 PROGRAM-ID. ABITANTI.
 AUTHOR. LUCIO
 DATE-WRITTEN. 19/10/2022
* QUESTO E' UN COMMENTO
*
 ENVIRONMENT DIVISION.  
* 
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.   
* INPUT FILE 

     SELECT PRIMOF   ASSIGN TO 'ABITANTI.TXT'
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL.
* OUTPUT FILE 

     
  
************************************************************
*                       DATA DIVISION                       
************************************************************
 DATA DIVISION.
 FILE SECTION.
 FD   PRIMOF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-PRIMOF.
 01 REC-PRIMOF  PIC X(30).
 
************************************************************
*                WORKING-STORAGE SECTION                     
************************************************************

 WORKING-STORAGE SECTION.   
 01   WS-ABITANTI.
      05 CITTA             PIC X(10) .
      05 NOME              PIC X(10).
      05 COGNOME           PIC X(10).
     

*                
    
 01 FINE-FILE PIC X VALUE SPACES. 
 01 WS-CITTA                PIC X(10) VALUE SPACES.  
 01 WS-COGNOME               PIC X(10) VALUE SPACES. 
 
************************************************************
*                PROCEDURE DIVISION                    
************************************************************
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
      PERFORM ELABORA THRU FINE-ELABORA UNTIL FINE-FILE = 'S'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
 
     OPEN INPUT PRIMOF.
       
     READ PRIMOF INTO WS-PRIMOF AT END 
          MOVE 'S' TO FINE-FILE
          DISPLAY 'FILE PRIMOF VUOTO'  
          NOT AT END
               MOVE CITTA          TO WS-CODCLI
               MOVE COGNOME        TO WS-COGNOME
     END-READ     . 
          
 FINE-APERTURA.   
      EXIT.
* RICORDATI DI CONTROLLARE COSA FARE PER IL PRIMO RECORD
 ELABORA.    
    
      IF CODCLI = WS-CODCLI
         PERFORM STESSA-CITTA  THRU FINE-STESSA-CITTA
      ELSE   
          PERFORM NUOVA-CITTA  THRU FINE-NUOVA-CITTA 
      END-IF.   
      
     
      READ PRIMOF INTO WS-PRIMOF AT END MOVE 'S' TO FINE-FILE.   
      
 FINE-ELABORA.    
      EXIT.
* 
 STESSO-CLIENTE.

 
 FINE-STESSO-CLIENTE.
      EXIT.
*
 NUOVA-CITTA.
     DISPLAY 'CITTA = '   WS-CITTA.
     DISPLAY 'COGNOME = ' WS-COGNOME.
     DISPLAY ' TOTALE = '  TOTALE.
     MOVE CITTA               TO WS-CITTA.
     MOVE COGNOME             TO WS-COGNOME.
     MOVE NOME                TO WS-NOME.
 
 FINE-NUOVA-CITTA.
      EXIT.
*
 CHIUSURA. 
     DISPLAY 'CODCLI = ' WS-CODCLI.
     DISPLAY 'COGNOME = ' WS-COGNOME.
     DISPLAY  'TOTALE = '  TOTALE. 
     
     CLOSE PRIMOF.
          
           
 FINE-CHIUSURA.    
      EXIT.